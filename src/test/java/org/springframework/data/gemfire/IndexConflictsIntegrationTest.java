/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexExistsException;
import org.apache.geode.cache.query.IndexNameConflictException;
import org.apache.geode.cache.query.QueryService;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Import;

/**
 * Integration tests with test cases testing the numerous conflicting {@link Index} configurations.
 *
 * An {@link IndexExistsException} is thrown when 2 or more {@link Index Indexes} share the same definition
 * but have different names.
 *
 * An {@link IndexNameConflictException} is thrown when 2 or more {@link Index Indexes} share the same name
 * but have potentially different definitions.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see <a href="https://jira.spring.io/browse/SGF-432">IndexFactoryBean traps IndexExistsException instead of IndexNameConflictException</a>
 * @see <a href="https://jira.spring.io/browse/SGF-637">Improve IndexFactoryBean's resilience and options for handling GemFire IndexExistsExceptions and IndexNameConflictExceptions</a>
 * @since 1.6.3
 */
public class IndexConflictsIntegrationTest {

	private static final AtomicBoolean IGNORE = new AtomicBoolean(false);
	private static final AtomicBoolean OVERRIDE = new AtomicBoolean(false);

	private ConfigurableApplicationContext applicationContext;

	private void assertIndex(Index index, String expectedName, String expectedExpression, String expectedFromClause,
			IndexType expectedType) {

		assertThat(index).isNotNull();
		assertThat(index.getName()).isEqualTo(expectedName);
		assertThat(index.getIndexedExpression()).isEqualTo(expectedExpression);
		assertThat(index.getFromClause()).isEqualTo(expectedFromClause);
		assertThat(IndexType.valueOf(index.getType())).isEqualTo(expectedType);
	}

	private void assertIndexCount(int count) {
		assertThat(getIndexCount()).isEqualTo(count);
	}

	private boolean close(ConfigurableApplicationContext applicationContext) {

		if (applicationContext != null) {
			applicationContext.close();
			return !(applicationContext.isActive() || applicationContext.isRunning());
		}

		return true;
	}

	private Index getIndex(String indexName) {

		for (Index index : nullSafeCollection(getQueryService().getIndexes())) {
			if (index.getName().equalsIgnoreCase(indexName)) {
				return index;
			}
		}

		return null;
	}

	private int getIndexCount() {
		return nullSafeCollection(getQueryService().getIndexes()).size();
	}

	private QueryService getQueryService() {
		return this.applicationContext.getBean("gemfireCache", GemFireCache.class).getQueryService();
	}

	private boolean hasIndex(String indexName) {
		return (getIndex(indexName) != null);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return new AnnotationConfigApplicationContext(annotatedClasses);
	}

	@Before
	public void setup() {
		assertThat(IGNORE.get()).isFalse();
		assertThat(OVERRIDE.get()).isFalse();
	}

	@After
	public void tearDown() {

		OVERRIDE.set(false);
		IGNORE.set(false);

		assertThat(close(this.applicationContext)).isTrue();
	}

	@Test
	public void indexDefinitionConflictIgnoresIndex() {

		assertThat(IGNORE.compareAndSet(false, true)).isTrue();

		this.applicationContext = newApplicationContext(IndexDefinitionConflictConfiguration.class);

		assertThat(this.applicationContext.containsBean("customerIdIndex")).isTrue();
		assertThat(this.applicationContext.containsBean("customerIdentifierIndex")).isTrue();
		assertIndexCount(1);
		assertThat(hasIndex("customerIdIndex")).isTrue();
		assertThat(hasIndex("customerIdentifierIndex")).isFalse();

		Index customersIdIndex = getIndex("customerIdIndex");

		assertIndex(customersIdIndex, "customerIdIndex",
			"id", "/Customers", IndexType.PRIMARY_KEY);
	}

	@Test
	public void indexDefinitionConflictOverridesIndex() {

		assertThat(OVERRIDE.compareAndSet(false, true)).isTrue();

		this.applicationContext = newApplicationContext(IndexDefinitionConflictConfiguration.class);

		assertThat(this.applicationContext.containsBean("customerIdIndex")).isTrue();
		assertThat(this.applicationContext.containsBean("customerIdentifierIndex")).isTrue();
		assertIndexCount(1);
		assertThat(hasIndex("customerIdIndex")).isFalse();
		assertThat(hasIndex("customerIdentifierIndex")).isTrue();

		Index customersIdentifierIndex = getIndex("customerIdentifierIndex");

		assertIndex(customersIdentifierIndex, "customerIdentifierIndex",
			"id", "/Customers", IndexType.PRIMARY_KEY);
	}

	@Test(expected = IndexExistsException.class)
	public void indexDefinitionConflictThrowsIndexExistsException() throws Throwable {
		try {
			this.applicationContext = newApplicationContext(IndexDefinitionConflictConfiguration.class);
		}
		catch (BeanCreationException expected) {

			assertThat(expected).hasMessageStartingWith("Error creating bean with name 'customerIdentifierIndex'");

			assertThat(expected).hasCauseInstanceOf(GemfireIndexException.class);

			String existingIndexDefinition = String.format(IndexFactoryBean.BASIC_INDEX_DEFINITION,
				"id", "/Customers", IndexType.PRIMARY_KEY);

			assertThat(expected.getCause()).hasMessageStartingWith(String.format(
				"An Index with a different name [customerIdIndex] having the same definition [%s] already exists",
					existingIndexDefinition));

			assertThat(expected.getCause()).hasCauseInstanceOf(IndexExistsException.class);

			assertThat(expected.getCause().getCause()).hasMessage("Similar Index Exists");

			assertThat(expected.getCause().getCause()).hasNoCause();

			throw expected.getCause().getCause();
		}
	}

	@Test
	public void indexNameConflictIgnoresIndex() {

		assertThat(IGNORE.compareAndSet(false, true)).isTrue();

		this.applicationContext = newApplicationContext(IndexNameConflictConfiguration.class);

		assertThat(this.applicationContext.containsBean("customerLastNameIndex")).isTrue();
		assertThat(this.applicationContext.containsBean("customerFirstNameIndex")).isTrue();
		assertIndexCount(1);
		assertThat(hasIndex(IndexNameConflictConfiguration.INDEX_NAME)).isTrue();

		Index customerLastNameIndex = getIndex(IndexNameConflictConfiguration.INDEX_NAME);

		assertIndex(customerLastNameIndex, IndexNameConflictConfiguration.INDEX_NAME,
			"lastName", "/Customers", IndexType.HASH);
	}

	@Test
	public void indexNameConflictOverridesIndex() {

		assertThat(OVERRIDE.compareAndSet(false, true)).isTrue();

		this.applicationContext = newApplicationContext(IndexNameConflictConfiguration.class);

		assertThat(this.applicationContext.getBeansOfType(Index.class)).hasSize(2);
		assertThat(this.applicationContext.containsBean("customerLastNameIndex")).isTrue();
		assertThat(this.applicationContext.containsBean("customerFirstNameIndex")).isTrue();
		assertIndexCount(1);
		assertThat(hasIndex(IndexNameConflictConfiguration.INDEX_NAME)).isTrue();

		Index customerFirstNameIndex = getIndex(IndexNameConflictConfiguration.INDEX_NAME);

		assertIndex(customerFirstNameIndex, IndexNameConflictConfiguration.INDEX_NAME,
			"firstName", "/Customers", IndexType.FUNCTIONAL);
	}

	@Test(expected = IndexNameConflictException.class)
	public void indexNameConflictThrowsIndexNameConflictException() throws Throwable {
		try {
			this.applicationContext = newApplicationContext(IndexNameConflictConfiguration.class);
		}
		catch (BeanCreationException expected) {

			assertThat(expected).hasMessageStartingWith("Error creating bean with name 'customerFirstNameIndex'");

			assertThat(expected).hasCauseInstanceOf(GemfireIndexException.class);

			assertThat(expected.getCause()).hasMessageStartingWith(String.format(
				"An Index with the same name [%s] having possibly a different definition already exists;",
					IndexNameConflictConfiguration.INDEX_NAME));

			assertThat(expected.getCause()).hasCauseInstanceOf(IndexNameConflictException.class);

			assertThat(expected.getCause().getCause()).hasMessage(String.format("Index named ' %s ' already exists.",
				IndexNameConflictConfiguration.INDEX_NAME));

			assertThat(expected.getCause().getCause()).hasNoCause();

			throw expected.getCause().getCause();
		}
	}

	@Configuration
	@SuppressWarnings("unused")
	public static class GemFireConfiguration {

		public Properties gemfireProperties() {

			Properties gemfireProperties = new Properties();

			gemfireProperties.setProperty("name", IndexConflictsIntegrationTest.class.getSimpleName());
			gemfireProperties.setProperty("mcast-port", "0");
			gemfireProperties.setProperty("log-level", "warning");

			return gemfireProperties;
		}

		@Bean
		public CacheFactoryBean gemfireCache() {

			CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

			cacheFactoryBean.setClose(true);
			cacheFactoryBean.setProperties(gemfireProperties());

			return cacheFactoryBean;
		}

		@Bean(name = "Customers")
		public ReplicatedRegionFactoryBean customersRegion(GemFireCache gemfireCache) {

			ReplicatedRegionFactoryBean customersRegionFactory = new ReplicatedRegionFactoryBean();

			customersRegionFactory.setCache(gemfireCache);
			customersRegionFactory.setClose(false);
			customersRegionFactory.setPersistent(false);

			return customersRegionFactory;
		}
	}

	@Configuration
	@Import(GemFireConfiguration.class)
	@SuppressWarnings("unused")
	public static class IndexDefinitionConflictConfiguration {

		@Bean
		public IndexFactoryBean customerIdIndex(GemFireCache gemfireCache) {

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(gemfireCache);
			indexFactoryBean.setExpression("id");
			indexFactoryBean.setFrom("/Customers");
			indexFactoryBean.setType(IndexType.PRIMARY_KEY);

			return indexFactoryBean;
		}

		@Bean
		@DependsOn("customerIdIndex")
		public IndexFactoryBean customerIdentifierIndex(GemFireCache gemfireCache) {

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(gemfireCache);
			indexFactoryBean.setExpression("id");
			indexFactoryBean.setIgnoreIfExists(IGNORE.get());
			indexFactoryBean.setFrom("/Customers");
			indexFactoryBean.setOverride(OVERRIDE.get());
			indexFactoryBean.setType(IndexType.PRIMARY_KEY);

			return indexFactoryBean;
		}
	}

	@Configuration
	@Import(GemFireConfiguration.class)
	@SuppressWarnings("unused")
	public static class IndexNameConflictConfiguration {

		protected static final String INDEX_NAME = "CustomerNameIdx";

		@Bean
		public IndexFactoryBean customerLastNameIndex(GemFireCache gemfireCache) {

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(gemfireCache);
			indexFactoryBean.setExpression("lastName");
			indexFactoryBean.setFrom("/Customers");
			indexFactoryBean.setName(INDEX_NAME);
			indexFactoryBean.setType(IndexType.HASH);

			return indexFactoryBean;
		}

		@Bean
		@DependsOn("customerLastNameIndex")
		public IndexFactoryBean customerFirstNameIndex(GemFireCache gemfireCache) {

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(gemfireCache);
			indexFactoryBean.setExpression("firstName");
			indexFactoryBean.setFrom("/Customers");
			indexFactoryBean.setIgnoreIfExists(IGNORE.get());
			indexFactoryBean.setName(INDEX_NAME);
			indexFactoryBean.setOverride(OVERRIDE.get());
			indexFactoryBean.setType(IndexType.FUNCTIONAL);

			return indexFactoryBean;
		}
	}
}
