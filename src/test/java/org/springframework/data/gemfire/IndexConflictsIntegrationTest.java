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

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;

import java.util.Properties;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexExistsException;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.PropertyPlaceholderConfigurer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Import;

/**
 * The IndexConflictsIntegrationTest class...
 *
 * @author John Blum
 * @see org.junit.Test
 * @link https://jira.spring.io/browse/SGF-432
 * @since 1.6.3
 */
public class IndexConflictsIntegrationTest {

	protected void assertIndex(Index index, String expectedName, String expectedExpression, String expectedFromClause,
			IndexType expectedType) {

		assertThat(index, is(notNullValue()));
		assertThat(index.getName(), is(equalTo(expectedName)));
		assertThat(index.getIndexedExpression(), is(equalTo(expectedExpression)));
		assertThat(index.getFromClause(), is(equalTo(expectedFromClause)));
		assertThat(IndexType.valueOf(index.getType()), is(equalTo(expectedType)));
	}

	protected boolean close(ConfigurableApplicationContext applicationContext) {
		if (applicationContext != null) {
			applicationContext.close();
			return !(applicationContext.isActive() || applicationContext.isRunning());
		}

		return true;
	}

	@Before
	public void setup() {
		System.getProperties().remove("gemfire.cache.region.index.override");

		assertThat(System.getProperties().containsKey("gemfire.cache.region.index.override"), is(false));
	}

	@Test(expected = BeanCreationException.class)
	public void indexDefinitionConflictThrowsException() {
		ConfigurableApplicationContext applicationContext = null;

		try {
			applicationContext = new AnnotationConfigApplicationContext(
				IndexDefinitionConflictGemFireConfiguration.class);
		}
		catch (BeanCreationException expected) {
			assertThat(expected.getMessage(), containsString("Error creating bean with name 'customerIdentityIndex'"
				+ " defined in org.springframework.data.gemfire.IndexConflictsIntegrationTest$IndexDefinitionConflictGemFireConfiguration:"
				+ " Invocation of init method failed"));
			assertThat(expected.getCause(), is(instanceOf(GemfireIndexException.class)));
			assertThat(expected.getCause().getMessage(),
				containsString("An Index with a different name having the same definition"
					+ " as this Index (customerIdentityIndex) already exists"));
			assertThat(expected.getCause().getCause(), is(instanceOf(IndexExistsException.class)));
			assertThat(expected.getCause().getCause().getMessage(), is(equalTo("Similar Index Exists")));

			throw expected;
		}
		finally {
			assertThat(close(applicationContext), is(true));
		}
	}

	@Test
	public void indexNameConflictOverridesExistingIndex() {
		ConfigurableApplicationContext applicationContext = null;

		try {
			applicationContext = new AnnotationConfigApplicationContext(IndexNameConflictGemFireConfiguration.class);

			assertThat(applicationContext.getBeansOfType(Index.class).size(), is(equalTo(2)));

			Cache gemfireCache = applicationContext.getBean("gemfireCache", Cache.class);

			assertThat(gemfireCache.getQueryService().getIndexes().size(), is(equalTo(1)));

			Index customerLastNameIndex = applicationContext.getBean("customerLastNameIndex", Index.class);

			assertIndex(customerLastNameIndex, IndexNameConflictGemFireConfiguration.INDEX_NAME,
				"lastName", "/Customers", IndexType.HASH);

			Index customerFirstNameIndex = applicationContext.getBean("customerFirstNameIndex", Index.class);

			assertIndex(customerFirstNameIndex, IndexNameConflictGemFireConfiguration.INDEX_NAME,
				"firstName", "/Customers", IndexType.FUNCTIONAL);

			assertThat(customerFirstNameIndex, is(not(sameInstance(customerLastNameIndex))));
			assertThat(gemfireCache.getQueryService().getIndexes().iterator().next(),
				is(sameInstance(customerFirstNameIndex)));
		}
		finally {
			assertThat(close(applicationContext), is(true));
		}
	}

	@Test
	public void indexNameConflictReturnsExistingIndex() {
		ConfigurableApplicationContext applicationContext = null;

		try {
			System.setProperty("gemfire.cache.region.index.override", Boolean.FALSE.toString());

			assertThat(System.getProperty("gemfire.cache.region.index.override", "true"),
				is(equalTo(Boolean.FALSE.toString())));

			applicationContext = new AnnotationConfigApplicationContext(IndexNameConflictGemFireConfiguration.class);

			assertThat(applicationContext.getBeansOfType(Index.class).size(), is(equalTo(2)));

			Cache gemfireCache = applicationContext.getBean("gemfireCache", Cache.class);

			assertThat(gemfireCache.getQueryService().getIndexes().size(), is(equalTo(1)));

			Index customerLastNameIndex = applicationContext.getBean("customerLastNameIndex", Index.class);

			assertIndex(customerLastNameIndex, IndexNameConflictGemFireConfiguration.INDEX_NAME,
				"lastName", "/Customers", IndexType.HASH);

			Index customerFirstNameIndex = applicationContext.getBean("customerFirstNameIndex", Index.class);

			assertIndex(customerFirstNameIndex, IndexNameConflictGemFireConfiguration.INDEX_NAME,
				"lastName", "/Customers", IndexType.HASH);

			assertThat(customerFirstNameIndex, is(sameInstance(customerLastNameIndex)));
			assertThat(gemfireCache.getQueryService().getIndexes().iterator().next(),
				is(sameInstance(customerLastNameIndex)));
		}
		finally {
			System.getProperties().remove("gemfire.cache.region.index.override");

			if (applicationContext != null) {
				applicationContext.close();
			}
		}
	}

	@Configuration
	@SuppressWarnings("unused")
	public static class BaseGemFireConfiguration {

		@Bean
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
			cacheFactoryBean.setProperties(gemfireProperties());
			cacheFactoryBean.setUseBeanFactoryLocator(false);
			return cacheFactoryBean;
		}

		@Bean(name = "Customers")
		public ReplicatedRegionFactoryBean customersRegion(Cache gemfireCache) {
			ReplicatedRegionFactoryBean customersRegionFactory = new ReplicatedRegionFactoryBean();
			customersRegionFactory.setCache(gemfireCache);
			customersRegionFactory.setName("Customers");
			customersRegionFactory.setPersistent(false);
			return customersRegionFactory;
		}
	}

	@Configuration
	@Import(BaseGemFireConfiguration.class)
	@SuppressWarnings("unused")
	public static class IndexDefinitionConflictGemFireConfiguration {

		@Bean
		public IndexFactoryBean customerIdIndex(Cache gemfireCache) {
			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();
			indexFactoryBean.setCache(gemfireCache);
			indexFactoryBean.setExpression("id");
			indexFactoryBean.setFrom("/Customers");
			indexFactoryBean.setType(IndexType.PRIMARY_KEY);
			return indexFactoryBean;
		}

		@Bean
		@DependsOn("customerIdIndex")
		public IndexFactoryBean customerIdentityIndex(Cache gemfireCache) {
			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();
			indexFactoryBean.setCache(gemfireCache);
			indexFactoryBean.setExpression("id");
			indexFactoryBean.setFrom("/Customers");
			indexFactoryBean.setType(IndexType.PRIMARY_KEY);
			return indexFactoryBean;
		}
	}

	@Configuration
	@Import(BaseGemFireConfiguration.class)
	@SuppressWarnings("unused")
	public static class IndexNameConflictGemFireConfiguration {

		protected static final String INDEX_NAME = "CustomerNameIdx";

		@Bean
		public PropertyPlaceholderConfigurer propertyPlaceholderConfigurer() {
			return new PropertyPlaceholderConfigurer();
		}

		@Bean
		public IndexFactoryBean customerLastNameIndex(Cache gemfireCache,
				@Value("${gemfire.cache.region.index.override:true}") boolean override) {

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(gemfireCache);
			indexFactoryBean.setExpression("lastName");
			indexFactoryBean.setFrom("/Customers");
			indexFactoryBean.setName(INDEX_NAME);
			indexFactoryBean.setOverride(override);
			indexFactoryBean.setType(IndexType.HASH);

			return indexFactoryBean;
		}

		@Bean
		@DependsOn("customerLastNameIndex")
		public IndexFactoryBean customerFirstNameIndex(Cache gemfireCache,
				@Value("${gemfire.cache.region.index.override:true}") boolean override) {

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(gemfireCache);
			indexFactoryBean.setExpression("firstName");
			indexFactoryBean.setFrom("/Customers");
			indexFactoryBean.setName(INDEX_NAME);
			indexFactoryBean.setOverride(override);
			indexFactoryBean.setType(IndexType.FUNCTIONAL);

			return indexFactoryBean;
		}
	}

}
