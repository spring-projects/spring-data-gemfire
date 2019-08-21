/*
 * Copyright 2017-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Optional;
import java.util.Set;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneService;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.data.gemfire.IndexFactoryBean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.test.entities.CollocatedPartitionRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.NonEntity;
import org.springframework.data.gemfire.mapping.annotation.ClientRegion;
import org.springframework.data.gemfire.mapping.annotation.LocalRegion;
import org.springframework.data.gemfire.mapping.annotation.ReplicateRegion;
import org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean;
import org.springframework.data.gemfire.test.GemfireTestBeanPostProcessor;

/**
 * Integration tests for {@link IndexConfigurer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
 * @see org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class IndexConfigurerIntegrationTests {

	private static ConfigurableApplicationContext applicationContext;

	@BeforeClass
	public static void setup() {

		applicationContext = newApplicationContext(TestConfiguration.class);

		assertThat(applicationContext).isNotNull();
		assertThat(applicationContext.containsBean("CustomersFirstNameFunctionalIdx")).isTrue();
		assertThat(applicationContext.containsBean("CustomersIdKeyIdx")).isTrue();
		assertThat(applicationContext.containsBean("GenericRegionEntityIdKeyIdx")).isTrue();
		assertThat(applicationContext.containsBean("LastNameIdx")).isTrue();
		assertThat(applicationContext.containsBean("luceneIndex")).isTrue();
		assertThat(applicationContext.containsBean("oqlIndex")).isTrue();
		assertThat(applicationContext.containsBean("TitleLuceneIdx")).isTrue();
	}

	@AfterClass
	public static void tearDown() {
		Optional.ofNullable(applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	/* (non-Javadoc) */
	private static ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	/* (non-Javadoc) */
	private void assertIndexConfigurerInvocations(TestIndexConfigurer indexConfigurer, String... indexBeanNames) {
		assertThat(indexConfigurer).isNotNull();
		assertThat(indexConfigurer).contains(indexBeanNames);
		assertThat(indexConfigurer).hasSize(indexBeanNames.length);
	}

	@Test
	public void indexConfigurerOneCalledSuccessfully() {

		assertIndexConfigurerInvocations(
			applicationContext.getBean("testIndexConfigurerOne", TestIndexConfigurer.class),
			"CustomersFirstNameFunctionalIdx", "CustomersIdKeyIdx", "GenericRegionEntityIdKeyIdx",
			"LastNameIdx", "TitleLuceneIdx");
	}

	@Test
	public void indexConfigurerTwoCalledSuccessfully() {

		assertIndexConfigurerInvocations(
			applicationContext.getBean("testIndexConfigurerTwo", TestIndexConfigurer.class),
			"CustomersFirstNameFunctionalIdx", "CustomersIdKeyIdx", "GenericRegionEntityIdKeyIdx",
			"LastNameIdx", "TitleLuceneIdx");
	}

	@PeerCacheApplication
	@EnableIndexing
	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = {
			@ComponentScan.Filter(type = FilterType.ANNOTATION,
				classes = { ClientRegion.class, LocalRegion.class, ReplicateRegion.class }),
			@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE,
				classes = CollocatedPartitionRegionEntity.class)
		}
	)
	static class TestConfiguration {

		@Bean("LoyalCustomers")
		public LocalRegionFactoryBean<Object, Object> localRegion(GemFireCache gemfireCache) {

			LocalRegionFactoryBean<Object, Object> localRegion = new LocalRegionFactoryBean<>();

			localRegion.setCache(gemfireCache);
			localRegion.setClose(false);
			localRegion.setPersistent(false);

			return localRegion;
		}

		@Bean
		GemfireTestBeanPostProcessor testBeanPostProcessor() {
			return new GemfireTestBeanPostProcessor();
		}

		@Bean
		BeanPostProcessor indexFactoryBeanReplacingBeanPostProcessor() {

			return new BeanPostProcessor() {

				@Override
				public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

					if (bean instanceof LuceneIndexFactoryBean) {
						LuceneIndexFactoryBean luceneIndexFactoryBean = (LuceneIndexFactoryBean) bean;
						LuceneService mockLuceneService = mock(LuceneService.class);
						LuceneIndex mockLuceneIndex = mock(LuceneIndex.class);

						luceneIndexFactoryBean.setLuceneIndex(mockLuceneIndex);
						luceneIndexFactoryBean.setLuceneService(mockLuceneService);
						luceneIndexFactoryBean.setRegionPath("/Test");
					}

					return bean;
				}
			};
		}
		@Bean
		LuceneIndexFactoryBean luceneIndex() {
			return new LuceneIndexFactoryBean();
		}

		@Bean
		IndexFactoryBean oqlIndex(GemFireCache cache) {

			IndexFactoryBean indexFactory = new IndexFactoryBean();

			indexFactory.setCache(cache);
			indexFactory.setExpression("*");
			indexFactory.setFrom("/Test");

			return indexFactory;
		}

		@Bean
		TestIndexConfigurer testIndexConfigurerOne() {
			return new TestIndexConfigurer();
		}

		@Bean
		TestIndexConfigurer testIndexConfigurerTwo() {
			return new TestIndexConfigurer();
		}

		@Bean
		String nonRelevantBean() {
			return "test";
		}
	}

	private static class TestIndexConfigurer implements IndexConfigurer, Iterable<String> {

		private final Set<String> beanNames = new HashSet<>();

		@Override
		public void configure(String beanName, IndexFactoryBean bean) {
			this.beanNames.add(beanName);
		}

		@Override
		public void configure(String beanName, LuceneIndexFactoryBean bean) {
			this.beanNames.add(beanName);
		}

		@Override
		public Iterator<String> iterator() {
			return Collections.unmodifiableSet(this.beanNames).iterator();
		}
	}
}
