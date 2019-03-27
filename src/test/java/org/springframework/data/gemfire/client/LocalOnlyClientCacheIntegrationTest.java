/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.client;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.util.Properties;
import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;

/**
 * The LocalOnlyClientCacheIntegrationTest class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class LocalOnlyClientCacheIntegrationTest {

	@Resource(name = "Example")
	private Region<Long, String> example;

	@Before
	public void assertRegionDetails() {
		assertThat(example, is(notNullValue()));
		assertThat(example.getName(), is(equalTo("Example")));
		assertThat(example.getFullPath(), is(equalTo(String.format("%1$sExample", Region.SEPARATOR))));
		assertThat(example.getAttributes(), is(notNullValue()));
		assertThat(example.getAttributes().getDataPolicy(), is(equalTo(DataPolicy.NORMAL)));
		assertThat(example.getAttributes().getKeyConstraint(), is(equalTo(Long.class)));
		assertThat(example.getAttributes().getValueConstraint(), is(equalTo(String.class)));
	}

	@Test
	public void getAndPutsAreSuccessful() {
		assertThat(example.put(1l, "one"), is(nullValue()));
		assertThat(example.put(2l, "two"), is(nullValue()));
		assertThat(example.put(3l, "three"), is(nullValue()));
		assertThat(example.get(1l), is(equalTo("one")));
		assertThat(example.get(2l), is(equalTo("two")));
		assertThat(example.get(3l), is(equalTo("three")));
		assertThat(example.get(0l), is(nullValue()));
		assertThat(example.get(4l), is(nullValue()));
	}

	@Configuration
	static class GemFireClientCacheConfiguration {

		@Bean
		PropertySourcesPlaceholderConfigurer propertyPlaceholderConfigurer() {
			return new PropertySourcesPlaceholderConfigurer();
		}

		@Bean
		Properties gemfireProperties(@Value("${spring.gemfire.log.level:warning}") String logLevel) {
			Properties gemfireProperties = new Properties();

			gemfireProperties.setProperty("name", LocalOnlyClientCacheIntegrationTest.class.getSimpleName());
			gemfireProperties.setProperty("log-level", logLevel);

			return gemfireProperties;
		}

		@Bean
		ClientCacheFactoryBean gemfireCache(@Qualifier("gemfireProperties") Properties gemfireProperties) {
			ClientCacheFactoryBean gemfireCache = new ClientCacheFactoryBean();

			gemfireCache.setClose(true);
			gemfireCache.setProperties(gemfireProperties);
			gemfireCache.setUseBeanFactoryLocator(false);

			return gemfireCache;
		}

		@Bean(name = "Example")
		ClientRegionFactoryBean<Long, String> exampleRegion(GemFireCache gemfireCache,
				RegionAttributes<Long, String> exampleAttributes) {

			ClientRegionFactoryBean<Long, String> exampleRegion = new ClientRegionFactoryBean<Long, String>();

			exampleRegion.setCache(gemfireCache);
			exampleRegion.setAttributes(exampleAttributes);
			exampleRegion.setName("Example");
			exampleRegion.setPersistent(false);
			exampleRegion.setShortcut(ClientRegionShortcut.LOCAL);

			return exampleRegion;
		}

		@Bean
		@SuppressWarnings("unchecked")
		RegionAttributesFactoryBean exampleRegionAttributes() {
			RegionAttributesFactoryBean exampleRegionAttributes = new RegionAttributesFactoryBean();

			exampleRegionAttributes.setKeyConstraint(Long.class);
			exampleRegionAttributes.setValueConstraint(String.class);

			return exampleRegionAttributes;
		}
	}

}
