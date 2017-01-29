/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.client;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.util.Properties;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The SpringJavaConfiguredClientCacheIntegrationTest class is a test suite of test cases testing
 * the proper configuration of a GemFire ClientCache instance using Spring Java-based configuration meta-data.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.client.ClientCache
 * @link https://jira.spring.io/browse/SGF-441
 * @since 1.8.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = SpringJavaConfiguredClientCacheIntegrationTest.GemFireConfiguration.class)
@SuppressWarnings("unused")
public class SpringJavaConfiguredClientCacheIntegrationTest {

	@Resource(name = "&clientCache")
	private ClientCacheFactoryBean clientCacheFactoryBean;

	@Autowired
	private Properties gemfireProperties;

	@Test
	public void clientCacheFactoryBeanConfiguration() {
		assertThat(clientCacheFactoryBean, is(notNullValue()));
		assertThat(clientCacheFactoryBean.getBeanName(), is(equalTo("clientCache")));
		assertThat(clientCacheFactoryBean.getProperties(), is(equalTo(gemfireProperties)));
	}

	@Configuration
	public static class GemFireConfiguration {

		@Bean
		public Properties gemfireProperties() {
			Properties gemfireProperties = new Properties();
			gemfireProperties.setProperty("name", SpringJavaConfiguredClientCacheIntegrationTest.class.getSimpleName());
			gemfireProperties.setProperty("mcast-port", "0");
			gemfireProperties.setProperty("log-level", "warning");
			return gemfireProperties;
		}

		@Bean
		public ClientCacheFactoryBean clientCache() {
			ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();
			clientCacheFactoryBean.setUseBeanFactoryLocator(false);
			clientCacheFactoryBean.setProperties(gemfireProperties());
			return clientCacheFactoryBean;
		}
	}

}
