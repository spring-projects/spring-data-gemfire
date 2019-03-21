/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.client;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Properties;

import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;

/**
 * The PoolsConfiguredWithLocatorsAndServersExpressionsIntegrationTests class is a test suite of test cases testing the use of
 * property placeholder values in the nested &lt;gfe:locator&gt; and &lt;gfe:server&gt; sub-elements
 * of the &lt;gfe:pool&gt; element as well as the <code>locators</code> and <code>servers</code> attributes.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see org.springframework.data.gemfire.config.PoolParser
 * @see <a href="https://jira.spring.io/browse/SGF-433">SGF-433</a>
 * @since 1.6.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class PoolsConfiguredWithLocatorsAndServersExpressionsIntegrationTests {

	private static ConnectionEndpointList anotherLocators = new ConnectionEndpointList();
	private static ConnectionEndpointList anotherServers = new ConnectionEndpointList();
	private static ConnectionEndpointList locators = new ConnectionEndpointList();
	private static ConnectionEndpointList servers = new ConnectionEndpointList();

	@Autowired
	@Qualifier("locatorPool")
	@SuppressWarnings("unused")
	private Pool locatorPool;

	@Autowired
	@Qualifier("serverPool")
	@SuppressWarnings("unused")
	private Pool serverPool;

	@Autowired
	@Qualifier("anotherLocatorPool")
	@SuppressWarnings("unused")
	private Pool anotherLocatorPool;

	@Autowired
	@Qualifier("anotherServerPool")
	@SuppressWarnings("unused")
	private Pool anotherServerPool;

	protected static ConnectionEndpoint newConnectionEndpoint(String host, int port) {
		return new ConnectionEndpoint(host, port);
	}

	protected void assertConnectionEndpoints(Iterable<ConnectionEndpoint> connectionEndpoints, String... expected) {
		assertThat(connectionEndpoints).isNotNull();

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : connectionEndpoints) {
			assertThat(connectionEndpoint.toString()).isEqualTo(expected[index++]);
		}

		assertThat(index).isEqualTo(expected.length);
	}

	@Test
	public void anotherLocatorPoolFactoryConfiguration() {
		String[] expected = { "localhost[10335]", "cardboardbox[10334]", "safetydepositbox[10336]", "pobox[10334]" };

		assertThat(anotherLocators.size()).isEqualTo(expected.length);
		assertConnectionEndpoints(anotherLocators, expected);
	}

	@Test
	public void anotherServerPoolFactoryConfiguration() {
		String[] expected = { "boombox[1234]", "jambox[40404]", "toolbox[8181]" };

		assertThat(anotherServers.size()).isEqualTo(expected.length);
		assertConnectionEndpoints(CollectionUtils.sort(anotherServers), expected);
	}

	@Test
	public void locatorPoolFactoryConfiguration() {
		String[] expected = { "backspace[10334]", "jambox[11235]", "mars[30303]", "pluto[20668]", "skullbox[12480]" };

		assertThat(locators.size()).isEqualTo(expected.length);
		assertConnectionEndpoints(CollectionUtils.sort(locators), expected);
	}

	@Test
	public void serverPoolFactoryConfiguration() {
		String[] expected = { "earth[4554]", "jupiter[40404]", "mars[5112]", "mercury[1234]", "neptune[42424]", "saturn[41414]",
			"uranis[0]", "venus[9876]" };

		assertThat(servers.size()).isEqualTo(expected.length);
		assertConnectionEndpoints(CollectionUtils.sort(servers), expected);
	}

	public static class TestBeanFactoryPostProcessor implements BeanFactoryPostProcessor {
		@Override
		public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
			BeanDefinition anotherLocatorPoolBeanDefinition = beanFactory.getBeanDefinition("anotherLocatorPool");
			anotherLocatorPoolBeanDefinition.setBeanClassName(AnotherLocatorPoolFactoryBean.class.getName());
			BeanDefinition anotherServerPoolBeanDefinition = beanFactory.getBeanDefinition("anotherServerPool");
			anotherServerPoolBeanDefinition.setBeanClassName(AnotherServerPoolFactoryBean.class.getName());
			BeanDefinition locatorPoolBeanDefinition = beanFactory.getBeanDefinition("locatorPool");
			locatorPoolBeanDefinition.setBeanClassName(LocatorPoolFactoryBean.class.getName());
			BeanDefinition serverPoolBeanDefinition = beanFactory.getBeanDefinition("serverPool");
			serverPoolBeanDefinition.setBeanClassName(ServerPoolFactoryBean.class.getName());
		}
	}

	public static class AnotherLocatorPoolFactoryBean extends TestPoolFactoryBean {
		@Override ConnectionEndpointList getLocatorList() {
			return anotherLocators;
		}
	}

	public static class AnotherServerPoolFactoryBean extends TestPoolFactoryBean {
		@Override ConnectionEndpointList getServerList() {
			return anotherServers;
		}
	}

	public static class LocatorPoolFactoryBean extends TestPoolFactoryBean {
		@Override ConnectionEndpointList getLocatorList() {
			return locators;
		}
	}

	public static class ServerPoolFactoryBean extends TestPoolFactoryBean {
		@Override ConnectionEndpointList getServerList() {
			return servers;
		}
	}

	@SuppressWarnings("unused")
	public static class SpELBoundBean {

		private final Properties clientProperties;

		public SpELBoundBean(Properties clientProperties) {
			Assert.notNull(clientProperties, "clientProperties must not be null");
			this.clientProperties = clientProperties;
		}

		public String locatorsHostsPorts() {
			return "safetydepositbox[10336], pobox";
		}

		public String serverTwoHost() {
			return clientProperties.getProperty("gemfire.cache.client.server.2.host");
		}

		public String serverTwoPort() {
			return clientProperties.getProperty("gemfire.cache.client.server.2.port");
		}
	}

	public static class TestPoolFactoryBean extends PoolFactoryBean {

		ConnectionEndpointList getLocatorList() {
			throw new UnsupportedOperationException("Not Implemented");
		}

		ConnectionEndpointList getServerList() {
			throw new UnsupportedOperationException("Not Implemented");
		}

		@Override
		protected PoolFactory createPoolFactory() {
			final PoolFactory mockPoolFactory = mock(PoolFactory.class);

			when(mockPoolFactory.addLocator(anyString(), anyInt())).thenAnswer(new Answer<PoolFactory>() {
				@Override
				public PoolFactory answer(InvocationOnMock invocation) throws Throwable {
					String host = invocation.getArgumentAt(0, String.class);
					int port = invocation.getArgumentAt(1, Integer.class);
					getLocatorList().add(newConnectionEndpoint(host, port));
					return mockPoolFactory;
				}
			});

			when(mockPoolFactory.addServer(anyString(), anyInt())).thenAnswer(new Answer<PoolFactory>() {
				@Override
				public PoolFactory answer(InvocationOnMock invocation) throws Throwable {
					String host = invocation.getArgumentAt(0, String.class);
					int port = invocation.getArgumentAt(1, Integer.class);
					getServerList().add(newConnectionEndpoint(host, port));
					return mockPoolFactory;
				}
			});

			return mockPoolFactory;
		}

		@Override
		boolean isDistributedSystemPresent() {
			return true;
		}
	}
}
