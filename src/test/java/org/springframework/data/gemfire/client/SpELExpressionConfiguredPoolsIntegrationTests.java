/*
 * Copyright 2010-2019 the original author or authors.
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
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.util.Collections;
import java.util.Optional;
import java.util.Properties;

import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.test.mock.GemFireMockObjectsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests that test the use of property placeholders in nested &lt;gfe:locator&gt; and &lt;gfe:server&gt;
 * elements of the SDG XML namespace &lt;gfe:pool&gt; element along with testing property placeholders in
 * the &lt;gfe:pool&gt; element <code>locators</code> and <code>servers</code> attributes.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.client.PoolFactory
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see org.springframework.data.gemfire.config.xml.PoolParser
 * @see <a href="https://jira.spring.io/browse/SGF-433">SGF-433</a>
 * @since 1.6.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class SpELExpressionConfiguredPoolsIntegrationTests {

	private static final ConnectionEndpointList anotherLocators = new ConnectionEndpointList();
	private static final ConnectionEndpointList anotherServers = new ConnectionEndpointList();
	private static final ConnectionEndpointList locators = new ConnectionEndpointList();
	private static final ConnectionEndpointList servers = new ConnectionEndpointList();

	@Autowired
	@Qualifier("locatorPool")
	private Pool locatorPool;

	@Autowired
	@Qualifier("serverPool")
	private Pool serverPool;

	@Autowired
	@Qualifier("anotherLocatorPool")
	private Pool anotherLocatorPool;

	@Autowired
	@Qualifier("anotherServerPool")
	private Pool anotherServerPool;

	private static void assertConnectionEndpoints(ConnectionEndpointList connectionEndpoints,
			String... expected) {

		assertThat(connectionEndpoints).isNotNull();
		assertThat(connectionEndpoints.size()).isEqualTo(expected.length);

		Collections.sort(connectionEndpoints);

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : connectionEndpoints) {
			assertThat(connectionEndpoint.toString()).isEqualTo(expected[index++]);
		}

		assertThat(index).isEqualTo(expected.length);
	}

	private static ConnectionEndpoint newConnectionEndpoint(String host, int port) {
		return new ConnectionEndpoint(host, port);
	}

	@Test
	public void anotherLocatorPoolFactoryConfiguration() {

		String[] expected = { "cardboardbox[10334]", "localhost[10335]", "pobox[10334]", "safetydepositbox[10336]" };

		assertConnectionEndpoints(anotherLocators, expected);
	}

	@Test
	public void anotherServerPoolFactoryConfiguration() {

		String[] expected = { "boombox[1234]", "jambox[40404]", "toolbox[8181]" };

		assertConnectionEndpoints(anotherServers, expected);
	}

	@Test
	public void locatorPoolFactoryConfiguration() {

		String[] expected = { "backspace[10334]", "jambox[11235]", "mars[30303]", "pluto[20668]", "skullbox[12480]" };

		assertConnectionEndpoints(locators, expected);
	}

	@Test
	public void serverPoolFactoryConfiguration() {

		String[] expected = {
			"earth[4554]", "jupiter[40404]", "mars[5112]", "mercury[1234]",
			"neptune[42424]", "saturn[41414]", "uranis[0]", "venus[9876]"
		};

		assertConnectionEndpoints(servers, expected);
	}

	public static class SpELBoundBean {

		private final Properties clientProperties;

		public SpELBoundBean(Properties clientProperties) {
			this.clientProperties = Optional.ofNullable(clientProperties)
				.orElseThrow(() -> newIllegalArgumentException("clientProperties are required"));
		}

		public String locatorsHostsPorts() {
			return "safetydepositbox[10336], pobox";
		}

		public String serverTwoHost() {
			return this.clientProperties.getProperty("gemfire.cache.client.server.2.host");
		}

		public String serverTwoPort() {
			return this.clientProperties.getProperty("gemfire.cache.client.server.2.port");
		}
	}

	public static class TestBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

		@Override
		public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {

			postProcessBeanDefinition(beanFactory, "anotherLocatorPool", AnotherLocatorPoolFactoryBean.class);
			postProcessBeanDefinition(beanFactory, "anotherServerPool", AnotherServerPoolFactoryBean.class);
			postProcessBeanDefinition(beanFactory, "locatorPool", LocatorPoolFactoryBean.class);
			postProcessBeanDefinition(beanFactory, "serverPool", ServerPoolFactoryBean.class);
		}

		private void postProcessBeanDefinition(ConfigurableListableBeanFactory beanFactory,
				String beanName, Class<?> beanType) {

			beanFactory.getBeanDefinition(beanName).setBeanClassName(beanType.getName());
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

	static class TestPoolFactoryBean extends PoolFactoryBean {

		ConnectionEndpointList getLocatorList() {
			throw new UnsupportedOperationException("Not Implemented");
		}

		ConnectionEndpointList getServerList() {
			throw new UnsupportedOperationException("Not Implemented");
		}

		@Override
		protected PoolFactory createPoolFactory() {

			PoolFactory mockPoolFactory = GemFireMockObjectsSupport.mockPoolFactory();

			when(mockPoolFactory.addLocator(anyString(), anyInt())).thenAnswer(invocation -> {
				String host = invocation.getArgument(0);
				int port = invocation.getArgument(1);
				getLocatorList().add(newConnectionEndpoint(host, port));
				return mockPoolFactory;
			});

			when(mockPoolFactory.addServer(anyString(), anyInt())).thenAnswer(invocation -> {
				String host = invocation.getArgument(0);
				int port = invocation.getArgument(1);
				getServerList().add(newConnectionEndpoint(host, port));
				return mockPoolFactory;
			});

			return mockPoolFactory;
		}

		@Override
		boolean isClientCachePresent() {
			return true;
		}
	}
}
