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

package org.springframework.data.gemfire.client;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

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
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;

/**
 * The PoolUsingLocatorsAndServersPropertyPlaceholdersTest class is a test suite of test cases testing the use of
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
public class PoolUsingLocatorsAndServersPropertyPlaceholdersTest {

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
	@Qualifier("anotherServerPool")
	@SuppressWarnings("unused")
	private Pool anotherServerPool;

	protected static ConnectionEndpoint newConnectionEndpoint(String host, int port) {
		return new ConnectionEndpoint(host, port);
	}

	protected void assertConnectionEndpoints(Iterable<ConnectionEndpoint> connectionEndpoints, String... expected) {
		assertThat(connectionEndpoints, is(notNullValue()));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : connectionEndpoints) {
			assertThat(connectionEndpoint.toString(), is(equalTo(expected[index++])));
		}

		assertThat(index, is(equalTo(expected.length)));
	}

	@Test
	public void anotherServerPoolFactoryConfiguration() {
		String[] expected = { "boombox[1234]", "jambox[40404]", "toolbox[8181]" };

		assertThat(anotherServers.size(), is(equalTo(expected.length)));

		assertConnectionEndpoints(SpringUtils.sort(anotherServers), expected);
	}

	@Test
	public void locatorPoolFactoryConfiguration() {
		String[] expected = { "backspace[10334]", "jambox[11235]", "mars[30303]", "pluto[20668]", "skullbox[12480]" };

		assertThat(locators.size(), is(equalTo(expected.length)));

		assertConnectionEndpoints(SpringUtils.sort(locators), expected);
	}

	@Test
	public void serverPoolFactoryConfiguration() {
		String[] expected = { "earth[4554]", "jupiter[40404]", "mercury[1234]", "neptune[42424]", "saturn[41414]",
			"uranis[0]", "venus[9876]" };

		assertThat(servers.size(), is(equalTo(expected.length)));

		assertConnectionEndpoints(SpringUtils.sort(servers), expected);
	}

	public static class TestBeanFactoryPostProcessor implements BeanFactoryPostProcessor {
		@Override
		public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
			BeanDefinition anotherServerPoolBeanDefinition = beanFactory.getBeanDefinition("anotherServerPool");
			anotherServerPoolBeanDefinition.setBeanClassName(AnotherServerPoolFactoryBean.class.getName());
			BeanDefinition locatorPoolBeanDefinition = beanFactory.getBeanDefinition("locatorPool");
			locatorPoolBeanDefinition.setBeanClassName(LocatorPoolFactoryBean.class.getName());
			BeanDefinition serverPoolBeanDefinition = beanFactory.getBeanDefinition("serverPool");
			serverPoolBeanDefinition.setBeanClassName(ServerPoolFactoryBean.class.getName());
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
