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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.client.PoolFactory;

/**
 * The PoolUsingLocatorsAndServersPropertyPlaceholdersTest class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class PoolUsingLocatorsAndServersPropertyPlaceholdersTest {

	private static ConnectionEndpointList locatorConnectionEndpoints = new ConnectionEndpointList();
	private static ConnectionEndpointList serverConnectionEndpoints = new ConnectionEndpointList();

	private static PoolFactory mockPoolFactory;

	protected static ConnectionEndpoint newConnectionEndpoint(String host, int port) {
		return new ConnectionEndpoint(host, port);
	}

	@BeforeClass
	public static void setup() {
		mockPoolFactory = mock(PoolFactory.class, "MockPoolFactory");

		when(mockPoolFactory.addLocator(anyString(), anyInt())).thenAnswer(new Answer<PoolFactory>() {
			@Override
			public PoolFactory answer(final InvocationOnMock invocation) throws Throwable {
				String host = invocation.getArgumentAt(0, String.class);
				int port = invocation.getArgumentAt(1, Integer.class);
				locatorConnectionEndpoints.add(newConnectionEndpoint(host, port));
				return mockPoolFactory;
			}
		});

		when(mockPoolFactory.addServer(anyString(), anyInt())).thenAnswer(new Answer<PoolFactory>() {
			@Override
			public PoolFactory answer(final InvocationOnMock invocation) throws Throwable {
				String host = invocation.getArgumentAt(0, String.class);
				int port = invocation.getArgumentAt(1, Integer.class);
				serverConnectionEndpoints.add(newConnectionEndpoint(host, port));
				return mockPoolFactory;
			}
		});
	}

	protected ConnectionEndpointList sort(ConnectionEndpointList list) {
		List<ConnectionEndpoint> connectionEndpoints = new ArrayList<ConnectionEndpoint>(list.size());

		for (ConnectionEndpoint connectionEndpoint : list) {
			connectionEndpoints.add(connectionEndpoint);
		}

		Collections.sort(connectionEndpoints);

		return new ConnectionEndpointList(connectionEndpoints);
	}

	protected void assertConnectionEndpoints(ConnectionEndpointList connectionEndpoints, String... expected) {
		assertThat(connectionEndpoints.isEmpty(), is(false));
		assertThat(connectionEndpoints.size(), is(equalTo(expected.length)));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : connectionEndpoints) {
			assertThat(connectionEndpoint.toString(), is(equalTo(expected[index++])));
		}
	}

	@Test
	public void locatorPoolFactoryConfiguration() {
		String[] expected = { "backspace[10334]", "jambox[11235]", "mars[30303]", "pluto[20668]", "skullbox[12480]" };

//		System.out.printf("locatorPool is... %1$s%n", locatorConnectionEndpoints);

		assertThat(locatorConnectionEndpoints.isEmpty(), is(false));
		assertThat(locatorConnectionEndpoints.size(), is(equalTo(expected.length)));

		assertConnectionEndpoints(sort(locatorConnectionEndpoints), expected);
	}

	@Test
	public void serverPoolFactoryConfiguration() {
		String[] expected = { "earth[4554]", "jupiter[40404]", "mercury[1234]", "neptune[42424]", "saturn[41414]",
			"uranis[0]", "venus[9876]" };

//		System.out.printf("serverPool is... %1$s%n", serverConnectionEndpoints);

		assertThat(serverConnectionEndpoints.isEmpty(), is(false));
		assertThat(serverConnectionEndpoints.size(), is(equalTo(expected.length)));

		assertConnectionEndpoints(sort(serverConnectionEndpoints), expected);
	}

	public static class TestBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

		@Override
		public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
			BeanDefinition locatorsPoolBeanDefinition = beanFactory.getBeanDefinition("locatorPool");
			locatorsPoolBeanDefinition.setBeanClassName(TestPoolFactoryBean.class.getName());
			BeanDefinition serversPoolBeanDefinition = beanFactory.getBeanDefinition("serverPool");
			serversPoolBeanDefinition.setBeanClassName(TestPoolFactoryBean.class.getName());
		}
	}

	public static class TestPoolFactoryBean extends PoolFactoryBean {

		@Override
		protected PoolFactory createPoolFactory() {
			return mockPoolFactory;
		}

		@Override
		protected void resolveDistributedSystem() {
		}
	}

}
