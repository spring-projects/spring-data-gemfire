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
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.execute.Function;

/**
 * The GemfireDataSourcePostProcessor class is a test suite of test cases testing the contract and functionality
 * of the GemfireDataSourcePostProcessor class, which is responsible for creating client PROXY Regions
 * for all data Regions on servers in a GemFire cluster, providing the ListRegion
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @see org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @see com.gemstone.gemfire.cache.client.ClientRegionFactory
 * @see com.gemstone.gemfire.cache.execute.Function
 * @since 1.7.0
 */
public class GemfireDataSourcePostProcessorTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(null) {
		@Override @SuppressWarnings("unchecked") <T> Iterable<T> execute(Function gemfireFunction, Object... arguments) {
			return (Iterable<T>) Arrays.asList("FunctionOne", "FunctionTwo");
		}
	};

	@Test
	public void postProcessBeanFactoryWhenListRegionsOnServerFunctionIsAvailable() {
		final AtomicBoolean createClientProxyRegionsCalled = new AtomicBoolean(false);

		final ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class,
			"MockStringBeanFactory");

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(null) {
			@Override boolean isFunctionAvailable(String targetFunctionId) {
				return true;
			}

			@Override void createClientProxyRegions(final ConfigurableListableBeanFactory beanFactory) {
				assertThat(beanFactory, is(sameInstance(mockBeanFactory)));
				createClientProxyRegionsCalled.compareAndSet(false, true);
			}
		};

		postProcessor.postProcessBeanFactory(mockBeanFactory);

		assertThat(createClientProxyRegionsCalled.get(), is(true));
	}

	@Test
	public void postProcessBeanFactoryWhenListRegionsOnServerFunctionIsNotAvailable() {
		final AtomicBoolean createClientProxyRegionsCalled = new AtomicBoolean(false);

		final ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class,
			"MockStringBeanFactory");

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(null) {
			@Override boolean isFunctionAvailable(String targetFunctionId) {
				return false;
			}

			@Override void createClientProxyRegions(final ConfigurableListableBeanFactory beanFactory) {
				assertThat(beanFactory, is(sameInstance(mockBeanFactory)));
				createClientProxyRegionsCalled.compareAndSet(false, true);
			}
		};

		postProcessor.postProcessBeanFactory(mockBeanFactory);

		assertThat(createClientProxyRegionsCalled.get(), is(false));
	}

	@Test
	public void isFunctionAvailableWhenFunctionIsAvailable() {
		assertThat(postProcessor.isFunctionAvailable("FunctionOne"), is(true));
	}

	@Test
	public void isFunctionAvailableWhenFunctionIsNotAvailable() {
		assertThat(postProcessor.isFunctionAvailable("functionOne"), is(false));
		assertThat(postProcessor.isFunctionAvailable("Functionone"), is(false));
		assertThat(postProcessor.isFunctionAvailable("FuncOne"), is(false));
		assertThat(postProcessor.isFunctionAvailable("Function1"), is(false));
		assertThat(postProcessor.isFunctionAvailable("FunctionUno"), is(false));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createClientProxyRegions() {
		ClientCache mockClientCache = mock(ClientCache.class, "MockGemFireClientCache");

		ClientRegionFactory mockClientRegionFactory = mock(ClientRegionFactory.class, "MockGemFireClientRegionFactory");

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.PROXY))).thenReturn(
			mockClientRegionFactory);

		Region mockRegionOne = mock(Region.class, "MockGemFireRegionOne");
		Region mockRegionTwo = mock(Region.class, "MockGemFireRegionTwo");

		final Map<String, Region<?, ?>> regionMap = new HashMap<String, Region<?, ?>>(2);

		regionMap.put("RegionOne", mockRegionOne);
		regionMap.put("RegionTwo", mockRegionTwo);

		doAnswer(new Answer<Region<?, ?>>() {
			@Override public Region<?, ?> answer(final InvocationOnMock invocation) throws Throwable {
				String regionName = invocation.getArgumentAt(0, String.class);
				assertThat(regionMap.containsKey(regionName), is(true));
				return regionMap.get(regionName);
			}
		}).when(mockClientRegionFactory).create(any(String.class));

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class, "SpringBeanFactory");

		when(mockBeanFactory.containsBean(any(String.class))).thenReturn(false);

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(mockClientCache) {
			@Override <T> Iterable<T> execute(final Function gemfireFunction, final Object... arguments) {
				return (Iterable<T>) Arrays.asList("RegionOne", "RegionTwo");
			}
		};

		postProcessor.createClientProxyRegions(mockBeanFactory);

		verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
		verify(mockClientRegionFactory, times(1)).create(eq("RegionOne"));
		verify(mockClientRegionFactory, times(1)).create(eq("RegionTwo"));
		verify(mockBeanFactory, times(1)).registerSingleton(eq("RegionOne"), same(mockRegionOne));
		verify(mockBeanFactory, times(1)).registerSingleton(eq("RegionTwo"), same(mockRegionTwo));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createClientProxyRegionWhenRegionBeanExists() {
		ClientCache mockClientCache = mock(ClientCache.class, "MockGemFireClientCache");

		ClientRegionFactory mockClientRegionFactory = mock(ClientRegionFactory.class, "MockGemFireClientRegionFactory");

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.PROXY))).thenReturn(
			mockClientRegionFactory);

		Region mockRegion = mock(Region.class, "MockGemFireRegion");

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class, "SpringBeanFactory");

		when(mockBeanFactory.containsBean(any(String.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("Example"))).thenReturn(mockRegion);

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(mockClientCache) {
			@Override <T> Iterable<T> execute(final Function gemfireFunction, final Object... arguments) {
				return (Iterable<T>) Collections.singletonList("Example");
			}
		};

		postProcessor.createClientProxyRegions(mockBeanFactory);

		verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
		verify(mockClientRegionFactory, never()).create(any(String.class));
		verify(mockBeanFactory, never()).registerSingleton(any(String.class), any(Region.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createClientProxyRegionWhenBeanOfDifferentTypeWithSameNameRegistered() {
		ClientCache mockClientCache = mock(ClientCache.class, "MockGemFireClientCache");

		ClientRegionFactory mockClientRegionFactory = mock(ClientRegionFactory.class, "MockGemFireClientRegionFactory");

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.PROXY))).thenReturn(
			mockClientRegionFactory);

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class, "SpringBeanFactory");

		when(mockBeanFactory.containsBean(any(String.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("Example"))).thenReturn(new Object());

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(mockClientCache) {
			@Override <T> Iterable<T> execute(final Function gemfireFunction, final Object... arguments) {
				return (Iterable<T>) Collections.singletonList("Example");
			}
		};

		try {
			expectedException.expect(IllegalArgumentException.class);
			expectedException.expectCause(is(nullValue(Throwable.class)));
			expectedException.expectMessage(is(equalTo(String.format(
				"Cannot create a client PROXY Region bean named '%1$s'. A bean with this name of type '%2$s' already exists.",
					"Example", Object.class.getName()))));
			postProcessor.createClientProxyRegions(mockBeanFactory);
		}
		finally {
			verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
			verify(mockClientRegionFactory, never()).create(any(String.class));
			verify(mockBeanFactory, never()).registerSingleton(any(String.class), any(Region.class));
		}
	}

}
