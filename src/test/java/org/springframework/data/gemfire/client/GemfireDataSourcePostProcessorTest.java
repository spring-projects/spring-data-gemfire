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

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.Scope;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.management.internal.cli.domain.RegionInformation;
import org.apache.geode.management.internal.cli.functions.GetRegionsFunction;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.client.function.ListRegionsOnServerFunction;

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
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientRegionFactory
 * @since 1.7.0
 */
public class GemfireDataSourcePostProcessorTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	protected RegionInformation newRegionInformation(Region<?, ?> region) {
		return new RegionInformation(region, false);
	}

	@SuppressWarnings("unchecked")
	protected Region<Object, Object> mockRegion(String name) {
		Region<Object, Object> mockRegion = mock(Region.class, name);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class,
			String.format("%1$s-RegionAttributes", name));

		when(mockRegion.getFullPath()).thenReturn(String.format("%1$s%2$s", Region.SEPARATOR, name));
		when(mockRegion.getName()).thenReturn(name);
		when(mockRegion.getParentRegion()).thenReturn(null);
		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getDataPolicy()).thenReturn(DataPolicy.PARTITION);
		when(mockRegionAttributes.getScope()).thenReturn(Scope.DISTRIBUTED_ACK);

		return mockRegion;
	}

	protected <T> List<T> asList(Iterable<T> iterable) {
		List<T> list = new ArrayList<T>();

		if (iterable != null) {
			for (T element : iterable) {
				list.add(element);
			}
		}

		return list;
	}

	@Test
	public void postProcessBeanFactoryCallsCreateClientRegionProxiesWithRegionNames() {
		final AtomicBoolean createClientRegionProxiesCalled = new AtomicBoolean(false);
		final ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class, "MockSpringBeanFactory");
		final List<String> testRegionNames = Collections.singletonList("Test");

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(null) {
			@Override Iterable<String> regionNames() {
				return testRegionNames;
			}

			@Override void createClientRegionProxies(ConfigurableListableBeanFactory beanFactory, Iterable<String> regionNames) {
				assertThat(beanFactory, is(sameInstance(mockBeanFactory)));
				assertSame(testRegionNames, regionNames);
				createClientRegionProxiesCalled.compareAndSet(false, true);
			}
		};

		postProcessor.postProcessBeanFactory(mockBeanFactory);

		assertThat(createClientRegionProxiesCalled.get(), is(true));
	}

	@Test
	public void regionNamesWithListRegionsOnServerFunction() {
		final List<String> expectedRegionNames = Arrays.asList("ExampleOne", "ExampleTwo");

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(null) {
			@Override @SuppressWarnings("unchecked") <T> T execute(Function gemfireFunction, Object... arguments) {
				assertThat(gemfireFunction, is(instanceOf(ListRegionsOnServerFunction.class)));
				return (T) expectedRegionNames;
			}
		};

		Iterable<String> actualRegionNames = postProcessor.regionNames();

		assertSame(expectedRegionNames, actualRegionNames);
	}

	@Test
	public void regionNamesWithGetRegionsFunction() {
		final List<String> expectedRegionNames = Arrays.asList("ExampleOne", "ExampleTwo");

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(null) {
			@Override @SuppressWarnings("unchecked") <T> T execute(Function gemfireFunction, Object... arguments) {
				if (gemfireFunction instanceof ListRegionsOnServerFunction) {
					throw new RuntimeException("fail");
				}
				else if (gemfireFunction instanceof GetRegionsFunction) {
					return (T) Arrays.asList(newRegionInformation(mockRegion(expectedRegionNames.get(0))),
						newRegionInformation(mockRegion(expectedRegionNames.get(1)))).toArray();
				}

				throw new IllegalArgumentException(String.format("GemFire Function (%1$s) with ID (%2$s) not registered",
					gemfireFunction.getClass().getName(), gemfireFunction.getId()));
			}
		};

		Iterable<String> actualRegionNames = postProcessor.regionNames();

		assertThat(asList(actualRegionNames).containsAll(expectedRegionNames), is(true));
	}

	@Test
	public void regionNamesWithGetRegionsFunctionReturningNoResults() {
		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(null) {
			@Override @SuppressWarnings("unchecked") <T> T execute(Function gemfireFunction, Object... arguments) {
				if (gemfireFunction instanceof ListRegionsOnServerFunction) {
					throw new RuntimeException("fail");
				}
				else if (gemfireFunction instanceof GetRegionsFunction) {
					return null;
				}

				throw new IllegalArgumentException(String.format("GemFire Function (%1$s) with ID (%2$s) not registered",
					gemfireFunction.getClass().getName(), gemfireFunction.getId()));
			}
		};

		Iterable<String> actualRegionNames = postProcessor.regionNames();

		assertThat(actualRegionNames, is(not(nullValue())));
		assertThat(actualRegionNames.iterator(), (is(not(nullValue()))));
		assertThat(actualRegionNames.iterator().hasNext(), is(false));
	}

	@Test
	public void regionNamesWithGetRegionsFunctionThrowingException() {
		final AtomicBoolean logMethodCalled = new AtomicBoolean(false);

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(null) {
			@Override @SuppressWarnings("unchecked") <T> T execute(Function gemfireFunction, Object... arguments) {
				throw new IllegalArgumentException(String.format("GemFire Function (%1$s) with ID (%2$s) not registered",
					gemfireFunction.getClass().getName(), gemfireFunction.getId()));
			}

			@Override void log(final String message, final Object... arguments) {
				assertThat(message.startsWith("Failed to determine the Regions available on the Server:"), is(true));
				logMethodCalled.compareAndSet(false, true);
			}
		};

		Iterable<String> actualRegionNames = postProcessor.regionNames();

		assertThat(actualRegionNames, is(not(nullValue())));
		assertThat(actualRegionNames.iterator(), (is(not(nullValue()))));
		assertThat(actualRegionNames.iterator().hasNext(), is(false));
		assertThat(logMethodCalled.get(), is(true));
	}

	@Test
	public void containsRegionInformationIsTrue() {
		assertThat(new GemfireDataSourcePostProcessor(null).containsRegionInformation(
			new Object[] { newRegionInformation(mockRegion("Example")) }), is(true));
	}

	@Test
	public void containsRegionInformationWithListOfRegionInformationIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(null).containsRegionInformation(
			Arrays.asList(newRegionInformation(mockRegion("Example")))), is(false));
	}

	@Test
	public void containsRegionInformationWithNonEmptyArrayContainingNonRegionInformationIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(null).containsRegionInformation(new Object[] { "test" }),
			is(false));
	}

	@Test
	public void containsRegionInformationWithEmptyArrayIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(null).containsRegionInformation(new Object[0]), is(false));
	}

	@Test
	public void containsRegionInformationWithNullIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(null).containsRegionInformation(null), is(false));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createClientRegionProxies() {
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
				String regionName = invocation.getArgument(0);
				assertThat(regionMap.containsKey(regionName), is(true));
				return regionMap.get(regionName);
			}
		}).when(mockClientRegionFactory).create(any(String.class));

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class, "MockSpringBeanFactory");

		when(mockBeanFactory.containsBean(any(String.class))).thenReturn(false);

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(mockClientCache);

		postProcessor.createClientRegionProxies(mockBeanFactory, regionMap.keySet());

		verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
		verify(mockClientRegionFactory, times(1)).create(eq("RegionOne"));
		verify(mockClientRegionFactory, times(1)).create(eq("RegionTwo"));
		verify(mockBeanFactory, times(1)).registerSingleton(eq("RegionOne"), same(mockRegionOne));
		verify(mockBeanFactory, times(1)).registerSingleton(eq("RegionTwo"), same(mockRegionTwo));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createClientRegionProxiesWhenRegionBeanExists() {
		ClientCache mockClientCache = mock(ClientCache.class, "MockGemFireClientCache");

		ClientRegionFactory mockClientRegionFactory = mock(ClientRegionFactory.class, "MockGemFireClientRegionFactory");

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.PROXY))).thenReturn(
			mockClientRegionFactory);

		Region mockRegion = mock(Region.class, "MockGemFireRegion");

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class, "MockSpringBeanFactory");

		when(mockBeanFactory.containsBean(any(String.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("Example"))).thenReturn(mockRegion);

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(mockClientCache);

		postProcessor.createClientRegionProxies(mockBeanFactory, Arrays.asList("Example"));

		verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
		verify(mockClientRegionFactory, never()).create(any(String.class));
		verify(mockBeanFactory, never()).registerSingleton(any(String.class), any(Region.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createClientRegionProxiesWhenBeanOfDifferentTypeWithSameNameAsRegionIsRegistered() {
		ClientCache mockClientCache = mock(ClientCache.class, "MockGemFireClientCache");

		ClientRegionFactory mockClientRegionFactory = mock(ClientRegionFactory.class, "MockGemFireClientRegionFactory");

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.PROXY))).thenReturn(
			mockClientRegionFactory);

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class, "MockSpringBeanFactory");

		when(mockBeanFactory.containsBean(any(String.class))).thenReturn(true);
		when(mockBeanFactory.getBean(any(String.class))).thenReturn(new Object());

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(mockClientCache);

		try {
			expectedException.expect(IllegalArgumentException.class);
			expectedException.expectCause(is(nullValue(Throwable.class)));
			expectedException.expectMessage(is(equalTo(String.format(
				"Cannot create a client PROXY Region bean named '%1$s'. A bean with this name of type '%2$s' already exists.",
					"Example", Object.class.getName()))));
			postProcessor.createClientRegionProxies(mockBeanFactory, Arrays.asList("Example"));
		}
		finally {
			verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
			verify(mockClientRegionFactory, never()).create(any(String.class));
			verify(mockBeanFactory, never()).registerSingleton(any(String.class), any(Region.class));
		}
	}

}
