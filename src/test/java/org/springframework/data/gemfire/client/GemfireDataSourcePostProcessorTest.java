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

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

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
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.client.function.ListRegionsOnServerFunction;
import org.springframework.data.gemfire.util.RegionUtils;

/**
 * Unit tests for {@link GemfireDataSourcePostProcessor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientRegionFactory
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @see org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor
 * @see org.springframework.data.gemfire.client.function.ListRegionsOnServerFunction
 * @since 1.7.0
 */
@RunWith(MockitoJUnitRunner.class)
public class GemfireDataSourcePostProcessorTest {

	@Mock
	private ClientCache mockClientCache;

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private RegionInformation newRegionInformation(Region<?, ?> region) {
		return new RegionInformation(region, false);
	}

	@SuppressWarnings("unchecked")
	private Region<Object, Object> mockRegion(String name) {

		Region<Object, Object> mockRegion = mock(Region.class, name);

		RegionAttributes<Object, Object> mockRegionAttributes =
			mock(RegionAttributes.class, String.format("%1$s-RegionAttributes", name));

		when(mockRegion.getParentRegion()).thenReturn(null);
		when(mockRegion.getFullPath()).thenReturn(RegionUtils.toRegionPath(name));
		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getDataPolicy()).thenReturn(DataPolicy.PARTITION);
		when(mockRegionAttributes.getScope()).thenReturn(Scope.DISTRIBUTED_ACK);

		return mockRegion;
	}

	@Test
	public void postProcessBeanFactoryCallsCreateClientRegionProxiesWithRegionNames() {

		AtomicBoolean createClientRegionProxiesCalled = new AtomicBoolean(false);

		ConfigurableListableBeanFactory mockBeanFactory =
			mock(ConfigurableListableBeanFactory.class, "MockSpringBeanFactory");

		List<String> testRegionNames = Collections.singletonList("Test");

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(this.mockClientCache) {

			@Override
			Iterable<String> regionNames() {
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

		List<String> expectedRegionNames = Arrays.asList("ExampleOne", "ExampleTwo");

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(this.mockClientCache) {

			@Override @SuppressWarnings("unchecked")
			<T> T execute(Function gemfireFunction, Object... arguments) {
				assertThat(gemfireFunction, is(instanceOf(ListRegionsOnServerFunction.class)));
				return (T) expectedRegionNames;
			}
		};

		Iterable<String> actualRegionNames = postProcessor.regionNames();

		assertSame(expectedRegionNames, actualRegionNames);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void regionNamesWithGetRegionsFunction() {

		List<String> expectedRegionNames = Arrays.asList("ExampleOne", "ExampleTwo");

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(this.mockClientCache) {

			@Override @SuppressWarnings("unchecked")
			<T> T execute(Function gemfireFunction, Object... arguments) {

				if (gemfireFunction instanceof ListRegionsOnServerFunction) {
					throw new RuntimeException("fail");
				}
				else if (gemfireFunction instanceof GetRegionsFunction) {
					return (T) Arrays.asList(newRegionInformation(mockRegion(expectedRegionNames.get(0))),
						newRegionInformation(mockRegion(expectedRegionNames.get(1)))).toArray();
				}

				throw new IllegalArgumentException(String.format("GemFire Function [%1$s] with ID [%2$s] not registered",
					gemfireFunction.getClass().getName(), gemfireFunction.getId()));
			}
		};

		List<String> actualRegionNames =
			StreamSupport.stream(postProcessor.regionNames().spliterator(), false).collect(Collectors.toList());

		assertThat(actualRegionNames.containsAll(expectedRegionNames), is(true));
	}

	@Test
	public void regionNamesWithGetRegionsFunctionReturningNoResults() {

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(this.mockClientCache) {

			@Override @SuppressWarnings("unchecked") <T> T
			execute(Function gemfireFunction, Object... arguments) {

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

		AtomicBoolean logMethodCalled = new AtomicBoolean(false);

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(this.mockClientCache) {

			@Override @SuppressWarnings("unchecked") <T> T
			execute(Function gemfireFunction, Object... arguments) {
				throw new IllegalArgumentException(String.format("GemFire Function (%1$s) with ID (%2$s) not registered",
					gemfireFunction.getClass().getName(), gemfireFunction.getId()));
			}

			@Override
			void log(final String message, final Object... arguments) {
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
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache)
			.containsRegionInformation(new Object[] { newRegionInformation(mockRegion("Example")) }),
				is(true));
	}

	@Test
	public void containsRegionInformationWithListOfRegionInformationIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache)
			.containsRegionInformation(Collections.singletonList(newRegionInformation(mockRegion("Example")))),
				is(false));
	}

	@Test
	public void containsRegionInformationWithNonEmptyArrayContainingNonRegionInformationIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache)
				.containsRegionInformation(new Object[] { "test" }), is(false));
	}

	@Test
	public void containsRegionInformationWithEmptyArrayIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache)
			.containsRegionInformation(new Object[0]), is(false));
	}

	@Test
	public void containsRegionInformationWithNullIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache).containsRegionInformation(null),
			is(false));
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

		postProcessor.createClientRegionProxies(mockBeanFactory, Collections.singletonList("Example"));

		verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
		verify(mockClientRegionFactory, never()).create(any(String.class));
		verify(mockBeanFactory, never()).registerSingleton(any(String.class), any(Region.class));
	}
}
