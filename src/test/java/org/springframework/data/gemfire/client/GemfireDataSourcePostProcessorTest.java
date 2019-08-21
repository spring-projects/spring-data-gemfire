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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.ArgumentMatchers.startsWith;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
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

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

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

	@SuppressWarnings("unchecked")
	private Region<Object, Object> mockRegion(String name) {

		Region<Object, Object> mockRegion = mock(Region.class, name);

		RegionAttributes<Object, Object> mockRegionAttributes =
			mock(RegionAttributes.class, String.format("%s-RegionAttributes", name));

		when(mockRegion.getParentRegion()).thenReturn(null);
		when(mockRegion.getFullPath()).thenReturn(RegionUtils.toRegionPath(name));
		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getDataPolicy()).thenReturn(DataPolicy.PARTITION);
		when(mockRegionAttributes.getScope()).thenReturn(Scope.DISTRIBUTED_ACK);

		return mockRegion;
	}

	private RegionInformation newRegionInformation(Region<?, ?> region) {
		return new RegionInformation(region, false);
	}

	@Test
	public void constructGemfireDataSourcePostProcessor() {

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(mockClientCache);

		assertThat(postProcessor).isNotNull();
		assertThat(postProcessor.getClientCache()).isEqualTo(this.mockClientCache);
		assertThat(postProcessor.getClientRegionShortcut().orElse(null)).isEqualTo(ClientRegionShortcut.PROXY);
		assertThat(postProcessor.getLogger()).isNotNull();
	}

	@Test
	public void setAndGetClientRegionShortcut() {

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(this.mockClientCache);

		assertThat(postProcessor).isNotNull();
		assertThat(postProcessor.getClientRegionShortcut().orElse(null)).isEqualTo(ClientRegionShortcut.PROXY);

		postProcessor.setClientRegionShortcut(ClientRegionShortcut.CACHING_PROXY);

		assertThat(postProcessor.getClientRegionShortcut().orElse(null))
			.isEqualTo(ClientRegionShortcut.CACHING_PROXY);

		postProcessor.setClientRegionShortcut(ClientRegionShortcut.LOCAL);

		assertThat(postProcessor.getClientRegionShortcut().orElse(null)).isEqualTo(ClientRegionShortcut.LOCAL);

		postProcessor.setClientRegionShortcut(null);

		assertThat(postProcessor.getClientRegionShortcut().isPresent()).isFalse();
	}

	@Test
	public void postProcessBeanFactoryCallsCreateClientRegionProxiesWithRegionNames() {

		AtomicBoolean createClientRegionProxiesCalled = new AtomicBoolean(false);

		ConfigurableListableBeanFactory mockBeanFactory =
			mock(ConfigurableListableBeanFactory.class, "MockSpringBeanFactory");

		String[] testRegionNames = { "Test" };

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(this.mockClientCache) {

			@Override
			Iterable<String> regionNames() {
				return Arrays.asList(testRegionNames);
			}

			@Override
			void createClientProxyRegions(ConfigurableListableBeanFactory beanFactory, Iterable<String> regionNames) {
				assertThat(beanFactory).isSameAs(mockBeanFactory);
				assertThat(regionNames).containsExactly(testRegionNames);
				createClientRegionProxiesCalled.compareAndSet(false, true);
			}
		};

		postProcessor.postProcessBeanFactory(mockBeanFactory);

		assertThat(createClientRegionProxiesCalled.get()).isTrue();
	}

	@Test
	public void regionNamesWithListRegionsOnServerFunction() {

		String[] expectedRegionNames = { "ExampleOne", "ExampleTwo" };

		GemfireDataSourcePostProcessor postProcessor = spy(new GemfireDataSourcePostProcessor(this.mockClientCache));

		doReturn(Arrays.asList(expectedRegionNames)).when(postProcessor)
			.execute(isA(ListRegionsOnServerFunction.class), any());

		Iterable<String> actualRegionNames = postProcessor.regionNames();

		assertThat(actualRegionNames).containsExactly(expectedRegionNames);

		verify(postProcessor, times(1)).execute(isA(ListRegionsOnServerFunction.class));
		verify(postProcessor, never()).execute(any(GetRegionsFunction.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void regionNamesWithGetRegionsFunction() {

		String[] expectedRegionNames = { "ExampleOne", "ExampleTwo" };

		GemfireDataSourcePostProcessor postProcessor = spy(new GemfireDataSourcePostProcessor(this.mockClientCache));

		doThrow(new RuntimeException("fail")).when(postProcessor).execute(isA(ListRegionsOnServerFunction.class), any());

		doAnswer(invocation ->
			Arrays.stream(expectedRegionNames)
				.map(this::mockRegion)
				.map(this::newRegionInformation)
				.collect(Collectors.toList())
				.toArray()
		).when(postProcessor).execute(isA(GetRegionsFunction.class), any());

		List<String> actualRegionNames =
			StreamSupport.stream(postProcessor.regionNames().spliterator(), false).collect(Collectors.toList());

		assertThat(actualRegionNames).containsExactly(expectedRegionNames);

		verify(postProcessor, times(1)).execute(isA(ListRegionsOnServerFunction.class));
		verify(postProcessor, times(1)).execute(isA(GetRegionsFunction.class));
	}

	@Test
	public void regionNamesWithGetRegionsFunctionReturningNoResults() {

		GemfireDataSourcePostProcessor postProcessor = spy(new GemfireDataSourcePostProcessor(this.mockClientCache));

		doThrow(new RuntimeException("fail")).when(postProcessor).execute(isA(ListRegionsOnServerFunction.class), any());
		doReturn(null).when(postProcessor).execute(isA(GetRegionsFunction.class), any());

		Iterable<String> actualRegionNames = postProcessor.regionNames();

		assertThat(actualRegionNames).isNotNull();
		assertThat(actualRegionNames).isEmpty();

		verify(postProcessor, times(1)).execute(isA(ListRegionsOnServerFunction.class));
		verify(postProcessor, times(1)).execute(isA(GetRegionsFunction.class));
	}

	@Test
	public void regionNamesWithGetRegionsFunctionThrowingException() {

		GemfireDataSourcePostProcessor postProcessor = spy(new GemfireDataSourcePostProcessor(this.mockClientCache));

		doAnswer(invocation -> {

			Function function = invocation.getArgument(0);

			throw new IllegalArgumentException(String.format("Function [%1$s] with ID [%2$s] not registered",
				function.getClass().getName(), function.getId()));

		}).when(postProcessor).execute(any(Function.class), any());

		Iterable<String> actualRegionNames = postProcessor.regionNames();

		assertThat(actualRegionNames).isNotNull();
		assertThat(actualRegionNames).isEmpty();

		verify(postProcessor, times(1)).execute(isA(ListRegionsOnServerFunction.class));
		verify(postProcessor, times(1)).execute(isA(GetRegionsFunction.class));
		verify(postProcessor, times(1))
			.logDebug(startsWith("Failed to determine the Regions available on the Server:"), any());
	}

	@Test
	public void containsRegionInformationIsTrue() {
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache)
			.containsRegionInformation(new Object[] { newRegionInformation(mockRegion("Example")) }))
				.isTrue();
	}

	@Test
	public void containsRegionInformationWithEmptyArrayIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache)
			.containsRegionInformation(new Object[0]))
				.isFalse();
	}

	@Test
	public void containsRegionInformationWithListOfRegionInformationIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache)
			.containsRegionInformation(Collections.singletonList(newRegionInformation(mockRegion("Example")))))
				.isFalse();
	}

	@Test
	public void containsRegionInformationWithNonEmptyArrayContainingNonRegionInformationIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache)
			.containsRegionInformation(new Object[] { "test" }))
				.isFalse();
	}

	@Test
	public void containsRegionInformationWithNullIsFalse() {
		assertThat(new GemfireDataSourcePostProcessor(this.mockClientCache).containsRegionInformation(null))
			.isFalse();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createClientProxyRegionsIsSuccessful() {

		ClientCache mockClientCache = mock(ClientCache.class, "MockGemFireClientCache");

		ClientRegionFactory mockClientRegionFactory = mock(ClientRegionFactory.class, "MockGemFireClientRegionFactory");

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.PROXY))).thenReturn(
			mockClientRegionFactory);

		Region mockRegionOne = mock(Region.class, "MockGemFireRegionOne");
		Region mockRegionTwo = mock(Region.class, "MockGemFireRegionTwo");

		Map<String, Region<?, ?>> regionMap = new HashMap<>(2);

		regionMap.put("RegionOne", mockRegionOne);
		regionMap.put("RegionTwo", mockRegionTwo);

		doAnswer(invocation -> {

			String regionName = invocation.getArgument(0);

			assertThat(regionMap.containsKey(regionName)).isTrue();

			return regionMap.get(regionName);

		}).when(mockClientRegionFactory).create(any(String.class));

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class, "MockSpringBeanFactory");

		when(mockBeanFactory.containsBean(any(String.class))).thenReturn(false);

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(mockClientCache);

		postProcessor.createClientProxyRegions(mockBeanFactory, regionMap.keySet());

		verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
		verify(mockClientRegionFactory, times(1)).create(eq("RegionOne"));
		verify(mockClientRegionFactory, times(1)).create(eq("RegionTwo"));
		verify(mockBeanFactory, times(1)).registerSingleton(eq("RegionOne"), same(mockRegionOne));
		verify(mockBeanFactory, times(1)).registerSingleton(eq("RegionTwo"), same(mockRegionTwo));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createClientProxyRegionsWhenRegionBeanExists() {

		ClientCache mockClientCache = mock(ClientCache.class, "MockGemFireClientCache");

		ClientRegionFactory mockClientRegionFactory = mock(ClientRegionFactory.class, "MockGemFireClientRegionFactory");

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.PROXY))).thenReturn(
			mockClientRegionFactory);

		Region mockRegion = mock(Region.class, "MockGemFireRegion");

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class, "MockSpringBeanFactory");

		when(mockBeanFactory.containsBean(any(String.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("Example"))).thenReturn(mockRegion);

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(mockClientCache);

		postProcessor.createClientProxyRegions(mockBeanFactory, Collections.singletonList("Example"));

		verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
		verify(mockClientRegionFactory, never()).create(any(String.class));
		verify(mockBeanFactory, never()).registerSingleton(any(String.class), any(Region.class));
	}

	@Test
	public void usingIsCorrect() {

		GemfireDataSourcePostProcessor postProcessor = new GemfireDataSourcePostProcessor(this.mockClientCache)
			.using(ClientRegionShortcut.LOCAL);

		assertThat(postProcessor).isNotNull();
		assertThat(postProcessor.getClientRegionShortcut().orElse(null)).isEqualTo(ClientRegionShortcut.LOCAL);
		assertThat(postProcessor.using(ClientRegionShortcut.CACHING_PROXY)).isEqualTo(postProcessor);
		assertThat(postProcessor.getClientRegionShortcut()
			.orElse(null)).isEqualTo(ClientRegionShortcut.CACHING_PROXY);
		assertThat(postProcessor.using(null)).isEqualTo(postProcessor);
		assertThat(postProcessor.getClientRegionShortcut().isPresent()).isFalse();
	}
}
