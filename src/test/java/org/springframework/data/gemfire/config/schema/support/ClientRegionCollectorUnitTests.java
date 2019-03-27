/*
 * Copyright 2017-2019 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.schema.support;

import static java.util.Arrays.stream;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationContext;

/**
 * Unit tests for {@link ClientRegionCollector}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.schema.support.ClientRegionCollector
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ClientRegionCollectorUnitTests {

	@Mock
	private ApplicationContext mockApplicationContext;

	@Mock
	private GemFireCache mockCache;

	private ClientRegionCollector clientRegionCollector = new ClientRegionCollector();

	@SuppressWarnings("unchecked")
	private <K, V> Region<K, V> mockRegion(String name) {

		Region<K, V> mockRegion = mock(Region.class, name);

		when(mockRegion.getName()).thenReturn(name);

		return mockRegion;
	}

	private Map<String, Region> asMap(Region<?, ?>... regions) {
		return stream(regions).collect(Collectors.toMap(Region::getName, Function.identity()));
	}

	@Test
	public void collectClientRegionsFromApplicationContext() {

		Region mockRegionOne = mockRegion("MockRegionOne");
		Region mockRegionTwo = mockRegion("MockRegionTwo");
		Region mockRegionThree = mockRegion("MockRegionThree");

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegionAttributes.getPoolName()).thenReturn("TestPool");
		when(mockRegionTwo.getAttributes()).thenReturn(mockRegionAttributes);

		Map<String, Region> regionBeans = asMap(mockRegionOne, mockRegionTwo, mockRegionThree);

		when(this.mockApplicationContext.getBeansOfType(eq(Region.class))).thenReturn(regionBeans);

		Set<Region> clientRegions = this.clientRegionCollector.collectFrom(this.mockApplicationContext);

		assertThat(clientRegions).isNotNull();
		assertThat(clientRegions).hasSize(1);
		assertThat(clientRegions).containsAll(asSet(mockRegionTwo));

		verify(this.mockApplicationContext, times(1)).getBeansOfType(eq(Region.class));
	}

	@Test
	public void collectClientRegionsFromApplicationContextWithNoClientRegions() {

		Region mockRegionOne = mockRegion("MockRegionOne");
		Region mockRegionTwo = mockRegion("MockRegionTwo");
		Region mockRegionThree = mockRegion("MockRegionThree");

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegionAttributes.getPoolName()).thenReturn("  ");
		when(mockRegionOne.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionThree.getAttributes()).thenReturn(mockRegionAttributes);

		Map<String, Region> regionBeans = asMap(mockRegionOne, mockRegionTwo, mockRegionThree);

		when(this.mockApplicationContext.getBeansOfType(eq(Region.class))).thenReturn(regionBeans);

		Set<Region> clientRegions = this.clientRegionCollector.collectFrom(this.mockApplicationContext);

		assertThat(clientRegions).isNotNull();
		assertThat(clientRegions).isEmpty();

		verify(this.mockApplicationContext, times(1)).getBeansOfType(eq(Region.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void collectClientRegionsFromGemFireCache() {

		Region mockRegionOne = mockRegion("MockRegionOne");
		Region mockRegionTwo = mockRegion("MockRegionTwo");
		Region mockRegionThree = mockRegion("MockRegionThree");

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegionAttributes.getPoolName()).thenReturn("TestPool");
		when(mockRegionOne.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionTwo.getAttributes()).thenReturn(mockRegionAttributes);

		when(this.mockCache.rootRegions()).thenReturn(asSet(mockRegionOne, mockRegionTwo, mockRegionThree));

		Set<Region> clientRegions = this.clientRegionCollector.collectFrom(this.mockCache);

		assertThat(clientRegions).isNotNull();
		assertThat(clientRegions).hasSize(2);
		assertThat(clientRegions).containsAll(asSet(mockRegionOne, mockRegionTwo));

		verify(this.mockCache, times(1)).rootRegions();
	}
}
