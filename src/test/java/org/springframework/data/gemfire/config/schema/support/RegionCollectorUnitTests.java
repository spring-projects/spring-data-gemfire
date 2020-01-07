/*
 * Copyright 2017-2020 the original author or authors.
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

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.context.ApplicationContext;

/**
 * Unit tests for {@link RegionCollector}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.schema.support.RegionCollector
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class RegionCollectorUnitTests {

	@Mock
	private ApplicationContext mockApplicationContext;

	@Mock
	private GemFireCache mockCache;

	private RegionCollector regionCollector = new RegionCollector();

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
	public void collectRegionsFromApplicationContext() {

		Region mockRegionOne = mockRegion("MockRegionOne");
		Region mockRegionTwo = mockRegion("MockRegionTwo");

		Map<String, Region> regionBeans = asMap(mockRegionOne, mockRegionTwo);

		when(this.mockApplicationContext.getBeansOfType(eq(Region.class))).thenReturn(regionBeans);

		Set<Region> actualRegions = this.regionCollector.collectFrom(this.mockApplicationContext);

		assertThat(actualRegions).isNotNull();
		assertThat(actualRegions).hasSize(2);
		assertThat(actualRegions).containsAll(asSet(mockRegionOne, mockRegionTwo));

		verify(this.mockApplicationContext, times(1)).getBeansOfType(eq(Region.class));
	}

	@Test
	public void collectRegionsFromApplicationContextWhenNoBeansOfTypeRegionExist() {

		when(this.mockApplicationContext.getBeansOfType(eq(Region.class))).thenReturn(Collections.emptyMap());

		Set<Region> actualRegions = this.regionCollector.collectFrom(this.mockApplicationContext);

		assertThat(actualRegions).isNotNull();
		assertThat(actualRegions).isEmpty();

		verify(this.mockApplicationContext, times(1)).getBeansOfType(eq(Region.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void collectRegionsFromGemFireCache() {

		Region mockRegionOne = mockRegion("MockRegionOne");
		Region mockRegionTwo = mockRegion("MockRegionTwo");

		when(this.mockCache.rootRegions()).thenReturn(asSet(mockRegionOne, mockRegionTwo));

		Set<Region> actualRegions = this.regionCollector.collectFrom(this.mockCache);

		assertThat(actualRegions).isNotNull();
		assertThat(actualRegions).hasSize(2);
		assertThat(actualRegions).containsAll(asSet(mockRegionOne, mockRegionTwo));

		verify(this.mockCache, times(1)).rootRegions();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void collectRegionsFromGemFireCacheWhenNoRootRegionsExist() {

		when(this.mockCache.rootRegions()).thenReturn(Collections.emptySet());

		Set<Region> actualRegions = this.regionCollector.collectFrom(this.mockCache);

		assertThat(actualRegions).isNotNull();
		assertThat(actualRegions).isEmpty();

		verify(this.mockCache, times(1)).rootRegions();
	}
}
