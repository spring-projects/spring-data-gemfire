/*
 * Copyright 2012-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.cache;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.cache.Cache;

/**
 * Unit tests for {@link GemfireCacheManager}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.junit.MockitoJUnitRunner
 * @since 1.9.0
 */
@RunWith(MockitoJUnitRunner.class)
public class GemfireCacheManagerUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Mock
	private GemFireCache mockGemFireCache;

	private GemfireCacheManager cacheManager;

	@Mock
	private Region mockRegion;

	@Before
	public void setup() {
		cacheManager = new GemfireCacheManager();
	}

	@SafeVarargs
	private static <T> Set<T> asSet(T... elements) {
		Set<T> set = new HashSet<T>(elements.length);
		Collections.addAll(set, elements);
		return set;
	}

	@SuppressWarnings("unchecked")
	private Region<?, ?> mockRegion(String name) {
		Region<?, ?> mockRegion = mock(Region.class, name);
		when(mockRegion.getName()).thenReturn(name);
		return mockRegion;
	}

	private Region<?, ?> regionFor(Iterable<Region<?, ?>> regions, String name) {

		for (Region<?, ?> region : regions) {
			if (region.getName().equals(name)) {
				return region;
			}
		}

		return null;
	}

	@Test
	public void assertGemFireCacheAvailableWithAvailableGemFireCacheIsSuccessful() {
		when(mockGemFireCache.isClosed()).thenReturn(false);
		assertThat(cacheManager.assertGemFireCacheAvailable(mockGemFireCache)).isSameAs(mockGemFireCache);
		verify(mockGemFireCache, times(1)).isClosed();
		verify(mockGemFireCache, times(1)).getName();
	}

	@Test
	public void assertGemFireCacheAvailableWithNullThrowsIllegalStateException() {

		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(is(equalTo("A cache instance is required")));

		cacheManager.assertGemFireCacheAvailable(null);
	}

	@Test
	public void assertGemFireCacheAvailableWithNamedClosedGemFireCacheThrowsIllegalStateException() {

		when(mockGemFireCache.isClosed()).thenReturn(true);
		when(mockGemFireCache.getName()).thenReturn("Example");

		try {
			exception.expect(IllegalStateException.class);
			exception.expectCause(is(nullValue(Throwable.class)));
			exception.expectMessage(is(equalTo("The cache [Example] has been closed")));

			cacheManager.assertGemFireCacheAvailable(mockGemFireCache);
		}
		finally {
			verify(mockGemFireCache,times(1)).isClosed();
			verify(mockGemFireCache, times(1)).getName();
		}
	}

	@Test
	public void assertGemFireRegionAvailableWithAvailableGemFireRegionIsSuccessful() {

		when(mockRegion.isDestroyed()).thenReturn(false);
		assertThat(cacheManager.assertGemFireRegionAvailable(mockRegion, "Example")).isSameAs(mockRegion);
		verify(mockRegion, times(1)).isDestroyed();
	}

	@Test
	public void assertGemFireRegionAvailableWithNullThrowIllegalStateException() {

		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(is(equalTo("No Region for cache name [Example] was found")));

		cacheManager.assertGemFireRegionAvailable(null, "Example");
	}

	@Test
	public void assertGemFireRegionAvailableWithDestroyedGemFireRegionThrowIllegalStateException() {

		when(mockRegion.isDestroyed()).thenReturn(true);

		try {
			exception.expect(IllegalStateException.class);
			exception.expectCause(is(nullValue(Throwable.class)));
			exception.expectMessage(is(equalTo("Region [Example] has been destroyed")));

			cacheManager.assertGemFireRegionAvailable(mockRegion, "Example");
		}
		finally {
			verify(mockRegion, times(1)).isDestroyed();
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void loadCachesIsSuccessful() {

		Set<Region<?, ?>> regions = asSet(mockRegion("one"), mockRegion("two"), mockRegion("three"));

		cacheManager.setRegions(regions);

		Collection<Cache> loadedCaches = cacheManager.loadCaches();

		assertThat(loadedCaches).isNotNull();
		assertThat(loadedCaches.size()).isEqualTo(3);

		for (Cache cache : loadedCaches) {
			assertThat(cache).isInstanceOf(GemfireCache.class);
			assertThat(regionFor(regions, cache.getName())).isSameAs(cache.getNativeCache());
		}
	}

	@Test
	public void resolveRegionsReturnsGivenRegions() {

		Set<Region<?, ?>> regions = asSet(mockRegion("one"), mockRegion("two"));

		assertThat(cacheManager.resolveRegions(mockGemFireCache, regions, asSet("three", "four"))).isSameAs(regions);
		assertThat(cacheManager.isDynamic()).isFalse();

		verify(mockGemFireCache, never()).getRegion(anyString());
		verify(mockGemFireCache, never()).rootRegions();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void resolveRegionsReturnsRegionsForCacheNamesOnly() {

		Region mockRegionOne = mockRegion("one");
		Region mockRegionTwo = mockRegion("two");

		when(mockGemFireCache.getRegion(eq("one"))).thenReturn(mockRegionOne);
		when(mockGemFireCache.getRegion(eq("two"))).thenReturn(mockRegionTwo);

		Set<Region<?, ?>> regions = cacheManager.resolveRegions(mockGemFireCache, null, asSet("one", "two"));

		assertThat(regions).isNotNull();
		assertThat(regions.size()).isEqualTo(2);
		assertThat(regions).containsAll(this.<Region<?, ?>>asSet(mockRegionOne, mockRegionTwo));
		assertThat(cacheManager.isDynamic()).isFalse();

		verify(mockGemFireCache, times(1)).getRegion(eq("one"));
		verify(mockGemFireCache, times(1)).getRegion(eq("two"));
		verify(mockGemFireCache, never()).getRegion(eq("three"));
	}

	@Test
	public void resolveRegionsReturnsGemFireCacheRootRegions() {

		Set<Region<?, ?>> rootRegions = asSet(mockRegion("one"), mockRegion("two"));

		when(mockGemFireCache.rootRegions()).thenReturn(rootRegions);

		assertThat(cacheManager.resolveRegions(mockGemFireCache, null, null)).isSameAs(rootRegions);
		assertThat(cacheManager.isDynamic()).isTrue();

		verify(mockGemFireCache, times(1)).rootRegions();
		verify(mockGemFireCache, never()).getRegion(anyString());
	}

	@Test
	public void isSetForNonNullNonEmptyIterableIsTrue() {
		assertThat(cacheManager.isSet(Collections.singleton(1))).isTrue();
	}

	@Test
	public void isSetForNullIterableIsFalse() {
		assertThat(cacheManager.isSet(null)).isFalse();
	}

	@Test
	public void isSetForEmptyIterableIsFalse() {
		assertThat(cacheManager.isSet(Collections.emptyList())).isFalse();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void regionForCacheNameReturnsRegion() {

		when(mockGemFireCache.isClosed()).thenReturn(false);
		when(mockGemFireCache.getName()).thenReturn("regionForCacheNameReturnsRegion");
		when(mockGemFireCache.getRegion(eq("Example"))).thenReturn(mockRegion);
		when(mockRegion.isDestroyed()).thenReturn(false);

		assertThat(cacheManager.regionFor(mockGemFireCache, "Example")).isSameAs(mockRegion);

		verify(mockGemFireCache, times(1)).isClosed();
		verify(mockGemFireCache, times(1)).getName();
		verify(mockGemFireCache, times(1)).getRegion(eq("Example"));
		verify(mockRegion, times(1)).isDestroyed();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getMissingCacheReturnsMissingCache() {

		Region mockRegion = mockRegion("missing");

		when(mockGemFireCache.getRegion(eq("missing"))).thenReturn(mockRegion);

		cacheManager.setCache(mockGemFireCache);

		Cache cache = cacheManager.getMissingCache("missing");

		assertThat(cache).isNotNull();
		assertThat(cache.getNativeCache()).isEqualTo(mockRegion);

		verify(mockGemFireCache, times(1)).getRegion(eq("missing"));
	}

	@Test
	public void getMissingCacheReturnsNull() {

		cacheManager.setRegions(Collections.singleton(mockRegion("one")));
		cacheManager.afterPropertiesSet();

		assertThat(cacheManager.isDynamic()).isFalse();
		assertThat(cacheManager.getMissingCache("missing")).isNull();
	}

	@Test
	public void setAndGetCache() {

		assertThat(cacheManager.getCache()).isNull();

		cacheManager.setCache(mockGemFireCache);

		assertThat(cacheManager.getCache()).isSameAs(mockGemFireCache);

		cacheManager.setCache(null);

		assertThat(cacheManager.getCache()).isNull();
	}

	@Test
	public void setAndGetCacheNames() {

		Set<Region<?, ?>> regions = asSet(mockRegion("one"), mockRegion("two"));

		cacheManager.setRegions(regions);
		cacheManager.afterPropertiesSet();

		assertThat(cacheManager.getCacheNames()).containsAll(asSet("one", "two"));
	}

	@Test
	public void setAndGetRegions() {

		Set<Region<?, ?>> regions = asSet(mockRegion("one"), mockRegion("two"));

		assertThat(cacheManager.getRegions()).isNull();

		cacheManager.setRegions(regions);

		assertThat(cacheManager.getRegions()).isSameAs(regions);

		cacheManager.setRegions(null);

		assertThat(cacheManager.getRegions()).isNull();
	}
}
