/*
 * Copyright 2019 the original author or authors.
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
package org.springframework.data.gemfire.client;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.client.Pool;

/**
 * Unit Tests for {@link PoolResolver}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.Pool
 * @since 2.3.0
 */
@RunWith(MockitoJUnitRunner.class)
public class PoolResolverUnitTests {

	@Mock
	private TestPoolResolver testPoolResolver;

	@Before
	public void setup() {
		when(this.testPoolResolver.resolve(any(Region.class))).thenCallRealMethod();
	}

	@Test
	public void resolvePoolFromRegionWithPoolReturnsPool() {

		Pool mockPool = mock(Pool.class);

		Region mockRegion = mock(Region.class);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getPoolName()).thenReturn("TestPool");
		when(this.testPoolResolver.resolve(eq("TestPool"))).thenReturn(mockPool);

		assertThat(this.testPoolResolver.resolve(mockRegion)).isEqualTo(mockPool);

		verifyNoInteractions(mockPool);
		verify(mockRegion, times(1)).getAttributes();
		verify(mockRegionAttributes, times(1)).getPoolName();
		verify(this.testPoolResolver, times(1)).resolve(eq("TestPool"));
	}

	private void testResolvePoolFromRegionWithNoPoolReturnsNull(String poolName) {

		Region mockRegion = mock(Region.class);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getPoolName()).thenReturn(poolName);

		assertThat(this.testPoolResolver.resolve(mockRegion)).isNull();

		verify(mockRegion, times(1)).getAttributes();
		verify(mockRegionAttributes, times(1)).getPoolName();
		verify(this.testPoolResolver, never()).resolve(eq(poolName));
	}

	@Test
	public void resolvePoolWithRegionWithBlankPoolNameReturnsNull() {
		testResolvePoolFromRegionWithNoPoolReturnsNull("  ");
	}

	@Test
	public void resolvePoolWithRegionWithEmptyPoolNameReturnsNull() {
		testResolvePoolFromRegionWithNoPoolReturnsNull("");
	}

	@Test
	public void resolvePoolWithRegionWithNullPoolNameReturnsNull() {
		testResolvePoolFromRegionWithNoPoolReturnsNull(null);
	}

	@Test
	public void resolvePoolWithNullRegionIsNullSafeAndReturnsNull() {
		assertThat(this.testPoolResolver.resolve((Region) null)).isNull();
	}

	@Test
	public void resolvePoolWithRegionHavingNullRegionAttributesIsNullSafeAndReturnsNull() {

		Region mockRegion = mock(Region.class);

		assertThat(this.testPoolResolver.resolve(mockRegion)).isNull();

		verify(mockRegion, times(1)).getAttributes();
	}

	private static abstract class TestPoolResolver implements PoolResolver { }

}
