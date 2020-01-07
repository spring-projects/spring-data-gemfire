/*
 * Copyright 2018-2020 the original author or authors.
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

package org.springframework.data.gemfire.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionService;

import org.junit.Test;

/**
 * Unit tests for {@link RegionUtils}.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.util.RegionUtils
 * @since 2.1.0
 */
public class RegionUtilsUnitTests {

	@Test
	public void assertAllDataPoliciesWithNullPersistentPropertyIsCompatible() {

		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PARTITION, null);
		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PERSISTENT_PARTITION, null);
		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PERSISTENT_REPLICATE, null);
		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.REPLICATE, null);
	}

	@Test
	public void assertNonPersistentDataPolicyWithNoPersistenceIsCompatible() {

		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PARTITION, false);
		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.REPLICATE, false);
	}

	@Test
	public void assertPersistentDataPolicyWithPersistenceIsCompatible() {

		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PERSISTENT_PARTITION, true);
		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PERSISTENT_REPLICATE, true);
	}

	@Test(expected = IllegalArgumentException.class)
	public void assertNonPersistentDataPolicyWithPersistentAttribute() {

		try {
			RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.REPLICATE, true);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Data Policy [REPLICATE] is not valid when persistent is true");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void assertPersistentDataPolicyWithNonPersistentAttribute() {

		try {
			RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PERSISTENT_PARTITION, false);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Data Policy [PERSISTENT_PARTITION] is not valid when persistent is false");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void closeRegionHandlesNull() {
		assertThat(RegionUtils.close((Region<?, ?>) null)).isFalse();
	}

	@Test
	public void closeRegionSuccessfully() {

		Region mockRegion = mock(Region.class);

		assertThat(RegionUtils.close(mockRegion)).isTrue();

		verify(mockRegion, times(1)).close();
	}

	@Test
	public void closeRegionUnsuccessfully() {

		Region mockRegion = mock(Region.class);

		doThrow(new RuntimeException("TEST")).when(mockRegion).close();

		assertThat(RegionUtils.close(mockRegion)).isFalse();

		verify(mockRegion, times(1)).close();
	}

	@Test
	public void nullRegionIsNotCloseable() {
		assertThat(RegionUtils.isCloseable(null)).isFalse();
	}

	@Test
	public void regionIsCloseable() {

		Region mockRegion = mock(Region.class);
		RegionService mockRegionService = mock(RegionService.class);

		when(mockRegion.getRegionService()).thenReturn(mockRegionService);
		when(mockRegionService.isClosed()).thenReturn(false);

		assertThat(RegionUtils.isCloseable(mockRegion)).isTrue();

		verify(mockRegion, times(1)).getRegionService();
		verify(mockRegionService, times(1)).isClosed();
	}

	@Test
	public void regionIsNotCloseable() {

		Region mockRegion = mock(Region.class);
		RegionService mockRegionService = mock(RegionService.class);

		when(mockRegion.getRegionService()).thenReturn(mockRegionService);
		when(mockRegionService.isClosed()).thenReturn(true);

		assertThat(RegionUtils.isCloseable(mockRegion)).isFalse();

		verify(mockRegion, times(1)).getRegionService();
		verify(mockRegionService, times(1)).isClosed();
	}

	@Test
	public void regionWithNoRegionServiceIsNotCloseable() {

		Region mockRegion = mock(Region.class);

		when(mockRegion.getRegionService()).thenReturn(null);

		assertThat(RegionUtils.isCloseable(mockRegion)).isFalse();

		verify(mockRegion, times(1)).getRegionService();
	}
}
