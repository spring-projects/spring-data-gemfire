/*
 * Copyright 2018-2019 the original author or authors.
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

import org.apache.geode.cache.DataPolicy;
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
		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.REPLICATE, null);
		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PERSISTENT_PARTITION, null);
		RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PERSISTENT_REPLICATE, null);
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
	public void testAssertNonPersistentDataPolicyWithPersistentAttribute() {

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
	public void testAssertPersistentDataPolicyWithNonPersistentAttribute() {

		try {
			RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy.PERSISTENT_PARTITION, false);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Data Policy [PERSISTENT_PARTITION] is not valid when persistent is false");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}
}
