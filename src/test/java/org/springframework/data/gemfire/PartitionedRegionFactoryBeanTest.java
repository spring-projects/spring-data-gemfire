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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.Test;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.RegionFactory;

/**
 * The PartitionedRegionFactoryBeanTest class is a test suite of test cases testing the component functionality
 * and correct behavior of the PartitionedRegionFactoryBean class.
 * <p/>
 * @author John Blum
 * @see org.mockito.Mockito
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.PartitionedRegionFactoryBean
 * @since 1.3.3
 */
@SuppressWarnings("unchecked")
public class PartitionedRegionFactoryBeanTest {

	private final PartitionedRegionFactoryBean factoryBean = new PartitionedRegionFactoryBean();

	protected RegionFactory<?, ?> createMockRegionFactory() {
		return mock(RegionFactory.class);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithInvalidDataPolicyName() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.resolveDataPolicy(mockRegionFactory, null, "INVALID_DATA_POLICY_NAME");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'INVALID_DATA_POLICY_NAME' is invalid.", e.getMessage());
			throw e;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithInvalidDataPolicyRegionType() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.resolveDataPolicy(mockRegionFactory, null, "REPLICATE");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'REPLICATE' is not supported in Partitioned Regions.", e.getMessage());
			throw e;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.REPLICATE));
		}
	}

	@Test
	public void testResolveDataPolicyWithPersistentUnspecifiedAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PARTITION));
	}

	@Test
	public void testResolveDataPolicyWithPersistentUnspecifiedAndPartitionedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, "PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PARTITION));
	}

	@Test
	public void testResolveDataPolicyWithPersistentUnspecifiedAndPersistentPartitionedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, "PERSISTENT_PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
	}

	@Test
	public void testResolveDataPolicyWithPersistentFalseAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PARTITION));
	}

	@Test
	public void testResolveDataPolicyWithPersistentTrueAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
	}

	@Test
	public void testResolveDataPolicyWithPersistentFalseAndPartitionedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, "PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PARTITION));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithPersistentFalseAndPersistentPartitionedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(false);
			factoryBean.resolveDataPolicy(mockRegionFactory, false, "PERSISTENT_PARTITION");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'PERSISTENT_PARTITION' is invalid when persistent is false.", e.getMessage());
			throw e;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithPersistentTrueAndPartitionedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, "PARTITION");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'PARTITION' is invalid when persistent is true.", e.getMessage());
			throw e;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PARTITION));
		}
	}

	@Test
	public void testResolveDataPolicyWithPersistentTrueAndPersistentPartitionedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, "PERSISTENT_PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
	}

}
