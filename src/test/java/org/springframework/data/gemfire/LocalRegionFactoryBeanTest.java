/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionFactory;
import org.apache.geode.cache.RegionShortcut;
import org.junit.Test;
import org.springframework.data.gemfire.test.support.AbstractRegionFactoryBeanTests;

/**
 * The PartitionedRegionFactoryBeanTest class is a test suite of test cases testing the component functionality
 * and correct behavior of the PartitionedRegionFactoryBean class.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.mockito.Mockito
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.LocalRegionFactoryBean
 * @since 1.3.x
 */
@SuppressWarnings("unchecked")
public class LocalRegionFactoryBeanTest extends AbstractRegionFactoryBeanTests {

	private final LocalRegionFactoryBean factoryBean = new LocalRegionFactoryBean();

	@SuppressWarnings("rawtypes")
	private RegionFactoryBeanConfig defaultConfig() {
		return new RegionFactoryBeanConfig(new LocalRegionFactoryBean(), "default") {

			@Override
			public void configureRegionFactoryBean() {
			}

			@Override
			public void verify() {
				Region region = regionFactoryBean.getRegion();
				assertNotNull(region);
				assertEquals(DataPolicy.DEFAULT, region.getAttributes().getDataPolicy());
			}
		};
	}

	@SuppressWarnings({ "deprecation", "rawtypes" })
	private RegionFactoryBeanConfig invalidConfig() {
		return new RegionFactoryBeanConfig(new LocalRegionFactoryBean(), "local-replicate") {

			@Override
			public void configureRegionFactoryBean() {
				regionFactoryBean.setDataPolicy("replicate");
			}

			@Override
			public void verify() {
				assertNotNull(this.exception);
			}
		};
	}

	@Override
	protected void createRegionFactoryBeanConfigs() {
		add(defaultConfig());
		add(invalidConfig());
	}

	protected RegionFactory<?, ?> createMockRegionFactory() {
		return mock(RegionFactory.class);
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, (String) null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.NORMAL));
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, (String) null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.NORMAL));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, (String) null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithBlankDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.resolveDataPolicy(mockRegionFactory, null, "  ");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy '  ' is invalid.", e.getMessage());
			throw e;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.NORMAL));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PRELOADED));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithEmptyDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.resolveDataPolicy(mockRegionFactory, null, "");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy '' is invalid.", e.getMessage());
			throw e;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.NORMAL));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PRELOADED));
		}
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
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.NORMAL));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PRELOADED));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithInvalidDataPolicyType() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.resolveDataPolicy(mockRegionFactory, null, "PARTITION");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'PARTITION' is not supported for Local Regions.", e.getMessage());
			throw e;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.NORMAL));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PRELOADED));
		}
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndNormalDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, "Normal");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.NORMAL));
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndNormalDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, "NORMAL");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.NORMAL));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndNormalDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, "NORMAL");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndPreloadedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, "preloaded");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PRELOADED));
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndPreloadedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, "PreLoaded");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PRELOADED));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndPreloadedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, "PRELOADED");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenShortcutIsNullAndPersistentReplicateDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setShortcut(null);
			factoryBean.resolveDataPolicy(mockRegionFactory, null, DataPolicy.PERSISTENT_REPLICATE);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'PERSISTENT_REPLICATE' is not supported for Local Regions.",
				expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(DataPolicy.NORMAL);
			verify(mockRegionFactory, never()).setDataPolicy(DataPolicy.PRELOADED);
			verify(mockRegionFactory, never()).setDataPolicy(DataPolicy.PERSISTENT_REPLICATE);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenShortcutNotPersistentAndPersistentReplicateDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setShortcut(RegionShortcut.LOCAL_OVERFLOW);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, DataPolicy.PERSISTENT_REPLICATE);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'PERSISTENT_REPLICATE' is not supported for Local Regions.",
				expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(DataPolicy.NORMAL);
			verify(mockRegionFactory, never()).setDataPolicy(DataPolicy.PRELOADED);
			verify(mockRegionFactory, never()).setDataPolicy(DataPolicy.PERSISTENT_REPLICATE);
		}
	}

	@Test
	public void testResolveDataPolicyWhenPersistentPersistentReplicateDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setShortcut(RegionShortcut.LOCAL_PERSISTENT_OVERFLOW);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, DataPolicy.PERSISTENT_REPLICATE);
		verify(mockRegionFactory).setDataPolicy(DataPolicy.PERSISTENT_REPLICATE);
	}
}
