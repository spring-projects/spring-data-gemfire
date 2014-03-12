/*
 * Copyright 2010-2013 the original author or authors.
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

import org.junit.Test;
import org.springframework.data.gemfire.support.AbstractRegionFactoryBeanTest;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionFactory;

/**
 * @author David Turanski
 * @author John Blum
 */
@SuppressWarnings("unchecked")
public class RegionFactoryBeanTest extends AbstractRegionFactoryBeanTest {

	private final RegionFactoryBean factoryBean = new RegionFactoryBean();

	@SuppressWarnings("rawtypes")
	private RegionFactoryBeanConfig defaultConfig() {
		return new RegionFactoryBeanConfig(new RegionFactoryBean(), "default") {
			@Override
			public void verify() {
				Region region = regionFactoryBean.getRegion();
				assertEquals(DataPolicy.DEFAULT, region.getAttributes().getDataPolicy());
			}

			@Override
			public void configureRegionFactoryBean() {
				// TODO Auto-generated method stub
			}
		};
	}

	@SuppressWarnings("rawtypes")
	private RegionFactoryBeanConfig persistent() {
		return new RegionFactoryBeanConfig(new RegionFactoryBean(), "persistent") {
			@Override
			public void verify() {
				Region region = regionFactoryBean.getRegion();
				assertEquals(DataPolicy.PERSISTENT_REPLICATE, region.getAttributes().getDataPolicy());
			}

			@Override
			public void configureRegionFactoryBean() {
				regionFactoryBean.setPersistent(true);
			}
		};
	}

	@SuppressWarnings("rawtypes")
	private RegionFactoryBeanConfig invalidPersistence() {
		return new RegionFactoryBeanConfig(new RegionFactoryBean(), "invalid-persistence") {
			@Override
			public void configureRegionFactoryBean() {
				regionFactoryBean.setDataPolicy("persistent_replicate");
				regionFactoryBean.setPersistent(false);
			}

			@Override
			public void verify() {
				assertNotNull(this.exception);
				assertEquals("Data Policy 'PERSISTENT_REPLICATE' is invalid when persistent is false.",
					exception.getMessage());
			}
		};
	}

	@Override
	protected void createRegionFactoryBeanConfigs() {
		addRFBConfig(defaultConfig());
		addRFBConfig(persistent());
		addRFBConfig(invalidPersistence());
	}

	protected RegionFactory<?, ?> createMockRegionFactory() {
		return mock(RegionFactory.class);
	}

	@Test
	public void testResolveDataPolicyWithPersistentUnspecifiedAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.DEFAULT));
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.DEFAULT));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithInvalidDataPolicyName() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		try {
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, "CSV");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'CSV' is invalid.", e.getMessage());
			throw e;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test
	public void testResolveDataPolicyWithPersistentUnspecifiedAndNormalDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, "NORMAL");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.NORMAL));
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentUnspecifiedAndPreloadedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, "PRELOADED");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PRELOADED));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenPersistentAndPersistentEmptyDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		try {
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, "EMPTY");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'EMPTY' is invalid when persistent is true.", e.getMessage());
			throw e;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.EMPTY));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test
	public void testResolveDataPolicyWithPersistentUnspecifiedAndPersistentPartitionedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, "PERSISTENT_PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenNotPersistentAndPersistentPartitionedDataPolicy() {
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
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndPersistentPartitionedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, "PERSISTENT_PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
	}

}
