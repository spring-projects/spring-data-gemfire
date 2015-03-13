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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.After;
import org.junit.Test;
import org.springframework.data.gemfire.support.AbstractRegionFactoryBeanTest;
import org.springframework.data.gemfire.test.support.ArrayUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CustomExpiry;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.ExpirationAttributes;
import com.gemstone.gemfire.cache.MembershipAttributes;
import com.gemstone.gemfire.cache.PartitionAttributes;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.RegionShortcut;
import com.gemstone.gemfire.cache.SubscriptionAttributes;
import com.gemstone.gemfire.internal.cache.GemFireCacheImpl;

/**
 * The RegionFactoryBeanTest class is a test suite of test cases testing the contract and functionality of the
 * RegionFactoryBean class.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.RegionFactoryBean
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.DataPolicy
 * @see com.gemstone.gemfire.cache.PartitionAttributes
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.RegionAttributes
 * @see com.gemstone.gemfire.cache.RegionFactory
 * @see com.gemstone.gemfire.cache.RegionShortcut
 */
@SuppressWarnings("unchecked")
public class RegionFactoryBeanTest extends AbstractRegionFactoryBeanTest {

	private final RegionFactoryBean factoryBean = new TestRegionFactoryBean();

	@After
	public void tearDown() {
		factoryBean.setDataPolicy((DataPolicy) null);
		factoryBean.setShortcut(null);
	}

	@SuppressWarnings("rawtypes")
	private RegionFactoryBeanConfig defaultConfig() {
		return new RegionFactoryBeanConfig(new TestRegionFactoryBean(), "default") {
			@Override
			public void configureRegionFactoryBean() {
			}

			@Override
			public void verify() {
				Region region = regionFactoryBean.getRegion();
				assertEquals(DataPolicy.DEFAULT, region.getAttributes().getDataPolicy());
			}
		};
	}

	@SuppressWarnings("rawtypes")
	private RegionFactoryBeanConfig persistentConfig() {
		return new RegionFactoryBeanConfig(new TestRegionFactoryBean(), "persistent") {
			@Override
			public void configureRegionFactoryBean() {
				regionFactoryBean.setPersistent(true);
			}

			@Override
			public void verify() {
				Region region = regionFactoryBean.getRegion();
				assertEquals(DataPolicy.PERSISTENT_REPLICATE, region.getAttributes().getDataPolicy());
			}
		};
	}

	@SuppressWarnings({ "deprecation", "rawtypes" })
	private RegionFactoryBeanConfig invalidPersistentConfig() {
		return new RegionFactoryBeanConfig(new TestRegionFactoryBean(), "invalid-persistence") {
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
		add(defaultConfig());
		add(persistentConfig());
		add(invalidPersistentConfig());
	}

	protected PartitionAttributes createPartitionAttributes(final String colocatedWith, final int localMaxMemory,
			final long recoveryDelay, final int redundantCopies, final long startupRecoveryDelay,
			final long totalMaxMemory, final int totalNumberOfBuckets) throws Exception {

		PartitionAttributesFactoryBean partitionAttributesFactoryBean = new PartitionAttributesFactoryBean();

		partitionAttributesFactoryBean.setColocatedWith(colocatedWith);
		partitionAttributesFactoryBean.setLocalMaxMemory(localMaxMemory);
		partitionAttributesFactoryBean.setRecoveryDelay(recoveryDelay);
		partitionAttributesFactoryBean.setRedundantCopies(redundantCopies);
		partitionAttributesFactoryBean.setStartupRecoveryDelay(startupRecoveryDelay);
		partitionAttributesFactoryBean.setTotalMaxMemory(totalMaxMemory);
		partitionAttributesFactoryBean.setTotalNumBuckets(totalNumberOfBuckets);
		partitionAttributesFactoryBean.afterPropertiesSet();

		return partitionAttributesFactoryBean.getObject();
	}

	protected RegionAttributes createMockRegionAttributes(final DataPolicy... dataPolicies) {
		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		when(mockRegionAttributes.getDataPolicy()).thenReturn(ArrayUtils.getFirst(DataPolicy.DEFAULT, dataPolicies));
		return mockRegionAttributes;
	}

	protected RegionFactory<?, ?> createMockRegionFactory() {
		return mock(RegionFactory.class);
	}

	protected RegionFactory<?, ?> createTestRegionFactory() {
		return new TestRegionFactory();
	}
	@Test
	public void testAssertDataPolicyAndPersistentAttributesAreCompatible() {
		RegionFactoryBean<?, ?> factoryBean = new TestRegionFactoryBean<Object, Object>();

		factoryBean.setPersistent(null);
		factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.PARTITION);
		factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.REPLICATE);
		factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.PERSISTENT_PARTITION);
		factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.PERSISTENT_REPLICATE);
		factoryBean.setPersistent(false);
		factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.PARTITION);
		factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.REPLICATE);
		factoryBean.setPersistent(true);
		factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.PERSISTENT_PARTITION);
		factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.PERSISTENT_REPLICATE);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAssertNonPersistentDataPolicyWithPersistentAttribute() {
		try {
			RegionFactoryBean<?, ?> factoryBean = new TestRegionFactoryBean<Object, Object>();
			factoryBean.setPersistent(true);
			factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.REPLICATE);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'REPLICATE' is invalid when persistent is true.", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAssertPersistentDataPolicyWithNonPersistentAttribute() {
		try {
			RegionFactoryBean<?, ?> factoryBean = new TestRegionFactoryBean<Object, Object>();
			factoryBean.setPersistent(false);
			factoryBean.assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy.PERSISTENT_PARTITION);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'PERSISTENT_PARTITION' is invalid when persistent is false.",
				expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testIsPersistent() {
		RegionFactoryBean<?, ?> factoryBean = new TestRegionFactoryBean<Object, Object>();

		assertFalse(factoryBean.isPersistent());

		factoryBean.setPersistent(false);

		assertFalse(factoryBean.isPersistent());

		factoryBean.setPersistent(true);

		assertTrue(factoryBean.isPersistent());
	}

	@Test
	public void testIsPersistentUnspecified() {
		RegionFactoryBean<?, ?> factoryBean = new TestRegionFactoryBean<Object, Object>();

		assertTrue(factoryBean.isPersistentUnspecified());

		factoryBean.setPersistent(false);

		assertFalse(factoryBean.isPersistentUnspecified());

		factoryBean.setPersistent(true);

		assertFalse(factoryBean.isPersistentUnspecified());

		factoryBean.setPersistent(null);

		assertTrue(factoryBean.isPersistentUnspecified());
	}

	@Test
	public void testIsNotPersistent() {
		RegionFactoryBean<?, ?> factoryBean = new TestRegionFactoryBean<Object, Object>();

		assertFalse(factoryBean.isNotPersistent());

		factoryBean.setPersistent(true);

		assertFalse(factoryBean.isNotPersistent());

		factoryBean.setPersistent(false);

		assertTrue(factoryBean.isNotPersistent());
	}

	@Test
	public void testCreateRegionFactoryWithShortcut() {
		Cache mockCache = mock(Cache.class);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);

		final RegionFactory mockRegionFactory = createMockRegionFactory();

		when(mockCache.createRegionFactory(eq(RegionShortcut.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW)))
			.thenReturn(mockRegionFactory);

		final AtomicBoolean setDataPolicyCalled = new AtomicBoolean(false);

		RegionFactoryBean factoryBean = new RegionFactoryBean() {
			@Override
			DataPolicy getDataPolicy(final RegionFactory regionFactory) {
				return DataPolicy.PERSISTENT_PARTITION;
			}

			@Override
			public void setDataPolicy(final DataPolicy dataPolicy) {
				assertEquals(DataPolicy.PERSISTENT_PARTITION, dataPolicy);
				super.setDataPolicy(dataPolicy);
				setDataPolicyCalled.set(true);
			}

			@Override
			protected RegionFactory mergeRegionAttributes(RegionFactory regionFactory, RegionAttributes regionAttributes) {
				return mockRegionFactory;
			}
		};

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setShortcut(RegionShortcut.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW);

		assertSame(mockRegionFactory, factoryBean.createRegionFactory(mockCache));
		assertTrue(setDataPolicyCalled.get());

		verify(mockCache, times(1)).createRegionFactory(eq(RegionShortcut.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW));
	}

	@Test
	public void testCreateRegionFactoryWithAttributes() {
		Cache mockCache = mock(Cache.class);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		RegionFactory mockRegionFactory = createMockRegionFactory();

		when(mockCache.createRegionFactory(eq(mockRegionAttributes))).thenReturn(mockRegionFactory);

		RegionFactoryBean factoryBean = new TestRegionFactoryBean();

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setShortcut(null);

		assertSame(mockRegionFactory, factoryBean.createRegionFactory(mockCache));

		verify(mockCache, times(1)).createRegionFactory(eq(mockRegionAttributes));
	}

	@Test
	public void testCreateRegionFactory() {
		Cache mockCache = mock(Cache.class);
		RegionFactory mockRegionFactory = createMockRegionFactory();

		when(mockCache.createRegionFactory()).thenReturn(mockRegionFactory);

		RegionFactoryBean factoryBean = new TestRegionFactoryBean();

		factoryBean.setAttributes(null);
		factoryBean.setShortcut(null);

		assertSame(mockRegionFactory, factoryBean.createRegionFactory(mockCache));

		verify(mockCache).createRegionFactory();
	}

	@Test
	public void testMergeRegionAttributes() throws Exception {
		EvictionAttributes testEvictionAttributes = EvictionAttributes.createLRUEntryAttributes();
		ExpirationAttributes testExpirationAttributes = new ExpirationAttributes(120, ExpirationAction.LOCAL_DESTROY);
		MembershipAttributes testMembershipAttributes = new MembershipAttributes();
		PartitionAttributes testPartitionAttributes = createPartitionAttributes("TestRegion", 1024000, 15000l, 0,
			45000l, 2048000000l, 97);
		SubscriptionAttributes testSubscriptionAttributes = new SubscriptionAttributes();

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		RegionFactory<Long, String> mockRegionFactory = (RegionFactory<Long, String>) createMockRegionFactory();

		when(mockRegionAttributes.getCloningEnabled()).thenReturn(false);
		when(mockRegionAttributes.getConcurrencyChecksEnabled()).thenReturn(true);
		when(mockRegionAttributes.getConcurrencyLevel()).thenReturn(51);
		when(mockRegionAttributes.getCustomEntryIdleTimeout()).thenReturn(null);
		when(mockRegionAttributes.getCustomEntryTimeToLive()).thenReturn(null);
		when(mockRegionAttributes.isDiskSynchronous()).thenReturn(true);
		when(mockRegionAttributes.getEnableAsyncConflation()).thenReturn(false);
		when(mockRegionAttributes.getEnableSubscriptionConflation()).thenReturn(false);
		when(mockRegionAttributes.getEntryIdleTimeout()).thenReturn(testExpirationAttributes);
		when(mockRegionAttributes.getEntryTimeToLive()).thenReturn(testExpirationAttributes);
		when(mockRegionAttributes.getEvictionAttributes()).thenReturn(testEvictionAttributes);
		when(mockRegionAttributes.getIgnoreJTA()).thenReturn(false);
		when(mockRegionAttributes.getIndexMaintenanceSynchronous()).thenReturn(false);
		when(mockRegionAttributes.getInitialCapacity()).thenReturn(1024);
		when(mockRegionAttributes.getKeyConstraint()).thenReturn(Long.class);
		when(mockRegionAttributes.getLoadFactor()).thenReturn(0.90f);
		when(mockRegionAttributes.isLockGrantor()).thenReturn(true);
		when(mockRegionAttributes.getMembershipAttributes()).thenReturn(testMembershipAttributes);
		when(mockRegionAttributes.getMulticastEnabled()).thenReturn(false);
		when(mockRegionAttributes.getPartitionAttributes()).thenReturn(testPartitionAttributes);
		when(mockRegionAttributes.getPoolName()).thenReturn("swimming");
		when(mockRegionAttributes.getRegionIdleTimeout()).thenReturn(testExpirationAttributes);
		when(mockRegionAttributes.getRegionTimeToLive()).thenReturn(testExpirationAttributes);
		when(mockRegionAttributes.getStatisticsEnabled()).thenReturn(true);
		when(mockRegionAttributes.getSubscriptionAttributes()).thenReturn(testSubscriptionAttributes);

		RegionFactoryBean factoryBean = new RegionFactoryBean() {
			@Override boolean isUserSpecifiedEvictionAttributes(final RegionAttributes regionAttributes) {
				return true;
			}

			@Override void validateRegionAttributes(final RegionAttributes regionAttributes) {
				// no-op!
			}
		};

		factoryBean.mergeRegionAttributes(mockRegionFactory, mockRegionAttributes);

		verify(mockRegionFactory).setCloningEnabled(eq(false));
		verify(mockRegionFactory).setConcurrencyChecksEnabled(eq(true));
		verify(mockRegionFactory).setConcurrencyLevel(eq(51));
		verify(mockRegionFactory).setCustomEntryIdleTimeout(null);
		verify(mockRegionFactory).setCustomEntryTimeToLive(null);
		verify(mockRegionFactory).setDiskSynchronous(eq(true));
		verify(mockRegionFactory).setEnableAsyncConflation(eq(false));
		verify(mockRegionFactory).setEnableSubscriptionConflation(eq(false));
		verify(mockRegionFactory).setEntryIdleTimeout(same(testExpirationAttributes));
		verify(mockRegionFactory).setEntryTimeToLive(same(testExpirationAttributes));
		verify(mockRegionFactory).setEvictionAttributes(same(testEvictionAttributes));
		verify(mockRegionFactory).setIgnoreJTA(eq(false));
		verify(mockRegionFactory).setIndexMaintenanceSynchronous(eq(false));
		verify(mockRegionFactory).setInitialCapacity(eq(1024));
		verify(mockRegionFactory).setKeyConstraint(Long.class);
		verify(mockRegionFactory).setLoadFactor(eq(0.90f));
		verify(mockRegionFactory).setLockGrantor(eq(true));
		verify(mockRegionFactory).setMembershipAttributes(same(testMembershipAttributes));
		verify(mockRegionFactory).setMulticastEnabled(eq(false));
		verify(mockRegionFactory).setPartitionAttributes(eq(testPartitionAttributes));
		verify(mockRegionFactory).setPoolName(eq("swimming"));
		verify(mockRegionFactory).setRegionIdleTimeout(same(testExpirationAttributes));
		verify(mockRegionFactory).setRegionTimeToLive(same(testExpirationAttributes));
		verify(mockRegionFactory).setStatisticsEnabled(eq(true));
		verify(mockRegionFactory).setSubscriptionAttributes(same(testSubscriptionAttributes));
	}

	@Test
	public void testMergeRegionAttributesWithNull() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		factoryBean.mergeRegionAttributes(mockRegionFactory, null);

		verify(mockRegionFactory, never()).setCloningEnabled(false);
		verify(mockRegionFactory, never()).setConcurrencyChecksEnabled(true);
		verify(mockRegionFactory, never()).setConcurrencyLevel(2);
		verify(mockRegionFactory, never()).setCustomEntryIdleTimeout(any(CustomExpiry.class));
		verify(mockRegionFactory, never()).setCustomEntryTimeToLive(any(CustomExpiry.class));
		verify(mockRegionFactory, never()).setDiskSynchronous(true);
		verify(mockRegionFactory, never()).setEnableAsyncConflation(true);
		verify(mockRegionFactory, never()).setEnableSubscriptionConflation(false);
		verify(mockRegionFactory, never()).setEntryIdleTimeout(any(ExpirationAttributes.class));
		verify(mockRegionFactory, never()).setEntryTimeToLive(any(ExpirationAttributes.class));
		verify(mockRegionFactory, never()).setEvictionAttributes(any(EvictionAttributes.class));
		verify(mockRegionFactory, never()).setIgnoreJTA(false);
		verify(mockRegionFactory, never()).setIndexMaintenanceSynchronous(false);
		verify(mockRegionFactory, never()).setInitialCapacity(51);
		verify(mockRegionFactory, never()).setKeyConstraint(any(Class.class));
		verify(mockRegionFactory, never()).setLoadFactor(0.75f);
		verify(mockRegionFactory, never()).setLockGrantor(true);
		verify(mockRegionFactory, never()).setMembershipAttributes(any(MembershipAttributes.class));
		verify(mockRegionFactory, never()).setMulticastEnabled(false);
		verify(mockRegionFactory, never()).setPartitionAttributes(any(PartitionAttributes.class));
		verify(mockRegionFactory, never()).setPoolName(any(String.class));
		verify(mockRegionFactory, never()).setRegionIdleTimeout(any(ExpirationAttributes.class));
		verify(mockRegionFactory, never()).setRegionTimeToLive(any(ExpirationAttributes.class));
		verify(mockRegionFactory, never()).setStatisticsEnabled(true);
		verify(mockRegionFactory, never()).setSubscriptionAttributes(any(SubscriptionAttributes.class));
		verify(mockRegionFactory, never()).setValueConstraint(any(Class.class));
	}

	@Test
	public void testPartialMergeRegionAttributes() {
		ExpirationAttributes testExpirationAttributes = new ExpirationAttributes(300, ExpirationAction.LOCAL_INVALIDATE);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		RegionFactory<Long, String> mockRegionFactory = (RegionFactory<Long, String>) createMockRegionFactory();

		when(mockRegionAttributes.getCloningEnabled()).thenReturn(true);
		when(mockRegionAttributes.getConcurrencyChecksEnabled()).thenReturn(false);
		when(mockRegionAttributes.getConcurrencyLevel()).thenReturn(8);
		when(mockRegionAttributes.getCustomEntryIdleTimeout()).thenReturn(null);
		when(mockRegionAttributes.getCustomEntryTimeToLive()).thenReturn(null);
		when(mockRegionAttributes.isDiskSynchronous()).thenReturn(false);
		when(mockRegionAttributes.getEnableAsyncConflation()).thenReturn(true);
		when(mockRegionAttributes.getEnableSubscriptionConflation()).thenReturn(true);
		when(mockRegionAttributes.getEntryIdleTimeout()).thenReturn(testExpirationAttributes);
		when(mockRegionAttributes.getEntryTimeToLive()).thenReturn(testExpirationAttributes);
		when(mockRegionAttributes.getEvictionAttributes()).thenReturn(null);
		when(mockRegionAttributes.getIgnoreJTA()).thenReturn(true);
		when(mockRegionAttributes.getIndexMaintenanceSynchronous()).thenReturn(true);
		when(mockRegionAttributes.getInitialCapacity()).thenReturn(512);
		when(mockRegionAttributes.getKeyConstraint()).thenReturn(Long.class);
		when(mockRegionAttributes.getLoadFactor()).thenReturn(0.60f);
		when(mockRegionAttributes.isLockGrantor()).thenReturn(false);
		when(mockRegionAttributes.getMembershipAttributes()).thenReturn(null);
		when(mockRegionAttributes.getMulticastEnabled()).thenReturn(true);
		when(mockRegionAttributes.getPartitionAttributes()).thenReturn(null);
		when(mockRegionAttributes.getPoolName()).thenReturn("swimming");
		when(mockRegionAttributes.getRegionIdleTimeout()).thenReturn(testExpirationAttributes);
		when(mockRegionAttributes.getRegionTimeToLive()).thenReturn(testExpirationAttributes);
		when(mockRegionAttributes.getStatisticsEnabled()).thenReturn(true);
		when(mockRegionAttributes.getSubscriptionAttributes()).thenReturn(null);

		RegionFactoryBean factoryBean = new RegionFactoryBean() {
			@Override boolean isUserSpecifiedEvictionAttributes(final RegionAttributes regionAttributes) {
				return false;
			}

			@Override void validateRegionAttributes(final RegionAttributes regionAttributes) {
				// no-op!
			}
		};

		factoryBean.mergeRegionAttributes(mockRegionFactory, mockRegionAttributes);

		verify(mockRegionFactory).setCloningEnabled(eq(true));
		verify(mockRegionFactory).setConcurrencyChecksEnabled(eq(false));
		verify(mockRegionFactory).setConcurrencyLevel(eq(8));
		verify(mockRegionFactory).setCustomEntryIdleTimeout(null);
		verify(mockRegionFactory).setCustomEntryTimeToLive(null);
		verify(mockRegionFactory).setDiskSynchronous(eq(false));
		verify(mockRegionFactory).setEnableAsyncConflation(eq(true));
		verify(mockRegionFactory).setEnableSubscriptionConflation(eq(true));
		verify(mockRegionFactory).setEntryIdleTimeout(same(testExpirationAttributes));
		verify(mockRegionFactory).setEntryTimeToLive(same(testExpirationAttributes));
		verify(mockRegionFactory, never()).setEvictionAttributes(any(EvictionAttributes.class));
		verify(mockRegionFactory).setIgnoreJTA(eq(true));
		verify(mockRegionFactory).setIndexMaintenanceSynchronous(eq(true));
		verify(mockRegionFactory).setInitialCapacity(eq(512));
		verify(mockRegionFactory).setKeyConstraint(Long.class);
		verify(mockRegionFactory).setLoadFactor(eq(0.60f));
		verify(mockRegionFactory).setLockGrantor(eq(false));
		verify(mockRegionFactory).setMembershipAttributes(null);
		verify(mockRegionFactory).setMulticastEnabled(eq(true));
		verify(mockRegionFactory, never()).setPartitionAttributes(any(PartitionAttributes.class));
		verify(mockRegionFactory).setPoolName(eq("swimming"));
		verify(mockRegionFactory).setRegionIdleTimeout(same(testExpirationAttributes));
		verify(mockRegionFactory).setRegionTimeToLive(same(testExpirationAttributes));
		verify(mockRegionFactory).setStatisticsEnabled(eq(true));
		verify(mockRegionFactory).setSubscriptionAttributes(null);
	}

	@Test
	public void testMergePartitionAttributesWithPartitionRedundantProxy() throws Exception {
		PartitionAttributes testPartitionAttributes = createPartitionAttributes("TestRegion", 512000, 15000l, 0,
			30000l, 1024000l, 51);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		RegionFactory mockRegionFactory = createTestRegionFactory();

		when(mockRegionAttributes.getPartitionAttributes()).thenReturn(testPartitionAttributes);

		factoryBean.setShortcut(RegionShortcut.PARTITION_PROXY_REDUNDANT);
		factoryBean.mergePartitionAttributes(mockRegionFactory, mockRegionAttributes);

		RegionAttributes regionAttributes = TestUtils.readField("regionAttributes",
			TestUtils.readField("attrsFactory", mockRegionFactory));
		PartitionAttributes actualPartitionAttributes = regionAttributes.getPartitionAttributes();

		assertNotNull(actualPartitionAttributes);
		assertNotSame(testPartitionAttributes, actualPartitionAttributes);
		assertEquals("TestRegion", actualPartitionAttributes.getColocatedWith());
		assertEquals(0, actualPartitionAttributes.getLocalMaxMemory());
		assertEquals(15000l, actualPartitionAttributes.getRecoveryDelay());
		assertEquals(1, actualPartitionAttributes.getRedundantCopies());
		assertEquals(30000l, actualPartitionAttributes.getStartupRecoveryDelay());
		assertEquals(1024000l, actualPartitionAttributes.getTotalMaxMemory());
		assertEquals(51, actualPartitionAttributes.getTotalNumBuckets());

		verify(mockRegionAttributes, times(2)).getPartitionAttributes();
	}

	@Test
	public void testMergePartitionAttributesWithPartitionRedundant() throws Exception {
		PartitionAttributes testPartitionAttributes = createPartitionAttributes("TestRegion", 512000, 15000l, 0,
			30000l, 1024000l, 51);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		RegionFactory mockRegionFactory = createTestRegionFactory();

		when(mockRegionAttributes.getPartitionAttributes()).thenReturn(testPartitionAttributes);

		factoryBean.setShortcut(RegionShortcut.PARTITION_REDUNDANT);
		factoryBean.mergePartitionAttributes(mockRegionFactory, mockRegionAttributes);

		RegionAttributes regionAttributes = TestUtils.readField("regionAttributes",
			TestUtils.readField("attrsFactory", mockRegionFactory));
		PartitionAttributes actualPartitionAttributes = regionAttributes.getPartitionAttributes();

		assertNotNull(actualPartitionAttributes);
		assertNotSame(testPartitionAttributes, actualPartitionAttributes);
		assertEquals("TestRegion", actualPartitionAttributes.getColocatedWith());
		assertEquals(512000, actualPartitionAttributes.getLocalMaxMemory());
		assertEquals(15000l, actualPartitionAttributes.getRecoveryDelay());
		assertEquals(1, actualPartitionAttributes.getRedundantCopies());
		assertEquals(30000l, actualPartitionAttributes.getStartupRecoveryDelay());
		assertEquals(1024000l, actualPartitionAttributes.getTotalMaxMemory());
		assertEquals(51, actualPartitionAttributes.getTotalNumBuckets());

		verify(mockRegionAttributes, times(2)).getPartitionAttributes();
	}

	@Test
	public void testMergePartitionAttributesWithPartitionRedundantPersistentOverflow() throws Exception {
		PartitionAttributes testPartitionAttributes = createPartitionAttributes("TestRegion", 512000, 15000l, 3,
			30000l, 1024000l, 51);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		RegionFactory mockRegionFactory = createTestRegionFactory();

		when(mockRegionAttributes.getPartitionAttributes()).thenReturn(testPartitionAttributes);

		factoryBean.setShortcut(RegionShortcut.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW);
		factoryBean.mergePartitionAttributes(mockRegionFactory, mockRegionAttributes);

		RegionAttributes regionAttributes = TestUtils.readField("regionAttributes",
			TestUtils.readField("attrsFactory", mockRegionFactory));
		PartitionAttributes actualPartitionAttributes = regionAttributes.getPartitionAttributes();

		assertNotNull(actualPartitionAttributes);
		assertNotSame(testPartitionAttributes, actualPartitionAttributes);
		assertEquals("TestRegion", actualPartitionAttributes.getColocatedWith());
		assertEquals(512000, actualPartitionAttributes.getLocalMaxMemory());
		assertEquals(15000l, actualPartitionAttributes.getRecoveryDelay());
		assertEquals(3, actualPartitionAttributes.getRedundantCopies());
		assertEquals(30000l, actualPartitionAttributes.getStartupRecoveryDelay());
		assertEquals(1024000l, actualPartitionAttributes.getTotalMaxMemory());
		assertEquals(51, actualPartitionAttributes.getTotalNumBuckets());

		verify(mockRegionAttributes, times(2)).getPartitionAttributes();
	}

	@Test
	public void testMergePartitionAttributesWithPartitionProxy() throws Exception {
		PartitionAttributes testPartitionAttributes = createPartitionAttributes("TestRegion", 512000, 15000l, 0,
			30000l, 1024000l, 51);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		RegionFactory mockRegionFactory = createTestRegionFactory();

		when(mockRegionAttributes.getPartitionAttributes()).thenReturn(testPartitionAttributes);

		factoryBean.setShortcut(RegionShortcut.PARTITION_PROXY);
		factoryBean.mergePartitionAttributes(mockRegionFactory, mockRegionAttributes);

		RegionAttributes regionAttributes = TestUtils.readField("regionAttributes",
			TestUtils.readField("attrsFactory", mockRegionFactory));
		PartitionAttributes actualPartitionAttributes = regionAttributes.getPartitionAttributes();

		assertNotNull(actualPartitionAttributes);
		assertNotSame(testPartitionAttributes, actualPartitionAttributes);
		assertEquals("TestRegion", actualPartitionAttributes.getColocatedWith());
		assertEquals(0, actualPartitionAttributes.getLocalMaxMemory());
		assertEquals(15000l, actualPartitionAttributes.getRecoveryDelay());
		assertEquals(0, actualPartitionAttributes.getRedundantCopies());
		assertEquals(30000l, actualPartitionAttributes.getStartupRecoveryDelay());
		assertEquals(1024000l, actualPartitionAttributes.getTotalMaxMemory());
		assertEquals(51, actualPartitionAttributes.getTotalNumBuckets());

		verify(mockRegionAttributes, times(2)).getPartitionAttributes();
	}

	@Test
	public void testMergePartitionAttributesWithPartition() throws Exception {
		PartitionAttributes testPartitionAttributes = createPartitionAttributes("TestRegion", 512000, 15000l, 0,
			30000l, 1024000l, 51);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		RegionFactory mockRegionFactory = createTestRegionFactory();

		when(mockRegionAttributes.getPartitionAttributes()).thenReturn(testPartitionAttributes);

		factoryBean.setShortcut(RegionShortcut.PARTITION);
		factoryBean.mergePartitionAttributes(mockRegionFactory, mockRegionAttributes);

		RegionAttributes regionAttributes = TestUtils.readField("regionAttributes",
			TestUtils.readField("attrsFactory", mockRegionFactory));
		PartitionAttributes actualPartitionAttributes = regionAttributes.getPartitionAttributes();

		assertNotNull(actualPartitionAttributes);
		assertNotSame(testPartitionAttributes, actualPartitionAttributes);
		assertEquals("TestRegion", actualPartitionAttributes.getColocatedWith());
		assertEquals(512000, actualPartitionAttributes.getLocalMaxMemory());
		assertEquals(15000l, actualPartitionAttributes.getRecoveryDelay());
		assertEquals(0, actualPartitionAttributes.getRedundantCopies());
		assertEquals(30000l, actualPartitionAttributes.getStartupRecoveryDelay());
		assertEquals(1024000l, actualPartitionAttributes.getTotalMaxMemory());
		assertEquals(51, actualPartitionAttributes.getTotalNumBuckets());

		verify(mockRegionAttributes, times(2)).getPartitionAttributes();
	}

	@Test
	public void testMergePartitionAttributes() {
		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);
		RegionFactory mockRegionFactory = createMockRegionFactory();

		when(mockRegionAttributes.getPartitionAttributes()).thenReturn(null);

		factoryBean.setShortcut(null);
		factoryBean.mergePartitionAttributes(mockRegionFactory, mockRegionAttributes);

		verify(mockRegionAttributes, times(1)).getPartitionAttributes();
		verify(mockRegionFactory, never()).setPartitionAttributes(any(PartitionAttributes.class));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, (String) null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.DEFAULT));
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, (String) null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.DEFAULT));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndDataPolicyUnspecified() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, (String) null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithBlankDataPolicyName() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, "  ");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy '  ' is invalid.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithEmptyDataPolicyName() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, "");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy '' is invalid.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWithInvalidDataPolicyName() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, "CSV");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'CSV' is invalid.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndNormalDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, "NORMAL");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.NORMAL));
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndPreloadedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, "PRELOADED");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PRELOADED));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenPersistentAndEmptyDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, "EMPTY");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'EMPTY' is invalid when persistent is true.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.EMPTY));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, "PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PARTITION));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndPersistentPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, "PERSISTENT_PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenNotPersistentAndPersistentPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(false);
			factoryBean.resolveDataPolicy(mockRegionFactory, false, "PERSISTENT_PARTITION");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'PERSISTENT_PARTITION' is invalid when persistent is false.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenPersistentAndPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, "PARTITION");
			fail(
				"Setting the 'persistent' attribute to TRUE and 'Data Policy' to PARTITION should have thrown an IllegalArgumentException!");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'PARTITION' is invalid when persistent is true.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PARTITION));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, "PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PARTITION));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndPersistentPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, "PERSISTENT_PARTITION");
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndRegionAttributesPreloadedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setAttributes(createMockRegionAttributes(DataPolicy.PRELOADED));
		factoryBean.setDataPolicy((DataPolicy) null);
		factoryBean.resolveDataPolicy(mockRegionFactory, null, (String) null);
		verify(mockRegionFactory, times(1)).setDataPolicy(eq(DataPolicy.PRELOADED));
		assertEquals(DataPolicy.PRELOADED, factoryBean.getDataPolicy());
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndRegionAttributesPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setAttributes(createMockRegionAttributes(DataPolicy.PARTITION));
		factoryBean.setDataPolicy((DataPolicy) null);
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, (String) null);
		verify(mockRegionFactory, times(1)).setDataPolicy(eq(DataPolicy.PARTITION));
		assertEquals(DataPolicy.PARTITION, factoryBean.getDataPolicy());
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndRegionAttributesPersistentPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setAttributes(createMockRegionAttributes(DataPolicy.PERSISTENT_PARTITION));
		factoryBean.setDataPolicy((DataPolicy) null);
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, (String) null);
		verify(mockRegionFactory, times(1)).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
		assertEquals(DataPolicy.PERSISTENT_PARTITION, factoryBean.getDataPolicy());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenNotPersistentAndRegionAttributesPersistentPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setAttributes(createMockRegionAttributes(DataPolicy.PERSISTENT_PARTITION));
			factoryBean.setPersistent(false);
			factoryBean.resolveDataPolicy(mockRegionFactory, false, (String) null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'PERSISTENT_PARTITION' is invalid when persistent is false.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_PARTITION));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenPersistentAndRegionAttributesPartitionDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setAttributes(createMockRegionAttributes(DataPolicy.PARTITION));
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, (String) null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'PARTITION' is invalid when persistent is true.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PARTITION));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
		}
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndUnspecifiedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setAttributes(createMockRegionAttributes());
		factoryBean.setPersistent(null);
		factoryBean.resolveDataPolicy(mockRegionFactory, null, (DataPolicy) null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.DEFAULT));
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndUnspecifiedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setAttributes(createMockRegionAttributes());
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, (DataPolicy) null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.DEFAULT));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndUnspecifiedDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setAttributes(createMockRegionAttributes());
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, (DataPolicy) null);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndReplicateDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, DataPolicy.REPLICATE);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.REPLICATE));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentUnspecifiedAndPersistentReplicateDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.resolveDataPolicy(mockRegionFactory, null, DataPolicy.PERSISTENT_REPLICATE);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenNotPersistentAndPersistentReplicateDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(false);
			factoryBean.resolveDataPolicy(mockRegionFactory, false, DataPolicy.PERSISTENT_REPLICATE);
			fail("Setting the 'persistent' attribute to FALSE and 'Data Policy' to PERSISTENT_REPLICATE should have thrown an IllegalArgumentException!");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'PERSISTENT_REPLICATE' is invalid when persistent is false.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.REPLICATE));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveDataPolicyWhenPersistentAndReplicateDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();

		try {
			factoryBean.setPersistent(true);
			factoryBean.resolveDataPolicy(mockRegionFactory, true, "REPLICATE");
			fail("Setting the 'persistent' attribute to TRUE and 'Data Policy' to REPLICATE should have thrown an IllegalArgumentException!");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy 'REPLICATE' is invalid when persistent is true.", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockRegionFactory, never()).setDataPolicy(null);
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.DEFAULT));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
			verify(mockRegionFactory, never()).setDataPolicy(eq(DataPolicy.REPLICATE));
		}
	}

	@Test
	public void testResolveDataPolicyWhenNotPersistentAndReplicateDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(false);
		factoryBean.resolveDataPolicy(mockRegionFactory, false, DataPolicy.REPLICATE);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.REPLICATE));
	}

	@Test
	public void testResolveDataPolicyWhenPersistentAndPersistentReplicateDataPolicy() {
		RegionFactory mockRegionFactory = createMockRegionFactory();
		factoryBean.setPersistent(true);
		factoryBean.resolveDataPolicy(mockRegionFactory, true, DataPolicy.PERSISTENT_REPLICATE);
		verify(mockRegionFactory).setDataPolicy(eq(DataPolicy.PERSISTENT_REPLICATE));
	}

	protected static class TestRegionFactory extends RegionFactory {

		protected TestRegionFactory() {
			super((GemFireCacheImpl) null);
		}
	}

	protected static class TestRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {
	}

}
