/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.Assume.assumeNotNull;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EntryOperation;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.EvictionAlgorithm;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.InterestPolicy;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.LossAction;
import org.apache.geode.cache.MembershipAttributes;
import org.apache.geode.cache.PartitionResolver;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.ResumptionAction;
import org.apache.geode.cache.Scope;
import org.apache.geode.cache.SubscriptionAttributes;
import org.apache.geode.cache.asyncqueue.AsyncEvent;
import org.apache.geode.cache.asyncqueue.AsyncEventListener;
import org.apache.geode.cache.partition.PartitionListener;
import org.apache.geode.cache.partition.PartitionListenerAdapter;
import org.apache.geode.cache.util.CacheListenerAdapter;
import org.apache.geode.cache.util.CacheWriterAdapter;
import org.apache.geode.cache.util.ObjectSizer;
import org.apache.geode.distributed.Role;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanIsAbstractException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

/**
 * The TemplateRegionsNamespaceTests class is a test suite of test cases testing the functionality and support for
 * Region Templating in SDG.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.Region
 * @since 1.5.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class TemplateRegionsNamespaceTests {

	@Autowired
	private ApplicationContext applicationContext;

	@Resource(name = "NonTemplateBasedReplicateRegion")
	private Region<Integer, String> nonTemplateBasedReplicateRegion;

	@Resource(name = "TemplateBasedReplicateRegion")
	private Region<String, Object> templateBasedReplicateRegion;

	@Resource(name = "/TemplateBasedReplicateRegion/TemplateBasedReplicateSubRegion")
	private Region<Integer, String> templateBasedReplicateSubRegion;

	@Resource(name = "TemplateBasedReplicateRegionNoOverrides")
	private Region<String, Object> templateBasedReplicateRegionNoOverrides;

	@Resource(name = "TemplateBasedPartitionRegion")
	private Region<Date, Object> templateBasedPartitionRegion;

	@Resource(name = "TemplateBasedLocalRegion")
	private Region<Long, String> templateBasedLocalRegion;

	private void assertAsyncEventQueues(Region<?, ?> region, String... expectedNames) {

		assertNotNull(region);
		assertNotNull(region.getAttributes());
		assertNotNull(region.getAttributes().getAsyncEventQueueIds());
		assertEquals(expectedNames.length, region.getAttributes().getAsyncEventQueueIds().size());

		for (String asyncEventQueueId : region.getAttributes().getAsyncEventQueueIds()) {
			assertTrue(Arrays.asList(expectedNames).contains(asyncEventQueueId));
		}
	}

	private void assertCacheListeners(Region<?, ?> region, String... expectedNames) {

		assertNotNull(region);
		assertNotNull(region.getAttributes());
		assertNotNull(region.getAttributes().getCacheListeners());
		assertEquals(expectedNames.length, region.getAttributes().getCacheListeners().length);

		for (CacheListener cacheListener : region.getAttributes().getCacheListeners()) {
			assertTrue(cacheListener instanceof TestCacheListener);
			assertTrue(Arrays.asList(expectedNames).contains(cacheListener.toString()));
		}
	}

	private void assertCacheLoader(Region<?, ?> region, String expectedName) {

		assertNotNull(region);
		assertNotNull(region.getAttributes());
		assertTrue(region.getAttributes().getCacheLoader() instanceof TestCacheLoader);
		assertEquals(expectedName, region.getAttributes().getCacheLoader().toString());
	}

	private void assertCacheWriter(Region<?, ?> region, String expectedName) {

		assertNotNull(region);
		assertNotNull(region.getAttributes());
		assertTrue(region.getAttributes().getCacheWriter() instanceof TestCacheWriter);
		assertEquals(expectedName, region.getAttributes().getCacheWriter().toString());
	}

	private void assertDefaultEvictionAttributes(EvictionAttributes evictionAttributes) {
		assumeNotNull(evictionAttributes);
		assertEvictionAttributes(evictionAttributes, EvictionAction.NONE, EvictionAlgorithm.NONE, 0, null);
	}

	private void assertEvictionAttributes(EvictionAttributes evictionAttributes, EvictionAction expectedAction,
			EvictionAlgorithm expectedAlgorithm, int expectedMaximum, ObjectSizer expectedObjectSizer) {

		assertNotNull("The 'EvictionAttributes' must not be null!", evictionAttributes);
		assertEquals(expectedAction, evictionAttributes.getAction());
		assertEquals(expectedAlgorithm, evictionAttributes.getAlgorithm());
		assertEquals(expectedMaximum, evictionAttributes.getMaximum());
		assertEquals(expectedObjectSizer, evictionAttributes.getObjectSizer());
	}

	private void assertDefaultExpirationAttributes(ExpirationAttributes expirationAttributes) {

		assumeNotNull(expirationAttributes);
		assertEquals(ExpirationAction.INVALIDATE, expirationAttributes.getAction());
		assertEquals(0, expirationAttributes.getTimeout());
	}

	private void assertExpirationAttributes(ExpirationAttributes expirationAttributes, ExpirationAction expectedAction,
			int expectedTimeout) {

		assertNotNull("The 'ExpirationAttributes' must not be null!", expirationAttributes);
		assertEquals(expectedAction, expirationAttributes.getAction());
		assertEquals(expectedTimeout, expirationAttributes.getTimeout());
	}

	private void assertGatewaySenders(Region<?, ?> region, String... gatewaySenderIds) {

		assertNotNull(region);
		assertNotNull(region.getAttributes());
		assertNotNull(region.getAttributes().getGatewaySenderIds());
		assertEquals(gatewaySenderIds.length, region.getAttributes().getGatewaySenderIds().size());

		for (String gatewaySenderId : region.getAttributes().getGatewaySenderIds()) {
			assertTrue(Arrays.asList(gatewaySenderIds).contains(gatewaySenderId));
		}
	}

	private void assertDefaultMembershipAttributes(MembershipAttributes membershipAttributes) {

		assumeNotNull(membershipAttributes);
		assertMembershipAttributes(membershipAttributes, LossAction.FULL_ACCESS, ResumptionAction.NONE);
	}

	private void assertMembershipAttributes(MembershipAttributes membershipAttributes, LossAction expectedLossAction,
			ResumptionAction expectedResumptionAction, String... expectedRequiredRoles) {

		assertNotNull("The 'MembershipAttributes' must not be null!", membershipAttributes);
		assertEquals(expectedLossAction, membershipAttributes.getLossAction());
		assertEquals(expectedResumptionAction, membershipAttributes.getResumptionAction());

		if (!ObjectUtils.isEmpty(expectedRequiredRoles)) {
			for (Role membershipRole : membershipAttributes.getRequiredRoles()) {
				assertTrue(String.format("Role '%1$s' was not found!", membershipRole),
					Arrays.asList(expectedRequiredRoles).contains(membershipRole.getName()));
			}
		}
	}

	private void assertPartitionListener(Region<?, ?> region, String... expectedNames) {

		assertNotNull(region);
		assertNotNull(region.getAttributes());
		assertNotNull(region.getAttributes().getPartitionAttributes());
		assertNotNull(region.getAttributes().getPartitionAttributes().getPartitionListeners());
		assertEquals(expectedNames.length, region.getAttributes().getPartitionAttributes().getPartitionListeners().length);

		for (PartitionListener partitionListener : region.getAttributes().getPartitionAttributes().getPartitionListeners()) {
			assertTrue(partitionListener instanceof TestPartitionListener);
			assertTrue(Arrays.asList(expectedNames).contains(partitionListener.toString()));
		}
	}

	private void assertPartitionResolver(Region<?, ?> region, String expectedName) {

		assertNotNull(region);
		assertNotNull(region.getAttributes());
		assertNotNull(region.getAttributes().getPartitionAttributes());
		assertTrue(region.getAttributes().getPartitionAttributes().getPartitionResolver() instanceof TestPartitionResolver);
		assertEquals(expectedName, region.getAttributes().getPartitionAttributes().getPartitionResolver().toString());
	}

	private void assertDefaultRegionAttributes(Region region) {

		assertNotNull("The Region must not be null!", region);
		assertNotNull(String.format("Region (%1$s) must have 'RegionAttributes' defined!",
			region.getFullPath()), region.getAttributes());
		assertNull(region.getAttributes().getCompressor());
		assertNull(region.getAttributes().getCustomEntryIdleTimeout());
		assertNull(region.getAttributes().getCustomEntryTimeToLive());
		assertNull(region.getAttributes().getDiskStoreName());
		assertFalse(region.getAttributes().getMulticastEnabled());
		assertNullEmpty(region.getAttributes().getPoolName());
		assertDefaultExpirationAttributes(region.getAttributes().getRegionTimeToLive());
		assertDefaultExpirationAttributes(region.getAttributes().getRegionIdleTimeout());
	}

	private void assertDefaultSubscriptionAttributes(SubscriptionAttributes subscriptionAttributes) {

		assumeNotNull(subscriptionAttributes);
		assertSubscriptionAttributes(subscriptionAttributes, InterestPolicy.DEFAULT);
	}

	private void assertSubscriptionAttributes(SubscriptionAttributes subscriptionAttributes,
			InterestPolicy expectedInterestPolicy) {

		assertNotNull("The 'SubscriptionAttributes' must not be null!", subscriptionAttributes);
		assertEquals(expectedInterestPolicy, subscriptionAttributes.getInterestPolicy());
	}

	private static void assertEmpty(Object[] array) {
		assertTrue((array == null || array.length == 0));
	}

	private static void assertEmpty(Iterable<?> collection) {
		assertTrue(collection == null || !collection.iterator().hasNext());
	}

	private static void assertNullEmpty(String value) {
		assertFalse(StringUtils.hasText(value));
	}

	private static void assertRegionMetaData(Region<?, ?> region, String expectedRegionName) {
		assertRegionMetaData(region, expectedRegionName, Region.SEPARATOR + expectedRegionName);
	}

	private static void assertRegionMetaData(Region<?, ?> region, String expectedRegionName, String expectedRegionPath) {

		assertNotNull(String.format("The '%1$s' Region was not properly configured and initialized!",
			expectedRegionName), region);
		assertEquals(expectedRegionName, region.getName());
		assertEquals(expectedRegionPath, region.getFullPath());
		assertNotNull(String.format("The '%1$s' Region must have RegionAttributes defined!",
			expectedRegionName), region.getAttributes());
	}

	@Test
	public void testNoAbstractRegionTemplateBeans() {

		String[] beanNames = {
			"BaseRegionTemplate",
			"ExtendedRegionTemplate",
			"ReplicateRegionTemplate",
			"PartitionRegionTemplate",
			"LocalRegionTemplate"
		};

		for (String beanName : beanNames) {
			assertTrue(applicationContext.containsBean(beanName));
			assertTrue(applicationContext.containsBeanDefinition(beanName));

			try {
				applicationContext.getBean(beanName);
				fail(String.format("The abstract bean definition '%1$s' should not exist as a bean in the Spring context!",
					beanName));
			}
			catch (BeansException ignore) {
				assertTrue(ignore instanceof BeanIsAbstractException);
				assertTrue(ignore.getMessage().contains(beanName));
			}
		}
	}

	@Test
	public void testNonTemplateBasedReplicateRegion() {

		assertRegionMetaData(nonTemplateBasedReplicateRegion, "NonTemplateBasedReplicateRegion");
		assertDefaultRegionAttributes(nonTemplateBasedReplicateRegion);
		assertEmpty(nonTemplateBasedReplicateRegion.getAttributes().getAsyncEventQueueIds());
		assertEmpty(nonTemplateBasedReplicateRegion.getAttributes().getCacheListeners());
		assertCacheLoader(nonTemplateBasedReplicateRegion, "ABC");
		assertCacheWriter(nonTemplateBasedReplicateRegion, "DEF");
		assertFalse(nonTemplateBasedReplicateRegion.getAttributes().getCloningEnabled());
		assertTrue(nonTemplateBasedReplicateRegion.getAttributes().getConcurrencyChecksEnabled());
		assertEquals(12, nonTemplateBasedReplicateRegion.getAttributes().getConcurrencyLevel());
		assertEquals(DataPolicy.REPLICATE, nonTemplateBasedReplicateRegion.getAttributes().getDataPolicy());
		assertTrue(nonTemplateBasedReplicateRegion.getAttributes().isDiskSynchronous());
		assertFalse(nonTemplateBasedReplicateRegion.getAttributes().getEnableAsyncConflation());
		assertFalse(nonTemplateBasedReplicateRegion.getAttributes().getEnableSubscriptionConflation());
		assertDefaultEvictionAttributes(nonTemplateBasedReplicateRegion.getAttributes().getEvictionAttributes());
		assertDefaultExpirationAttributes(nonTemplateBasedReplicateRegion.getAttributes().getEntryIdleTimeout());
		assertDefaultExpirationAttributes(nonTemplateBasedReplicateRegion.getAttributes().getEntryTimeToLive());
		assertEmpty(nonTemplateBasedReplicateRegion.getAttributes().getGatewaySenderIds());
		assertFalse(nonTemplateBasedReplicateRegion.getAttributes().getIgnoreJTA());
		assertTrue(nonTemplateBasedReplicateRegion.getAttributes().getIndexMaintenanceSynchronous());
		assertEquals(97, nonTemplateBasedReplicateRegion.getAttributes().getInitialCapacity());
		assertNull(nonTemplateBasedReplicateRegion.getAttributes().getKeyConstraint());
		assertEquals("0.65", String.valueOf(nonTemplateBasedReplicateRegion.getAttributes().getLoadFactor()));
		assertFalse(nonTemplateBasedReplicateRegion.getAttributes().isLockGrantor());
		assertDefaultMembershipAttributes(nonTemplateBasedReplicateRegion.getAttributes().getMembershipAttributes());
		assertNull(nonTemplateBasedReplicateRegion.getAttributes().getPartitionAttributes());
		assertEquals(Scope.DISTRIBUTED_NO_ACK, nonTemplateBasedReplicateRegion.getAttributes().getScope());
		assertFalse(nonTemplateBasedReplicateRegion.getAttributes().getStatisticsEnabled());
		assertDefaultSubscriptionAttributes(nonTemplateBasedReplicateRegion.getAttributes().getSubscriptionAttributes());
		assertNull(nonTemplateBasedReplicateRegion.getAttributes().getValueConstraint());
	}

	@Test
	public void testTemplateBasedReplicateRegion() {

		assertRegionMetaData(templateBasedReplicateRegion, "TemplateBasedReplicateRegion");
		assertDefaultRegionAttributes(templateBasedReplicateRegion);
		assertEmpty(templateBasedReplicateRegion.getAttributes().getAsyncEventQueueIds());
		assertCacheListeners(templateBasedReplicateRegion, "XYZ");
		assertCacheLoader(templateBasedReplicateRegion, "dbLoader");
		assertCacheWriter(templateBasedReplicateRegion, "dbWriter");
		assertTrue(templateBasedReplicateRegion.getAttributes().getCloningEnabled());
		assertTrue(templateBasedReplicateRegion.getAttributes().getConcurrencyChecksEnabled());
		assertEquals(24, templateBasedReplicateRegion.getAttributes().getConcurrencyLevel());
		assertEquals(DataPolicy.REPLICATE, templateBasedReplicateRegion.getAttributes().getDataPolicy());
		assertFalse(templateBasedReplicateRegion.getAttributes().isDiskSynchronous());
		assertFalse(templateBasedReplicateRegion.getAttributes().getEnableAsyncConflation());
		assertTrue(templateBasedReplicateRegion.getAttributes().getEnableSubscriptionConflation());
		assertEvictionAttributes(templateBasedReplicateRegion.getAttributes().getEvictionAttributes(),
			EvictionAction.OVERFLOW_TO_DISK, EvictionAlgorithm.LRU_ENTRY, 2024, null);
		assertExpirationAttributes(templateBasedReplicateRegion.getAttributes().getEntryIdleTimeout(),
			ExpirationAction.DESTROY, 600);
		assertExpirationAttributes(templateBasedReplicateRegion.getAttributes().getEntryTimeToLive(),
			ExpirationAction.INVALIDATE, 300);
		assertEmpty(templateBasedReplicateRegion.getAttributes().getGatewaySenderIds());
		assertTrue(templateBasedReplicateRegion.getAttributes().getIgnoreJTA());
		assertTrue(templateBasedReplicateRegion.getAttributes().getIndexMaintenanceSynchronous());
		assertEquals(51, templateBasedReplicateRegion.getAttributes().getInitialCapacity());
		assertEquals(String.class, templateBasedReplicateRegion.getAttributes().getKeyConstraint());
		assertEquals("0.85", String.valueOf(templateBasedReplicateRegion.getAttributes().getLoadFactor()));
		assertTrue(templateBasedReplicateRegion.getAttributes().isLockGrantor());
		assertDefaultMembershipAttributes(templateBasedReplicateRegion.getAttributes().getMembershipAttributes());
		assertNull(templateBasedReplicateRegion.getAttributes().getPartitionAttributes());
		assertEquals(Scope.GLOBAL, templateBasedReplicateRegion.getAttributes().getScope());
		assertTrue(templateBasedReplicateRegion.getAttributes().getStatisticsEnabled());
		assertSubscriptionAttributes(templateBasedReplicateRegion.getAttributes().getSubscriptionAttributes(),
			InterestPolicy.CACHE_CONTENT);
		assertEquals(Object.class, templateBasedReplicateRegion.getAttributes().getValueConstraint());
	}

	@Test
	public void testTemplateBasedReplicateSubRegion() {

		assertRegionMetaData(templateBasedReplicateSubRegion, "TemplateBasedReplicateSubRegion",
			"/TemplateBasedReplicateRegion/TemplateBasedReplicateSubRegion");
		assertDefaultRegionAttributes(templateBasedReplicateSubRegion);
		assertEmpty(templateBasedReplicateSubRegion.getAttributes().getAsyncEventQueueIds());
		assertCacheListeners(templateBasedReplicateSubRegion, "testListener");
		assertCacheLoader(templateBasedReplicateSubRegion, "A");
		assertCacheWriter(templateBasedReplicateSubRegion, "B");
		assertFalse(templateBasedReplicateSubRegion.getAttributes().getCloningEnabled());
		assertTrue(templateBasedReplicateSubRegion.getAttributes().getConcurrencyChecksEnabled());
		assertEquals(16, templateBasedReplicateSubRegion.getAttributes().getConcurrencyLevel());
		assertEquals(DataPolicy.REPLICATE, templateBasedReplicateSubRegion.getAttributes().getDataPolicy());
		assertFalse(templateBasedReplicateSubRegion.getAttributes().isDiskSynchronous());
		assertTrue(templateBasedReplicateSubRegion.getAttributes().getEnableAsyncConflation());
		assertFalse(templateBasedReplicateSubRegion.getAttributes().getEnableSubscriptionConflation());
		assertDefaultEvictionAttributes(templateBasedReplicateSubRegion.getAttributes().getEvictionAttributes());
		assertExpirationAttributes(templateBasedReplicateSubRegion.getAttributes().getEntryIdleTimeout(),
			ExpirationAction.DESTROY, 600);
		assertExpirationAttributes(templateBasedReplicateSubRegion.getAttributes().getEntryTimeToLive(),
			ExpirationAction.DESTROY, 600);
		assertEmpty(templateBasedReplicateSubRegion.getAttributes().getGatewaySenderIds());
		assertTrue(templateBasedReplicateSubRegion.getAttributes().getIgnoreJTA());
		assertFalse(templateBasedReplicateSubRegion.getAttributes().getIndexMaintenanceSynchronous());
		assertEquals(51, templateBasedReplicateSubRegion.getAttributes().getInitialCapacity());
		assertEquals(Integer.class, templateBasedReplicateSubRegion.getAttributes().getKeyConstraint());
		assertEquals("0.95", String.valueOf(templateBasedReplicateSubRegion.getAttributes().getLoadFactor()));
		assertFalse(templateBasedReplicateSubRegion.getAttributes().isLockGrantor());
		assertMembershipAttributes(templateBasedReplicateSubRegion.getAttributes().getMembershipAttributes(),
			LossAction.LIMITED_ACCESS, ResumptionAction.NONE, "readWriteNode");
		assertNull(templateBasedReplicateSubRegion.getAttributes().getPartitionAttributes());
		assertEquals(Scope.DISTRIBUTED_NO_ACK, templateBasedReplicateSubRegion.getAttributes().getScope());
		assertTrue(templateBasedReplicateSubRegion.getAttributes().getStatisticsEnabled());
		assertDefaultSubscriptionAttributes(templateBasedReplicateSubRegion.getAttributes().getSubscriptionAttributes());
		assertEquals(String.class, templateBasedReplicateSubRegion.getAttributes().getValueConstraint());
	}

	@Test
	public void testTemplateBasedReplicateRegionNoOverrides() {

		assertRegionMetaData(templateBasedReplicateRegionNoOverrides, "TemplateBasedReplicateRegionNoOverrides");
		assertDefaultRegionAttributes(templateBasedReplicateRegionNoOverrides);
		assertEmpty(templateBasedReplicateRegionNoOverrides.getAttributes().getAsyncEventQueueIds());
		assertCacheListeners(templateBasedReplicateRegionNoOverrides, "XYZ");
		assertNull(templateBasedReplicateRegionNoOverrides.getAttributes().getCacheLoader());
		assertNull(templateBasedReplicateRegionNoOverrides.getAttributes().getCacheWriter());
		assertTrue(templateBasedReplicateRegionNoOverrides.getAttributes().getCloningEnabled());
		assertTrue(templateBasedReplicateRegionNoOverrides.getAttributes().getConcurrencyChecksEnabled());
		assertEquals(24, templateBasedReplicateRegionNoOverrides.getAttributes().getConcurrencyLevel());
		assertEquals(DataPolicy.PERSISTENT_REPLICATE, templateBasedReplicateRegionNoOverrides.getAttributes().getDataPolicy());
		assertFalse(templateBasedReplicateRegionNoOverrides.getAttributes().isDiskSynchronous());
		assertFalse(templateBasedReplicateRegionNoOverrides.getAttributes().getEnableAsyncConflation());
		assertTrue(templateBasedReplicateRegionNoOverrides.getAttributes().getEnableSubscriptionConflation());
		assertEvictionAttributes(templateBasedReplicateRegionNoOverrides.getAttributes().getEvictionAttributes(),
			EvictionAction.OVERFLOW_TO_DISK, EvictionAlgorithm.LRU_ENTRY, 2024, null);
		assertExpirationAttributes(templateBasedReplicateRegionNoOverrides.getAttributes().getEntryIdleTimeout(),
			ExpirationAction.DESTROY, 600);
		assertExpirationAttributes(templateBasedReplicateRegionNoOverrides.getAttributes().getEntryTimeToLive(),
			ExpirationAction.INVALIDATE, 300);
		assertEmpty(templateBasedReplicateRegionNoOverrides.getAttributes().getGatewaySenderIds());
		assertTrue(templateBasedReplicateRegionNoOverrides.getAttributes().getIgnoreJTA());
		assertTrue(templateBasedReplicateRegionNoOverrides.getAttributes().getIndexMaintenanceSynchronous());
		assertEquals(51, templateBasedReplicateRegionNoOverrides.getAttributes().getInitialCapacity());
		assertEquals(String.class, templateBasedReplicateRegionNoOverrides.getAttributes().getKeyConstraint());
		assertEquals(0.85f, templateBasedReplicateRegionNoOverrides.getAttributes().getLoadFactor(), 0.0f);
		assertFalse(templateBasedReplicateRegionNoOverrides.getAttributes().isLockGrantor());
		assertDefaultMembershipAttributes(templateBasedReplicateRegionNoOverrides.getAttributes().getMembershipAttributes());
		assertNull(templateBasedReplicateRegionNoOverrides.getAttributes().getPartitionAttributes());
		assertEquals(Scope.DISTRIBUTED_ACK, templateBasedReplicateRegionNoOverrides.getAttributes().getScope());
		assertTrue(templateBasedReplicateRegionNoOverrides.getAttributes().getStatisticsEnabled());
		assertSubscriptionAttributes(templateBasedReplicateRegionNoOverrides.getAttributes().getSubscriptionAttributes(),
			InterestPolicy.CACHE_CONTENT);
		assertEquals(Object.class, templateBasedReplicateRegionNoOverrides.getAttributes().getValueConstraint());
	}

	@Test
	public void testTemplateBasedPartitionRegion() {

		assertRegionMetaData(templateBasedPartitionRegion, "TemplateBasedPartitionRegion");
		assertDefaultRegionAttributes(templateBasedPartitionRegion);
		assertAsyncEventQueues(templateBasedPartitionRegion, "TestAsyncEventQueue");
		assertCacheListeners(templateBasedPartitionRegion, "X", "Y", "Z");
		assertCacheLoader(templateBasedPartitionRegion, "A");
		assertCacheWriter(templateBasedPartitionRegion, "dbWriter");
		assertFalse(templateBasedPartitionRegion.getAttributes().getCloningEnabled());
		assertTrue(templateBasedPartitionRegion.getAttributes().getConcurrencyChecksEnabled());
		assertEquals(DataPolicy.PERSISTENT_PARTITION, templateBasedPartitionRegion.getAttributes().getDataPolicy());
		assertTrue(templateBasedPartitionRegion.getAttributes().isDiskSynchronous());
		assertTrue(templateBasedPartitionRegion.getAttributes().getEnableAsyncConflation());
		assertTrue(templateBasedPartitionRegion.getAttributes().getEnableSubscriptionConflation());
		assertEvictionAttributes(templateBasedPartitionRegion.getAttributes().getEvictionAttributes(),
			EvictionAction.OVERFLOW_TO_DISK, EvictionAlgorithm.LRU_ENTRY, 8192000, null);
		assertExpirationAttributes(templateBasedPartitionRegion.getAttributes().getEntryIdleTimeout(),
			ExpirationAction.DESTROY, 600);
		assertExpirationAttributes(templateBasedPartitionRegion.getAttributes().getEntryTimeToLive(),
			ExpirationAction.INVALIDATE, 300);
		assertGatewaySenders(templateBasedPartitionRegion, "TestGatewaySender");
		assertFalse(templateBasedPartitionRegion.getAttributes().getIgnoreJTA());
		assertFalse(templateBasedPartitionRegion.getAttributes().getIndexMaintenanceSynchronous());
		assertEquals(51, templateBasedPartitionRegion.getAttributes().getInitialCapacity());
		assertEquals(Date.class, templateBasedPartitionRegion.getAttributes().getKeyConstraint());
		assertEquals("0.7", String.valueOf(templateBasedPartitionRegion.getAttributes().getLoadFactor()));
		assertFalse(templateBasedPartitionRegion.getAttributes().isLockGrantor());
		assertMembershipAttributes(templateBasedPartitionRegion.getAttributes().getMembershipAttributes(),
			LossAction.NO_ACCESS, ResumptionAction.REINITIALIZE, "admin", "root", "supertool");
		assertNotNull(templateBasedPartitionRegion.getAttributes().getPartitionAttributes());
		assertEquals("Neighbor",
			templateBasedPartitionRegion.getAttributes().getPartitionAttributes().getColocatedWith());
		assertEquals(8192, templateBasedPartitionRegion.getAttributes().getPartitionAttributes().getLocalMaxMemory());
		assertEquals(2, templateBasedPartitionRegion.getAttributes().getPartitionAttributes().getRedundantCopies());
		assertEquals(60000l, templateBasedPartitionRegion.getAttributes().getPartitionAttributes().getRecoveryDelay());
		assertEquals(15000l, templateBasedPartitionRegion.getAttributes().getPartitionAttributes().getStartupRecoveryDelay());
		assertEquals(16384, templateBasedPartitionRegion.getAttributes().getPartitionAttributes().getTotalMaxMemory());
		assertEquals(91, templateBasedPartitionRegion.getAttributes().getPartitionAttributes().getTotalNumBuckets());
		assertPartitionListener(templateBasedPartitionRegion, "testListener");
		assertPartitionResolver(templateBasedPartitionRegion, "testResolver");
		assertEquals(Scope.DISTRIBUTED_NO_ACK, templateBasedPartitionRegion.getAttributes().getScope());
		assertTrue(templateBasedPartitionRegion.getAttributes().getStatisticsEnabled());
		assertSubscriptionAttributes(templateBasedPartitionRegion.getAttributes().getSubscriptionAttributes(),
			InterestPolicy.ALL);
		assertEquals(Object.class, templateBasedPartitionRegion.getAttributes().getValueConstraint());
	}

	@Test
	public void testTemplateBasedLocalRegion() {

		assertRegionMetaData(templateBasedLocalRegion, "TemplateBasedLocalRegion");
		assertDefaultRegionAttributes(templateBasedLocalRegion);
		assertEmpty(templateBasedLocalRegion.getAttributes().getAsyncEventQueueIds());
		assertCacheListeners(templateBasedLocalRegion, "X", "Y", "Z");
		assertNull(templateBasedLocalRegion.getAttributes().getCacheLoader());
		assertNull(templateBasedLocalRegion.getAttributes().getCacheWriter());
		assertTrue(templateBasedLocalRegion.getAttributes().getCloningEnabled());
		assertFalse(templateBasedLocalRegion.getAttributes().getConcurrencyChecksEnabled());
		assertEquals(8, templateBasedLocalRegion.getAttributes().getConcurrencyLevel());
		assertEquals(DataPolicy.NORMAL, templateBasedLocalRegion.getAttributes().getDataPolicy());
		assertFalse(templateBasedLocalRegion.getAttributes().isDiskSynchronous());
		assertFalse(templateBasedLocalRegion.getAttributes().getEnableAsyncConflation());
		assertFalse(templateBasedLocalRegion.getAttributes().getEnableSubscriptionConflation());
		assertEvictionAttributes(templateBasedLocalRegion.getAttributes().getEvictionAttributes(),
			EvictionAction.LOCAL_DESTROY, EvictionAlgorithm.LRU_ENTRY, 4096, null);
		assertExpirationAttributes(templateBasedLocalRegion.getAttributes().getEntryIdleTimeout(),
			ExpirationAction.DESTROY, 600);
		assertExpirationAttributes(templateBasedLocalRegion.getAttributes().getEntryTimeToLive(),
			ExpirationAction.INVALIDATE, 300);
		assertEmpty(templateBasedLocalRegion.getAttributes().getGatewaySenderIds());
		assertTrue(templateBasedLocalRegion.getAttributes().getIgnoreJTA());
		assertTrue(templateBasedLocalRegion.getAttributes().getIndexMaintenanceSynchronous());
		assertEquals(51, templateBasedLocalRegion.getAttributes().getInitialCapacity());
		assertEquals(Long.class, templateBasedLocalRegion.getAttributes().getKeyConstraint());
		assertEquals("0.85", String.valueOf(templateBasedLocalRegion.getAttributes().getLoadFactor()));
		assertFalse(templateBasedLocalRegion.getAttributes().isLockGrantor());
		assertDefaultMembershipAttributes(templateBasedLocalRegion.getAttributes().getMembershipAttributes());
		assertNull(templateBasedLocalRegion.getAttributes().getPartitionAttributes());
		assertEquals(Scope.LOCAL, templateBasedLocalRegion.getAttributes().getScope());
		assertTrue(templateBasedLocalRegion.getAttributes().getStatisticsEnabled());
		assertDefaultSubscriptionAttributes(templateBasedLocalRegion.getAttributes().getSubscriptionAttributes());
		assertEquals(String.class, templateBasedLocalRegion.getAttributes().getValueConstraint());
	}

	public static final class TestAsyncEventListener implements AsyncEventListener {

		private String name;

		public void setName(String name) {
			this.name = name;
		}

		@Override
		public boolean processEvents(List<AsyncEvent> asyncEvents) {
			return false;
		}

		@Override
		public void close() { }

		@Override
		public String toString() {
			return name;
		}
	}

	public static final class TestCacheListener extends CacheListenerAdapter {

		private String name;

		public void setName(String name) {
			this.name = name;
		}

		@Override
		public String toString() {
			return name;
		}
	}

	public static final class TestCacheLoader implements CacheLoader {

		private String name;

		public void setName(String name) {
			this.name = name;
		}

		@Override
		public Object load(LoaderHelper loaderHelper) throws CacheLoaderException {
			return null;
		}

		@Override
		public void close() { }

		@Override
		public String toString() {
			return name;
		}
	}

	public static final class TestCacheWriter extends CacheWriterAdapter {

		private String name;

		public void setName(String name) {
			this.name = name;
		}

		@Override
		public String toString() {
			return name;
		}
	}

	public static final class TestPartitionListener extends PartitionListenerAdapter {

		private String name;

		public void setName(String name) {
			this.name = name;
		}

		@Override
		public String toString() {
			return name;
		}
	}

	public static final class TestPartitionResolver implements PartitionResolver {

		private String name;

		public void setName(String name) {
			this.name = name;
		}

		@Override
		public Object getRoutingObject(EntryOperation entryOperation) {
			return null;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public void close() { }

		@Override
		public String toString() {
			return name;
		}
	}
}
