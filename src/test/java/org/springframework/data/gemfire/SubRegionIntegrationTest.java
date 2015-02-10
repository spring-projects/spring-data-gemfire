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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.InterestPolicy;
import com.gemstone.gemfire.cache.LossAction;
import com.gemstone.gemfire.cache.MembershipAttributes;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.ResumptionAction;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.SubscriptionAttributes;

/**
 * The SubRegionIntegrationTest class is a test suite of test cases testing the functionality of SubRegions in GemFire
 * configured with Spring Data GemFire's XML namespace configuration meta-data.  This test class tests a complex
 * SubRegion configuration in order to ensure functional completeness.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.4.0
 * @since 7.0.1 (GemFire)
 */
@ContextConfiguration(value = "complex-subregion.xml", initializers = GemfireTestApplicationContextInitializer.class)
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class SubRegionIntegrationTest {

	@Autowired
	private Cache cache;

	@Resource(name = "Customers")
	private Region customers;

	@Resource(name = "/Customers/Accounts")
	private Region accounts;

	@Test
	public void testGemFireAccountsSubRegionCreation() {
		assertNotNull("The GemFire Cache was not properly initialized!", cache);

		Region customers = cache.getRegion("Customers");

		assertNotNull(customers);
		assertEquals("Customers", customers.getName());
		assertEquals("/Customers", customers.getFullPath());

		Region accounts = customers.getSubregion("Accounts");

		assertNotNull(accounts);
		assertEquals("Accounts", accounts.getName());
		assertEquals("/Customers/Accounts", accounts.getFullPath());

		Region cacheAccounts = cache.getRegion("/Customers/Accounts");

		assertSame(accounts, cacheAccounts);
	}

	@Test
	public void testSpringSubRegionConfiguration() {
		assertNotNull("The /Customers/Accounts SubRegion was not properly initialized!", accounts);
		assertEquals("Accounts", accounts.getName());
		assertEquals("/Customers/Accounts", accounts.getFullPath());

		RegionAttributes regionAttributes = accounts.getAttributes();

		assertNotNull(regionAttributes);
		assertEquals(DataPolicy.PERSISTENT_REPLICATE, regionAttributes.getDataPolicy());
		assertEquals(20, regionAttributes.getConcurrencyLevel());
		assertTrue(regionAttributes.isDiskSynchronous());
		assertTrue(regionAttributes.getIgnoreJTA());
		assertFalse(regionAttributes.getIndexMaintenanceSynchronous());
		assertEquals(1000, regionAttributes.getInitialCapacity());
		assertEquals(Long.class, regionAttributes.getKeyConstraint());
		assertEquals(Scope.DISTRIBUTED_ACK, regionAttributes.getScope());
		assertTrue(regionAttributes.getStatisticsEnabled());
		assertEquals(String.class, regionAttributes.getValueConstraint());
		assertNotNull(regionAttributes.getCacheListeners());
		assertEquals(1, regionAttributes.getCacheListeners().length);
		assertTrue(regionAttributes.getCacheListeners()[0] instanceof SimpleCacheListener);
		assertTrue(regionAttributes.getCacheLoader() instanceof SimpleCacheLoader);
		assertTrue(regionAttributes.getCacheWriter() instanceof SimpleCacheWriter);

		EvictionAttributes evictionAttributes = regionAttributes.getEvictionAttributes();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, evictionAttributes.getAction());
		assertEquals(10000, evictionAttributes.getMaximum());

		MembershipAttributes membershipAttributes = regionAttributes.getMembershipAttributes();

		assertNotNull(membershipAttributes);
		assertNotNull(membershipAttributes.getRequiredRoles());
		assertEquals(1, membershipAttributes.getRequiredRoles().size());
		assertTrue(membershipAttributes.getRequiredRoles().iterator().next().getName().equalsIgnoreCase("TEST"));
		assertEquals(LossAction.LIMITED_ACCESS, membershipAttributes.getLossAction());
		assertEquals(ResumptionAction.REINITIALIZE, membershipAttributes.getResumptionAction());

		SubscriptionAttributes subscriptionAttributes = regionAttributes.getSubscriptionAttributes();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.CACHE_CONTENT, subscriptionAttributes.getInterestPolicy());
	}

}
