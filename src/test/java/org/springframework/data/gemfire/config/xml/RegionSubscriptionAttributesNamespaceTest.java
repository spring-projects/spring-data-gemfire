/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.InterestPolicy;
import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The RegionSubscriptionAttributesNamespaceTest class is a test suite of test cases testing the contract
 * and functionality of declaring and defining Subscription Attributes for a Region in the Spring Data GemFire
 * XML namespace (XSD) configuration meta-data.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.SubscriptionAttributesFactoryBean
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.6.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class RegionSubscriptionAttributesNamespaceTest {

	@Resource(name = "NoSubscriptionRegion")
	private Region<?, ?> noSubscriptionRegion;

	@Resource(name = "AllSubscriptionRegion")
	private Region<?, ?> allSubscriptionRegion;

	@Resource(name = "CacheContentSubscriptionRegion")
	private Region<?, ?> cacheContentSubscriptionRegion;

	@Resource(name = "DefaultSubscriptionRegion")
	private Region<?, ?> defaultSubscriptionRegion;

	protected void assertSubscription(final Region<?, ?> region, final String expectedRegionName,
			final DataPolicy expectedDataPolicy, final InterestPolicy expectedInterestedPolicy) {
		assertNotNull(String.format("The '%1$s' Region was not properly configured an initialized!",
			expectedRegionName), region);
		assertEquals(expectedRegionName, region.getName());
		assertNotNull(region.getAttributes());
		assertEquals(expectedDataPolicy, region.getAttributes().getDataPolicy());
		assertNotNull(region.getAttributes().getSubscriptionAttributes());
		assertEquals(expectedInterestedPolicy, region.getAttributes().getSubscriptionAttributes().getInterestPolicy());
	}

	@Test
	public void testNoSubscriptionRegion() {
		assertSubscription(noSubscriptionRegion, "NoSubscriptionRegion", DataPolicy.REPLICATE, InterestPolicy.DEFAULT);
	}

	@Test
	public void testAllSubscriptionRegion() {
		assertSubscription(allSubscriptionRegion, "AllSubscriptionRegion", DataPolicy.REPLICATE, InterestPolicy.ALL);
	}

	@Test
	public void testCacheContentSubscriptionRegion() {
		assertSubscription(cacheContentSubscriptionRegion, "CacheContentSubscriptionRegion", DataPolicy.PARTITION,
			InterestPolicy.CACHE_CONTENT);
	}

	@Test
	public void testDefaultSubscriptionRegion() {
		assertSubscription(defaultSubscriptionRegion, "DefaultSubscriptionRegion", DataPolicy.PARTITION,
			InterestPolicy.DEFAULT);
	}

}
