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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.apache.geode.cache.InterestPolicy;
import org.apache.geode.cache.SubscriptionAttributes;
import org.junit.Test;

/**
 * The SubscriptionAttributesFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the SubscriptionAttributesFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.SubscriptionAttributesFactoryBean
 * @since 1.6.0
 */
public class SubscriptionAttributesFactoryBeanTest {

	@Test
	public void testIsSingleton() {
		assertTrue(new SubscriptionAttributesFactoryBean().isSingleton());
	}

	@Test
	public void testSetAndGetInterestPolicy() {
		SubscriptionAttributesFactoryBean factoryBean = new SubscriptionAttributesFactoryBean();

		assertEquals(InterestPolicy.DEFAULT, factoryBean.getInterestPolicy());

		factoryBean.setInterestPolicy(InterestPolicy.CACHE_CONTENT);

		assertEquals(InterestPolicy.CACHE_CONTENT, factoryBean.getInterestPolicy());

		factoryBean.setInterestPolicy(null);

		assertEquals(InterestPolicy.DEFAULT, factoryBean.getInterestPolicy());
	}

	@Test
	public void testGetObjectAndObjectTypeForAllInterestPolicy() throws Exception {
		SubscriptionAttributesFactoryBean factoryBean = new SubscriptionAttributesFactoryBean();

		factoryBean.setInterestPolicy(InterestPolicy.ALL);
		factoryBean.afterPropertiesSet();

		assertEquals(InterestPolicy.ALL, factoryBean.getInterestPolicy());

		SubscriptionAttributes subscriptionAttributes = factoryBean.getObject();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.ALL, subscriptionAttributes.getInterestPolicy());
		assertTrue(SubscriptionAttributes.class.isAssignableFrom(factoryBean.getObjectType()));
	}

	@Test
	public void testGetObjectAndObjectTypeForCacheContentInterestPolicy() throws Exception {
		SubscriptionAttributesFactoryBean factoryBean = new SubscriptionAttributesFactoryBean();

		factoryBean.setInterestPolicy(InterestPolicy.CACHE_CONTENT);
		factoryBean.afterPropertiesSet();

		assertEquals(InterestPolicy.CACHE_CONTENT, factoryBean.getInterestPolicy());

		SubscriptionAttributes subscriptionAttributes = factoryBean.getObject();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.CACHE_CONTENT, subscriptionAttributes.getInterestPolicy());
		assertTrue(SubscriptionAttributes.class.isAssignableFrom(factoryBean.getObjectType()));
	}

	@Test
	public void testGetObjectAndObjectTypeForDefaultInterestPolicy() throws Exception {
		SubscriptionAttributesFactoryBean factoryBean = new SubscriptionAttributesFactoryBean();

		factoryBean.afterPropertiesSet();

		assertEquals(InterestPolicy.DEFAULT, factoryBean.getInterestPolicy());

		SubscriptionAttributes subscriptionAttributes = factoryBean.getObject();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.DEFAULT, subscriptionAttributes.getInterestPolicy());
		assertTrue(SubscriptionAttributes.class.isAssignableFrom(factoryBean.getObjectType()));
	}

	@Test
	public void testGetObjectAndObjectTypeForNullInterestPolicy() throws Exception {
		SubscriptionAttributesFactoryBean factoryBean = new SubscriptionAttributesFactoryBean();

		factoryBean.setInterestPolicy(null);
		factoryBean.afterPropertiesSet();

		assertEquals(InterestPolicy.DEFAULT, factoryBean.getInterestPolicy());

		SubscriptionAttributes subscriptionAttributes = factoryBean.getObject();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.DEFAULT, subscriptionAttributes.getInterestPolicy());
		assertTrue(SubscriptionAttributes.class.isAssignableFrom(factoryBean.getObjectType()));
	}

}
