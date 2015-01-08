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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.gemstone.gemfire.cache.InterestPolicy;
import com.gemstone.gemfire.cache.SubscriptionAttributes;

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
	public void testGetObjectAndObjectTypeForSubscriptionType() throws Exception {
		SubscriptionAttributesFactoryBean factoryBean = new SubscriptionAttributesFactoryBean();

		factoryBean.setType(SubscriptionType.CACHE_CONTENT);
		factoryBean.afterPropertiesSet();

		assertEquals(SubscriptionType.CACHE_CONTENT, factoryBean.getType());
		assertEquals(InterestPolicy.CACHE_CONTENT, factoryBean.getPolicy());

		SubscriptionAttributes subscriptionAttributes = factoryBean.getObject();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.CACHE_CONTENT, subscriptionAttributes.getInterestPolicy());
		assertTrue(SubscriptionAttributes.class.isAssignableFrom(factoryBean.getObjectType()));
	}

	@Test
	public void testGetObjectAndObjectTypeForDefaultSubscriptionType() throws Exception {
		SubscriptionAttributesFactoryBean factoryBean = new SubscriptionAttributesFactoryBean();

		factoryBean.setType(null);
		factoryBean.afterPropertiesSet();

		assertEquals(SubscriptionType.DEFAULT, factoryBean.getType());
		assertEquals(InterestPolicy.DEFAULT, factoryBean.getPolicy());

		SubscriptionAttributes subscriptionAttributes = factoryBean.getObject();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.DEFAULT, subscriptionAttributes.getInterestPolicy());
		assertTrue(SubscriptionAttributes.class.isAssignableFrom(factoryBean.getObjectType()));
	}

	@Test
	public void testGetObjectAndObjectTypeForInterestPolicy() throws Exception {
		SubscriptionAttributesFactoryBean factoryBean = new SubscriptionAttributesFactoryBean();

		factoryBean.setPolicy(InterestPolicy.ALL);
		factoryBean.afterPropertiesSet();

		assertEquals(SubscriptionType.ALL, factoryBean.getType());
		assertEquals(InterestPolicy.ALL, factoryBean.getPolicy());

		SubscriptionAttributes subscriptionAttributes = factoryBean.getObject();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.ALL, subscriptionAttributes.getInterestPolicy());
		assertTrue(SubscriptionAttributes.class.isAssignableFrom(factoryBean.getObjectType()));
	}

	@Test
	public void testGetObjectAndObjectTypeForNullInterestPolicy() throws Exception {
		SubscriptionAttributesFactoryBean factoryBean = new SubscriptionAttributesFactoryBean();

		factoryBean.setPolicy(null);
		factoryBean.afterPropertiesSet();

		assertEquals(SubscriptionType.DEFAULT, factoryBean.getType());
		assertEquals(InterestPolicy.DEFAULT, factoryBean.getPolicy());

		SubscriptionAttributes subscriptionAttributes = factoryBean.getObject();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.DEFAULT, subscriptionAttributes.getInterestPolicy());
		assertTrue(SubscriptionAttributes.class.isAssignableFrom(factoryBean.getObjectType()));
	}

}
