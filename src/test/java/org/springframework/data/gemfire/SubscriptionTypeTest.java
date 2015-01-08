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
import static org.junit.Assert.assertNull;

import org.junit.Test;

import com.gemstone.gemfire.cache.InterestPolicy;

/**
 * The SubscriptionTypeTest class is a test suite of test cases testing the contract and functionality
 * of the SubscriptionType enumerated type.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.SubscriptionType
 * @since 1.6.0
 */
public class SubscriptionTypeTest {

	@Test
	public void testValueOfInterestPolicies() {
		try {
			for (byte ordinal = 0; ordinal < Byte.MAX_VALUE; ordinal++) {
				InterestPolicy interestPolicy = InterestPolicy.fromOrdinal(ordinal);
				SubscriptionType subscriptionType = SubscriptionType.valueOf(interestPolicy);
				assertNotNull(subscriptionType);
				assertEquals(interestPolicy, subscriptionType.getInterestPolicy());
			}
		}
		catch (ArrayIndexOutOfBoundsException ignore) {
		}
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(SubscriptionType.ALL, SubscriptionType.valueOfIgnoreCase("all"));
		assertEquals(SubscriptionType.CACHE_CONTENT, SubscriptionType.valueOfIgnoreCase("Cache_Content"));
		assertEquals(SubscriptionType.DEFAULT, SubscriptionType.valueOfIgnoreCase("DeFault"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValue() {
		assertNull(SubscriptionType.valueOfIgnoreCase(null));
		assertNull(SubscriptionType.valueOfIgnoreCase(""));
		assertNull(SubscriptionType.valueOfIgnoreCase("  "));
		assertNull(SubscriptionType.valueOfIgnoreCase("@11"));
		assertNull(SubscriptionType.valueOfIgnoreCase("CACHE_KEYS"));
		assertNull(SubscriptionType.valueOfIgnoreCase("invalid"));
		assertNull(SubscriptionType.valueOfIgnoreCase("test"));
	}

}
