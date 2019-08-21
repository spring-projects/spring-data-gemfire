/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.apache.geode.cache.InterestPolicy;

import org.junit.Test;

/**
 * The InterestPolicyTypeTest class is a test suite of test cases testing the contract and functionality
 * of the InterestPolicyType enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.InterestPolicyType
 * @see org.apache.geode.cache.InterestPolicy
 * @since 1.6.0
 */
public class InterestPolicyTypeTest {

	@Test
	public void testStaticGetInterestPolicy() {
		assertEquals(InterestPolicy.ALL, InterestPolicyType.getInterestPolicy(InterestPolicyType.ALL));
		assertEquals(InterestPolicy.CACHE_CONTENT, InterestPolicyType.getInterestPolicy(InterestPolicyType.CACHE_CONTENT));
	}

	@Test
	public void testStaticGetInterestPolicyWithNull() {
		assertNull(InterestPolicyType.getInterestPolicy(null));
	}

	@Test
	public void testGetInterestPolicy() {
		assertEquals(InterestPolicy.ALL, InterestPolicyType.ALL.getInterestPolicy());
		assertEquals(InterestPolicy.CACHE_CONTENT, InterestPolicyType.CACHE_CONTENT.getInterestPolicy());
	}

	@Test
	public void testDefault() {
		assertEquals(InterestPolicy.DEFAULT, InterestPolicyType.DEFAULT.getInterestPolicy());
		assertSame(InterestPolicyType.CACHE_CONTENT, InterestPolicyType.DEFAULT);
	}

	@Test
	public void testValueOf() {
		try {
			for (byte ordinal = 0; ordinal < Byte.MAX_VALUE; ordinal++) {
				InterestPolicy interestPolicy = InterestPolicy.fromOrdinal(ordinal);
				InterestPolicyType interestPolicyType = InterestPolicyType.valueOf(interestPolicy);

				assertNotNull(interestPolicyType);
				assertEquals(interestPolicy, interestPolicyType.getInterestPolicy());
			}
		}
		catch (ArrayIndexOutOfBoundsException ignore) {
		}
	}

	@Test
	public void testValueOfWithNull() {
		assertNull(InterestPolicyType.valueOf((InterestPolicy) null));
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(InterestPolicyType.ALL, InterestPolicyType.valueOfIgnoreCase("all"));
		assertEquals(InterestPolicyType.CACHE_CONTENT, InterestPolicyType.valueOfIgnoreCase("Cache_Content"));
		assertEquals(InterestPolicyType.ALL, InterestPolicyType.valueOfIgnoreCase("ALL"));
		assertEquals(InterestPolicyType.CACHE_CONTENT, InterestPolicyType.valueOfIgnoreCase("CACHE_ConTent"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(InterestPolicyType.valueOfIgnoreCase("@11"));
		assertNull(InterestPolicyType.valueOfIgnoreCase("CACHE_KEYS"));
		assertNull(InterestPolicyType.valueOfIgnoreCase("invalid"));
		assertNull(InterestPolicyType.valueOfIgnoreCase("test"));
		assertNull(InterestPolicyType.valueOfIgnoreCase("  "));
		assertNull(InterestPolicyType.valueOfIgnoreCase(""));
		assertNull(InterestPolicyType.valueOfIgnoreCase(null));
	}

}
