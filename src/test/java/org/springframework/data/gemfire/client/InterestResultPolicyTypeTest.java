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

package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.apache.geode.cache.InterestResultPolicy;

import org.junit.Test;

/**
 * The InterestResultPolicyTypeTest class is a test suite of test cases testing the contract and functionality
 * of the InterestResultPolicyType enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.client.InterestResultPolicyTypeTest
 * @see org.apache.geode.cache.InterestResultPolicy
 * @since 1.6.0
 */
public class InterestResultPolicyTypeTest {

	@Test
	public void testStaticGetInterestResultPolicy() {
		assertEquals(InterestResultPolicy.KEYS, InterestResultPolicyType.getInterestResultPolicy(
			InterestResultPolicyType.KEYS));
		assertEquals(InterestResultPolicy.KEYS_VALUES, InterestResultPolicyType.getInterestResultPolicy(
			InterestResultPolicyType.KEYS_VALUES));
	}

	@Test
	public void testStaticGetInterestResultPolicyWithNull() {
		assertNull(InterestResultPolicyType.getInterestResultPolicy(null));
	}

	@Test
	public void testDefault() {
		assertEquals(InterestResultPolicyType.DEFAULT, InterestResultPolicyType.valueOf(InterestResultPolicy.DEFAULT));
		assertEquals(InterestResultPolicy.DEFAULT, InterestResultPolicyType.DEFAULT.getInterestResultPolicy());
		assertSame(InterestResultPolicyType.KEYS_VALUES, InterestResultPolicyType.DEFAULT);
	}

	@Test
	public void testValueOf() {
		try {
			for (byte ordinal = 0; ordinal < Byte.MAX_VALUE; ordinal++) {
				InterestResultPolicy interestResultPolicy = InterestResultPolicy.fromOrdinal(ordinal);
				InterestResultPolicyType interestResultPolicyType = InterestResultPolicyType.valueOf(
					interestResultPolicy);

				assertNotNull(interestResultPolicyType);
				assertEquals(interestResultPolicy, interestResultPolicyType.getInterestResultPolicy());
			}
		}
		catch (ArrayIndexOutOfBoundsException ignore) {
		}
	}

	@Test
	public void testValueOfWithNull() {
		assertNull(InterestResultPolicyType.valueOf((InterestResultPolicy) null));
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(InterestResultPolicyType.KEYS, InterestResultPolicyType.valueOfIgnoreCase("KEYS"));
		assertEquals(InterestResultPolicyType.KEYS_VALUES, InterestResultPolicyType.valueOfIgnoreCase("Keys_Values"));
		assertEquals(InterestResultPolicyType.NONE, InterestResultPolicyType.valueOfIgnoreCase("none"));
		assertEquals(InterestResultPolicyType.NONE, InterestResultPolicyType.valueOfIgnoreCase("nONE"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(InterestResultPolicyType.valueOfIgnoreCase("keyz"));
		assertNull(InterestResultPolicyType.valueOfIgnoreCase("KEY_VALUE"));
		assertNull(InterestResultPolicyType.valueOfIgnoreCase("all"));
		assertNull(InterestResultPolicyType.valueOfIgnoreCase("  "));
		assertNull(InterestResultPolicyType.valueOfIgnoreCase(""));
		assertNull(InterestResultPolicyType.valueOfIgnoreCase(null));
	}

}
