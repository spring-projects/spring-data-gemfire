/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.Test;

/**
 * The SubscriptionEvictionPolicyConverterTest class is a test suite of test cases testing the contract
 * and functionality of the SubscriptionEvictionPolicyConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.server.SubscriptionEvictionPolicy
 * @see org.springframework.data.gemfire.server.SubscriptionEvictionPolicyConverter
 * @since 1.6.0
 */
public class SubscriptionEvictionPolicyConverterTest {

	private final SubscriptionEvictionPolicyConverter converter = new SubscriptionEvictionPolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(SubscriptionEvictionPolicy.ENTRY, converter.convert("EnTry"));
		assertEquals(SubscriptionEvictionPolicy.MEM, converter.convert("MEM"));
		assertEquals(SubscriptionEvictionPolicy.NONE, converter.convert("nONE"));
		assertEquals(SubscriptionEvictionPolicy.NONE, converter.convert("NOne"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertIllegalValue() {
		try {
			converter.setAsText("memory");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(memory) is not a valid SubscriptionEvictionPolicy!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		assertNull(converter.getValue());
		converter.setAsText("enTRY");
		assertEquals(SubscriptionEvictionPolicy.ENTRY, converter.getValue());
		converter.setAsText("MEm");
		assertEquals(SubscriptionEvictionPolicy.MEM, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithIllegalValue() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("KEYS");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(KEYS) is not a valid SubscriptionEvictionPolicy!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
