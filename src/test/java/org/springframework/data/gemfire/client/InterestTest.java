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

package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.springframework.core.ConstantException;

import com.gemstone.gemfire.cache.InterestResultPolicy;

/**
 * The InterestTest class is a test suite of test cases testing the contract and functionality of the Interest class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.client.Interest
 * @since 1.6.0
 */
public class InterestTest {

	@Test
	public void testConstruct() {
		Interest<String> interest = new Interest<String>("aKey", "keys", true, false);

		assertEquals("aKey", interest.getKey());
		assertEquals(InterestResultPolicy.KEYS, interest.getPolicy());
		assertTrue(interest.isDurable());
		assertFalse(interest.isReceiveValues());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConstructWithNullKey() {
		try {
			new Interest<Object>(null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("a non-null key is required", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = ConstantException.class)
	public void testConstructWithInvalidPolicy() {
		new Interest<String>("aKey", "INVALID");
	}

	@Test
	public void testSetAndGetState() {
		Interest<String> interest = new Interest();

		assertNull(interest.getKey());
		assertEquals(InterestResultPolicy.DEFAULT, interest.getPolicy());
		assertFalse(interest.isDurable());
		assertTrue(interest.isReceiveValues());

		interest.setDurable(true);
		interest.setKey("testKey");
		interest.setPolicy(InterestResultPolicy.KEYS_VALUES);
		interest.setReceiveValues(false);

		assertEquals("testKey", interest.getKey());
		assertEquals(InterestResultPolicy.KEYS_VALUES, interest.getPolicy());
		assertTrue(interest.isDurable());
		assertFalse(interest.isReceiveValues());
	}

	@Test
	public void testSetAndGetPolicy() {
		Interest<Object> interest = new Interest<Object>();

		assertEquals(InterestResultPolicy.DEFAULT, interest.getPolicy());

		interest.setPolicy(InterestResultPolicy.NONE);

		assertEquals(InterestResultPolicy.NONE, interest.getPolicy());

		interest.setPolicy("keys");

		assertEquals(InterestResultPolicy.KEYS, interest.getPolicy());
	}

	@Test(expected = ConstantException.class)
	public void testSetPolicyWithIllegalValue() {
		new Interest<Object>().setPolicy("ILLEGAL");
	}

}
