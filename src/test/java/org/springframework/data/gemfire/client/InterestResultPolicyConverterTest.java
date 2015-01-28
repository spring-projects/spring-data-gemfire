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
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.Test;

import com.gemstone.gemfire.cache.InterestResultPolicy;

/**
 * The InterestResultPolicyConverterTest class is a test suite of test cases testing the contract and functionality
 * of the InterestResultPolicyConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.client.InterestResultPolicyConverter
 * @see org.springframework.data.gemfire.client.InterestResultPolicyType
 * @see com.gemstone.gemfire.cache.InterestResultPolicy
 * @since 1.6.0
 */
public class InterestResultPolicyConverterTest {

	private final InterestResultPolicyConverter converter = new InterestResultPolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(InterestResultPolicy.NONE, converter.convert("NONE"));
		assertEquals(InterestResultPolicy.KEYS, converter.convert("Keys"));
		assertEquals(InterestResultPolicy.KEYS_VALUES, converter.convert("kEyS_ValUes"));
		assertEquals(InterestResultPolicy.NONE, converter.convert("nONe"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertIllegalValue() {
		try {
			converter.convert("illegal_value");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(illegal_value) is not a valid InterestResultPolicy!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		assertNull(converter.getValue());
		converter.setAsText("NOne");
		assertEquals(InterestResultPolicy.NONE, converter.getValue());
		converter.setAsText("KeYs");
		assertEquals(InterestResultPolicy.KEYS, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithIllegalValue() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("illegal_value");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(illegal_value) is not a valid InterestResultPolicy!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
