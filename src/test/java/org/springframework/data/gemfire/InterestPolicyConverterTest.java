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
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.Test;

import com.gemstone.gemfire.cache.InterestPolicy;

/**
 * The InterestPolicyConverterTest class is a test suite of test cases testing the contract and functionality
 * of the InterestPolicyConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.InterestPolicyConverter
 * @see com.gemstone.gemfire.cache.InterestPolicy
 * @since 1.6.0
 */
public class InterestPolicyConverterTest {

	private InterestPolicyConverter converter = new InterestPolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(InterestPolicy.ALL, converter.convert("all"));
		assertEquals(InterestPolicy.CACHE_CONTENT, converter.convert("Cache_Content"));
		assertEquals(InterestPolicy.CACHE_CONTENT, converter.convert("CACHE_ConTent"));
		assertEquals(InterestPolicy.ALL, converter.convert("ALL"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertIllegalValue() {
		try {
			converter.convert("invalid_value");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(invalid_value) is not a valid InterestPolicy!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		converter.setAsText("aLl");
		assertEquals(InterestPolicy.ALL, converter.getValue());
		converter.setAsText("Cache_CoNTeNT");
		assertEquals(InterestPolicy.CACHE_CONTENT, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithInvalidValue() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("none");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(none) is not a valid InterestPolicy!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
