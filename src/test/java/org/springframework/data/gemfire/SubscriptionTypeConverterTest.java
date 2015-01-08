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

import org.junit.After;
import org.junit.Test;

/**
 * The SubscriptionTypeConverterTest class is a test suite of test cases testing the contract and functionality
 * of the SubscriptionTypeConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.SubscriptionTypeConverter
 * @since 1.5.0
 */
public class SubscriptionTypeConverterTest {

	private SubscriptionTypeConverter converter = new SubscriptionTypeConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(SubscriptionType.ALL, converter.convert("all"));
		assertEquals(SubscriptionType.CACHE_CONTENT, converter.convert("Cache_Content"));
		assertEquals(SubscriptionType.DEFAULT, converter.convert("DeFault"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertWithInvalidValue() {
		try {
			converter.convert("invalid");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Source (invalid) is not a valid SubscriptionType!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		converter.setAsText("aLl");
		assertEquals(SubscriptionType.ALL, converter.getValue());
		converter.setAsText("CACHE_content");
		assertEquals(SubscriptionType.CACHE_CONTENT, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithInvalidValue() {
		try {
			converter.setAsText("none");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Source (none) is not a valid SubscriptionType!", expected.getMessage());
			throw expected;
		}
	}

}
