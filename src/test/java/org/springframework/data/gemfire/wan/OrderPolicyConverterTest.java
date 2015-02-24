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

package org.springframework.data.gemfire.wan;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.Test;

import com.gemstone.gemfire.cache.util.Gateway;

/**
 * The OrderPolicyConverterTest class is a test suite of test cases testing the contract and functionality
 * of the OrderPolicyConverter.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.wan.OrderPolicyConverter
 * @since 1.7.0
 */
@SuppressWarnings("deprecation")
public class OrderPolicyConverterTest {

	private final OrderPolicyConverter converter = new OrderPolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(Gateway.OrderPolicy.KEY, converter.convert("key"));
		assertEquals(Gateway.OrderPolicy.PARTITION, converter.convert("Partition"));
		assertEquals(Gateway.OrderPolicy.THREAD, converter.convert("THREAD"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertIllegalValue() {
		try {
			converter.convert("process");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(process) is not a valid OrderPolicy!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		converter.setAsText("PartItIOn");
		assertEquals(Gateway.OrderPolicy.PARTITION, converter.getValue());
		converter.setAsText("thREAD");
		assertEquals(Gateway.OrderPolicy.THREAD, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithIllegalValue() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("value");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(value) is not a valid OrderPolicy!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
