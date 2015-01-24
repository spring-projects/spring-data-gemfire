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

/**
 * The IndexMaintenanceTypeConverterTest class is a test suite of test case testing the contract and functionality
 * of the IndexMaintenancePolicyConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.IndexMaintenancePolicyConverter
 * @see org.springframework.data.gemfire.IndexMaintenancePolicyType
 * @since 1.6.0
 */
public class IndexMaintenancePolicyConverterTest {

	private final IndexMaintenancePolicyConverter converter = new IndexMaintenancePolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(IndexMaintenancePolicyType.ASYNCHRONOUS, converter.convert("asynchronous"));
		assertEquals(IndexMaintenancePolicyType.SYNCHRONOUS, converter.convert("Synchronous"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertIllegalValue() {
		try {
			converter.convert("sync");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(sync) is not a valid IndexMaintenancePolicyType!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		converter.setAsText("aSynchronous");
		assertEquals(IndexMaintenancePolicyType.ASYNCHRONOUS, converter.getValue());
		converter.setAsText("synchrONoUS");
		assertEquals(IndexMaintenancePolicyType.SYNCHRONOUS, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextThrowsIllegalArgumentException() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("async");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("(async) is not a valid IndexMaintenancePolicyType!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
