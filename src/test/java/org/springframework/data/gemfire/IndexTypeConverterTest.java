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
 * The IndexTypeConverterTest class is a test suite of test cases testing the contract and functionality
 * of the IndexTypeConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.IndexType
 * @see org.springframework.data.gemfire.IndexTypeConverter
 * @since 1.5.2
 */
public class IndexTypeConverterTest {

	private final IndexTypeConverter converter = new IndexTypeConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(IndexType.FUNCTIONAL, converter.convert("FUNCTIONAL"));
		assertEquals(IndexType.HASH, converter.convert("hASh"));
		assertEquals(IndexType.KEY, converter.convert("Key"));
		assertEquals(IndexType.PRIMARY_KEY, converter.convert("primary_KEY"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertWithIllegalValue() {
		try {
			converter.convert("function");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Failed to convert String (function) into an IndexType!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		assertNull(converter.getValue());
		converter.setAsText("HasH");
		assertEquals(IndexType.HASH, converter.getValue());
		converter.setAsText("key");
		assertEquals(IndexType.KEY, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithIllegalValue() {
		try {
			assertNull(converter.getValue());
			converter.setAsText("invalid");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Failed to convert String (invalid) into an IndexType!", expected.getMessage());
			throw expected;
		}
		finally {
			assertNull(converter.getValue());
		}
	}

}
