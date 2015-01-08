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
 * The EvictionTypeConverterTest class is a test suite of test cases testing the contract and functionality
 * of the EvictionTypeConverter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.EvictionTypeConverter
 * @since 1.6.0
 */
public class EvictionTypeConverterTest {

	private final EvictionTypeConverter converter = new EvictionTypeConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void testConvert() {
		assertEquals(EvictionType.ENTRY_COUNT, converter.convert("entry_count"));
		assertEquals(EvictionType.HEAP_PERCENTAGE, converter.convert("Heap_Percentage"));
		assertEquals(EvictionType.MEMORY_SIZE, converter.convert("MEMorY_SiZe"));
		assertEquals(EvictionType.NONE, converter.convert("NONE"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertWithIllegalValue() {
		try {
			converter.convert("LIFO_MEMORY");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Source (LIFO_MEMORY) is not a valid EvictionType!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAsText() {
		assertNull(converter.getValue());
		converter.setAsText("heap_percentage");
		assertEquals(EvictionType.HEAP_PERCENTAGE, converter.getValue());
		converter.setAsText("NOne");
		assertEquals(EvictionType.NONE, converter.getValue());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetAsTextWithIllegalValue() {
		try {
			converter.setAsText("LRU_COUNT");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Source (LRU_COUNT) is not a valid EvictionType!", expected.getMessage());
			throw expected;
		}
	}

}
