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

import org.junit.Test;

import com.gemstone.gemfire.cache.EvictionAlgorithm;

/**
 * The EvictionTypeTest class is a test suite of test cases testing the contract and functionality
 * of the EvictionType enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.EvictionType
 * @see com.gemstone.gemfire.cache.EvictionAlgorithm
 * @since 1.6.0
 */
public class EvictionTypeTest {

	@Test
	public void testStaticGetEvictionAlgorithm() {
		assertEquals(EvictionAlgorithm.LRU_HEAP, EvictionType.getEvictionAlgorithm(EvictionType.HEAP_PERCENTAGE));
		assertEquals(EvictionAlgorithm.LRU_MEMORY, EvictionType.getEvictionAlgorithm(EvictionType.MEMORY_SIZE));
	}

	@Test
	public void testStaticGetEvictionAlgorithmWithNull() {
		assertNull(EvictionType.getEvictionAlgorithm(null));
	}

	@Test
	public void testGetEvictionAlgorithm() {
		assertEquals(EvictionAlgorithm.LRU_ENTRY, EvictionType.ENTRY_COUNT.getEvictionAlgorithm());
		assertEquals(EvictionAlgorithm.LRU_HEAP, EvictionType.HEAP_PERCENTAGE.getEvictionAlgorithm());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, EvictionType.MEMORY_SIZE.getEvictionAlgorithm());
		assertEquals(EvictionAlgorithm.NONE, EvictionType.NONE.getEvictionAlgorithm());
	}

	@Test
	public void testValueOfEvictionAlgorithms() {
		assertEquals(EvictionType.ENTRY_COUNT, EvictionType.valueOf(EvictionAlgorithm.LRU_ENTRY));
		assertEquals(EvictionType.HEAP_PERCENTAGE, EvictionType.valueOf(EvictionAlgorithm.LRU_HEAP));
		assertEquals(EvictionType.MEMORY_SIZE, EvictionType.valueOf(EvictionAlgorithm.LRU_MEMORY));
		assertEquals(EvictionType.NONE, EvictionType.valueOf(EvictionAlgorithm.NONE));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testValueOfInvalidEvictionAlgorithms() {
		assertNull(EvictionType.valueOf(EvictionAlgorithm.LIFO_ENTRY));
		assertNull(EvictionType.valueOf(EvictionAlgorithm.LIFO_MEMORY));
		assertNull(EvictionType.valueOf((EvictionAlgorithm) null));
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(EvictionType.ENTRY_COUNT, EvictionType.valueOfIgnoreCase("entry_count"));
		assertEquals(EvictionType.HEAP_PERCENTAGE, EvictionType.valueOfIgnoreCase("Heap_Percentage"));
		assertEquals(EvictionType.MEMORY_SIZE, EvictionType.valueOfIgnoreCase("MEMorY_SiZe"));
		assertEquals(EvictionType.NONE, EvictionType.valueOfIgnoreCase("NONE"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(EvictionType.valueOfIgnoreCase("number_of_entries"));
		assertNull(EvictionType.valueOfIgnoreCase("heap_%"));
		assertNull(EvictionType.valueOfIgnoreCase("mem_size"));
		assertNull(EvictionType.valueOfIgnoreCase("memory_space"));
		assertNull(EvictionType.valueOfIgnoreCase("  "));
		assertNull(EvictionType.valueOfIgnoreCase(""));
		assertNull(EvictionType.valueOfIgnoreCase(null));
	}

}
