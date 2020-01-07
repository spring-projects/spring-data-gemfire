/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire.eviction;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.apache.geode.cache.EvictionAlgorithm;

import org.junit.Test;

/**
 * The EvictionTypeTest class is a test suite of test cases testing the contract and functionality
 * of the EvictionType enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see EvictionPolicyType
 * @see org.apache.geode.cache.EvictionAlgorithm
 * @since 1.6.0
 */
public class EvictionPolicyTypeTest {

	@Test
	public void testStaticGetEvictionAlgorithm() {
		assertEquals(EvictionAlgorithm.LRU_HEAP, EvictionPolicyType.getEvictionAlgorithm(
			EvictionPolicyType.HEAP_PERCENTAGE));
		assertEquals(EvictionAlgorithm.LRU_MEMORY, EvictionPolicyType.getEvictionAlgorithm(
			EvictionPolicyType.MEMORY_SIZE));
	}

	@Test
	public void testStaticGetEvictionAlgorithmWithNull() {
		assertNull(EvictionPolicyType.getEvictionAlgorithm(null));
	}

	@Test
	public void testGetEvictionAlgorithm() {
		assertEquals(EvictionAlgorithm.LRU_ENTRY, EvictionPolicyType.ENTRY_COUNT.getEvictionAlgorithm());
		assertEquals(EvictionAlgorithm.LRU_HEAP, EvictionPolicyType.HEAP_PERCENTAGE.getEvictionAlgorithm());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, EvictionPolicyType.MEMORY_SIZE.getEvictionAlgorithm());
		assertEquals(EvictionAlgorithm.NONE, EvictionPolicyType.NONE.getEvictionAlgorithm());
	}

	@Test
	public void testValueOf() {
		assertEquals(EvictionPolicyType.ENTRY_COUNT, EvictionPolicyType.valueOf(EvictionAlgorithm.LRU_ENTRY));
		assertEquals(EvictionPolicyType.HEAP_PERCENTAGE, EvictionPolicyType.valueOf(EvictionAlgorithm.LRU_HEAP));
		assertEquals(EvictionPolicyType.MEMORY_SIZE, EvictionPolicyType.valueOf(EvictionAlgorithm.LRU_MEMORY));
		assertEquals(EvictionPolicyType.NONE, EvictionPolicyType.valueOf(EvictionAlgorithm.NONE));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testValueOfInvalidEvictionAlgorithms() {
		assertNull(EvictionPolicyType.valueOf(EvictionAlgorithm.LIFO_ENTRY));
		assertNull(EvictionPolicyType.valueOf(EvictionAlgorithm.LIFO_MEMORY));
	}

	@Test
	public void testValueOfWithNull() {
		assertNull(EvictionPolicyType.valueOf((EvictionAlgorithm) null));
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(EvictionPolicyType.ENTRY_COUNT, EvictionPolicyType.valueOfIgnoreCase("entry_count"));
		assertEquals(EvictionPolicyType.HEAP_PERCENTAGE, EvictionPolicyType.valueOfIgnoreCase("Heap_Percentage"));
		assertEquals(EvictionPolicyType.MEMORY_SIZE, EvictionPolicyType.valueOfIgnoreCase("MEMorY_SiZe"));
		assertEquals(EvictionPolicyType.NONE, EvictionPolicyType.valueOfIgnoreCase("NONE"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(EvictionPolicyType.valueOfIgnoreCase("number_of_entries"));
		assertNull(EvictionPolicyType.valueOfIgnoreCase("heap_%"));
		assertNull(EvictionPolicyType.valueOfIgnoreCase("mem_size"));
		assertNull(EvictionPolicyType.valueOfIgnoreCase("memory_space"));
		assertNull(EvictionPolicyType.valueOfIgnoreCase("  "));
		assertNull(EvictionPolicyType.valueOfIgnoreCase(""));
		assertNull(EvictionPolicyType.valueOfIgnoreCase(null));
	}

}
