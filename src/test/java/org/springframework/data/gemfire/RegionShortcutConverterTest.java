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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;

import org.apache.geode.cache.RegionShortcut;
import org.junit.Test;

/**
 * The RegionShortcutConverterTest class is a test suite of test cases testing the contract and functionality of the
 * RegionShortcutConverter class
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.RegionShortcutConverter
 * @see org.apache.geode.cache.RegionShortcut
 * @since 1.3.4
 */
public class RegionShortcutConverterTest {

	private final RegionShortcutConverter converter = new RegionShortcutConverter();

	@Test
	public void testToUpperCase() {
		assertEquals("TEST", RegionShortcutConverter.toUpperCase("test"));
		assertEquals("TEST", RegionShortcutConverter.toUpperCase(" Test  "));
		assertEquals("", RegionShortcutConverter.toUpperCase(""));
		assertEquals("", RegionShortcutConverter.toUpperCase("  "));
		assertEquals("NULL", RegionShortcutConverter.toUpperCase("null"));
		assertEquals("null", RegionShortcutConverter.toUpperCase(null));
	}

	@Test
	public void testConvert() {
		for (RegionShortcut shortcut : RegionShortcut.values()) {
			assertEquals(shortcut, converter.convert(shortcut.name()));
		}

		assertEquals(RegionShortcut.PARTITION_PROXY, converter.convert("Partition_Proxy"));
		assertEquals(RegionShortcut.REPLICATE_OVERFLOW, converter.convert("replicate_overflow"));
		assertEquals(RegionShortcut.LOCAL_HEAP_LRU, converter.convert("local_Heap_LRU"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertWithIllegalEnumeratedValue() {
		converter.convert("localPersistentOverflow");
	}

}
