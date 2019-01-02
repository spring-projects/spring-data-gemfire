/*
 * Copyright 2010-2019 the original author or authors.
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

import org.junit.Test;

import com.gemstone.gemfire.cache.client.ClientRegionShortcut;

/**
 * The ClientRegionShortcutConverterTest class is a test suite of test cases testing the contract and functionality 
 * of the ClientRegionShortcutConverter class
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.ClientRegionShortcutConverter
 * @see com.gemstone.gemfire.cache.client.ClientRegionShortcut
 * @since 1.3.4
 */
public class ClientRegionShortcutConverterTest {

	private final ClientRegionShortcutConverter converter = new ClientRegionShortcutConverter();

	@Test
	public void testToUpperCase() {
		assertEquals("TEST", ClientRegionShortcutConverter.toUpperCase("test"));
		assertEquals("TEST", ClientRegionShortcutConverter.toUpperCase(" Test  "));
		assertEquals("", ClientRegionShortcutConverter.toUpperCase(""));
		assertEquals("", ClientRegionShortcutConverter.toUpperCase("  "));
		assertEquals("NULL", ClientRegionShortcutConverter.toUpperCase("null"));
		assertEquals("null", ClientRegionShortcutConverter.toUpperCase(null));
	}

	@Test
	public void testConvert() {
		for (ClientRegionShortcut shortcut : ClientRegionShortcut.values()) {
			assertEquals(shortcut, converter.convert(shortcut.name()));
		}

		assertEquals(ClientRegionShortcut.PROXY, converter.convert("Proxy"));
		assertEquals(ClientRegionShortcut.CACHING_PROXY, converter.convert("caching_proxy"));
		assertEquals(ClientRegionShortcut.LOCAL_HEAP_LRU, converter.convert("local_Heap_LRU"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConvertWithIllegalEnumeratedValue() {
		converter.convert("LOCAL Persistent OverFlow");
	}

}
