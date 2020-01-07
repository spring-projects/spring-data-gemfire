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

package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.apache.geode.cache.client.ClientRegionShortcut;

import org.junit.Test;

/**
 * The ClientRegionShortcutWrapperTest class is a test suite of test cases testing the contract and functionality of the
 * ClientRegionShortcutWrapper enum class type.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see ClientRegionShortcutWrapper
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @since 1.4.0
 */
public class ClientRegionShortcutWrapperTest {

	@Test
	public void testOneToOneMapping() {
		for (ClientRegionShortcut shortcut : ClientRegionShortcut.values()) {
			assertNotNull(ClientRegionShortcutWrapper.valueOf(shortcut.name()));
			assertFalse(ClientRegionShortcutWrapper.UNSPECIFIED.equals(ClientRegionShortcutWrapper.valueOf(shortcut)));
		}
	}

	@Test
	public void testClientRegionShortcutUnspecified() {
		assertEquals(ClientRegionShortcutWrapper.UNSPECIFIED, ClientRegionShortcutWrapper.valueOf(
			(ClientRegionShortcut) null));
	}

	@Test
	public void testIsCaching() {
		assertTrue(ClientRegionShortcutWrapper.CACHING_PROXY.isCaching());
		assertTrue(ClientRegionShortcutWrapper.CACHING_PROXY_HEAP_LRU.isCaching());
		assertTrue(ClientRegionShortcutWrapper.CACHING_PROXY_OVERFLOW.isCaching());
		assertFalse(ClientRegionShortcutWrapper.LOCAL.isCaching());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_HEAP_LRU.isCaching());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_OVERFLOW.isCaching());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_PERSISTENT.isCaching());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isCaching());
		assertFalse(ClientRegionShortcutWrapper.PROXY.isCaching());
		assertFalse(ClientRegionShortcutWrapper.UNSPECIFIED.isCaching());
	}

	@Test
	public void testIsHeapLru() {
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY.isHeapLru());
		assertTrue(ClientRegionShortcutWrapper.CACHING_PROXY_HEAP_LRU.isHeapLru());
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY_OVERFLOW.isHeapLru());
		assertFalse(ClientRegionShortcutWrapper.LOCAL.isHeapLru());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_HEAP_LRU.isHeapLru());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_OVERFLOW.isHeapLru());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_PERSISTENT.isHeapLru());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isHeapLru());
		assertFalse(ClientRegionShortcutWrapper.PROXY.isHeapLru());
		assertFalse(ClientRegionShortcutWrapper.UNSPECIFIED.isHeapLru());
	}

	@Test
	public void testIsLocal() {
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY.isLocal());
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY_HEAP_LRU.isLocal());
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY_OVERFLOW.isLocal());
		assertTrue(ClientRegionShortcutWrapper.LOCAL.isLocal());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_HEAP_LRU.isLocal());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_OVERFLOW.isLocal());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_PERSISTENT.isLocal());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isLocal());
		assertFalse(ClientRegionShortcutWrapper.PROXY.isLocal());
		assertFalse(ClientRegionShortcutWrapper.UNSPECIFIED.isLocal());
	}

	@Test
	public void testIsOverflow() {
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY.isOverflow());
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY_HEAP_LRU.isOverflow());
		assertTrue(ClientRegionShortcutWrapper.CACHING_PROXY_OVERFLOW.isOverflow());
		assertFalse(ClientRegionShortcutWrapper.LOCAL.isOverflow());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_HEAP_LRU.isOverflow());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_OVERFLOW.isOverflow());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_PERSISTENT.isOverflow());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isOverflow());
		assertFalse(ClientRegionShortcutWrapper.PROXY.isOverflow());
		assertFalse(ClientRegionShortcutWrapper.UNSPECIFIED.isOverflow());
	}

	@Test
	public void testIsPersistent() {
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY.isPersistent());
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY_HEAP_LRU.isPersistent());
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY_OVERFLOW.isPersistent());
		assertFalse(ClientRegionShortcutWrapper.LOCAL.isPersistent());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_HEAP_LRU.isPersistent());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_OVERFLOW.isPersistent());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_PERSISTENT.isPersistent());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isPersistent());
		assertFalse(ClientRegionShortcutWrapper.PROXY.isPersistent());
		assertFalse(ClientRegionShortcutWrapper.UNSPECIFIED.isPersistent());
	}

	@Test
	public void testIsPersistentOverflow() {
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY.isPersistentOverflow());
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY_HEAP_LRU.isPersistentOverflow());
		assertFalse(ClientRegionShortcutWrapper.CACHING_PROXY_OVERFLOW.isPersistentOverflow());
		assertFalse(ClientRegionShortcutWrapper.LOCAL.isPersistentOverflow());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_HEAP_LRU.isPersistentOverflow());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_OVERFLOW.isPersistentOverflow());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_PERSISTENT.isPersistentOverflow());
		assertTrue(ClientRegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isPersistentOverflow());
		assertFalse(ClientRegionShortcutWrapper.PROXY.isPersistentOverflow());
		assertFalse(ClientRegionShortcutWrapper.UNSPECIFIED.isPersistentOverflow());
	}

	@Test
	public void testIsProxy() {
		assertTrue(ClientRegionShortcutWrapper.CACHING_PROXY.isProxy());
		assertTrue(ClientRegionShortcutWrapper.CACHING_PROXY_HEAP_LRU.isProxy());
		assertTrue(ClientRegionShortcutWrapper.CACHING_PROXY_OVERFLOW.isProxy());
		assertFalse(ClientRegionShortcutWrapper.LOCAL.isProxy());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_HEAP_LRU.isProxy());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_OVERFLOW.isProxy());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_PERSISTENT.isProxy());
		assertFalse(ClientRegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isProxy());
		assertTrue(ClientRegionShortcutWrapper.PROXY.isProxy());
		assertFalse(ClientRegionShortcutWrapper.UNSPECIFIED.isProxy());
	}
}
