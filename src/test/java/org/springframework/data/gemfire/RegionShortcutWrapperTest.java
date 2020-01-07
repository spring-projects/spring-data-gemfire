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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.apache.geode.cache.RegionShortcut;

import org.junit.Test;

/**
 * The RegionShortcutWrapperTest class is a test suite of test cases testing the contract and functionality of the
 * RegionShortcutWrapper enum class type.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see RegionShortcutWrapper
 * @see org.apache.geode.cache.RegionShortcut
 * @since 1.4.0
 */
public class RegionShortcutWrapperTest {

	@Test
	public void testOneForOneMapping() {
		for (RegionShortcut shortcut : RegionShortcut.values()) {
			assertNotNull(RegionShortcutWrapper.valueOf(shortcut.name()));
			assertFalse(RegionShortcutWrapper.UNSPECIFIED.equals(RegionShortcutWrapper.valueOf(shortcut)));
		}
	}

	@Test
	public void testRegionShortcutUnspecified() {
		assertEquals(RegionShortcutWrapper.UNSPECIFIED, RegionShortcutWrapper.valueOf((RegionShortcut) null));
	}

	@Test
	public void testIsHeapLru() {
		assertFalse(RegionShortcutWrapper.LOCAL.isHeapLru());
		assertTrue(RegionShortcutWrapper.LOCAL_HEAP_LRU.isHeapLru());
		assertFalse(RegionShortcutWrapper.LOCAL_OVERFLOW.isHeapLru());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT.isHeapLru());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION.isHeapLru());
		assertTrue(RegionShortcutWrapper.PARTITION_HEAP_LRU.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION_OVERFLOW.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT_OVERFLOW.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY_REDUNDANT.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT.isHeapLru());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_HEAP_LRU.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_OVERFLOW.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT.isHeapLru());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW.isHeapLru());
		assertFalse(RegionShortcutWrapper.REPLICATE.isHeapLru());
		assertTrue(RegionShortcutWrapper.REPLICATE_HEAP_LRU.isHeapLru());
		assertFalse(RegionShortcutWrapper.REPLICATE_OVERFLOW.isHeapLru());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT.isHeapLru());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT_OVERFLOW.isHeapLru());
		assertFalse(RegionShortcutWrapper.REPLICATE_PROXY.isHeapLru());
		assertFalse(RegionShortcutWrapper.UNSPECIFIED.isHeapLru());
	}

	@Test
	public void testIsLocal() {
		assertTrue(RegionShortcutWrapper.LOCAL.isLocal());
		assertTrue(RegionShortcutWrapper.LOCAL_HEAP_LRU.isLocal());
		assertTrue(RegionShortcutWrapper.LOCAL_OVERFLOW.isLocal());
		assertTrue(RegionShortcutWrapper.LOCAL_PERSISTENT.isLocal());
		assertTrue(RegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_HEAP_LRU.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_OVERFLOW.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT_OVERFLOW.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY_REDUNDANT.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_HEAP_LRU.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_OVERFLOW.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT.isLocal());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW.isLocal());
		assertFalse(RegionShortcutWrapper.REPLICATE.isLocal());
		assertFalse(RegionShortcutWrapper.REPLICATE_HEAP_LRU.isLocal());
		assertFalse(RegionShortcutWrapper.REPLICATE_OVERFLOW.isLocal());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT.isLocal());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT_OVERFLOW.isLocal());
		assertFalse(RegionShortcutWrapper.REPLICATE_PROXY.isLocal());
		assertFalse(RegionShortcutWrapper.UNSPECIFIED.isLocal());
	}

	@Test
	public void testIsOverflow() {
		assertFalse(RegionShortcutWrapper.LOCAL.isOverflow());
		assertFalse(RegionShortcutWrapper.LOCAL_HEAP_LRU.isOverflow());
		assertTrue(RegionShortcutWrapper.LOCAL_OVERFLOW.isOverflow());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT.isOverflow());
		assertTrue(RegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION.isOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_HEAP_LRU.isOverflow());
		assertTrue(RegionShortcutWrapper.PARTITION_OVERFLOW.isOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT.isOverflow());
		assertTrue(RegionShortcutWrapper.PARTITION_PERSISTENT_OVERFLOW.isOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY.isOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY_REDUNDANT.isOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT.isOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_HEAP_LRU.isOverflow());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_OVERFLOW.isOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT.isOverflow());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW.isOverflow());
		assertFalse(RegionShortcutWrapper.REPLICATE.isOverflow());
		assertFalse(RegionShortcutWrapper.REPLICATE_HEAP_LRU.isOverflow());
		assertTrue(RegionShortcutWrapper.REPLICATE_OVERFLOW.isOverflow());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT.isOverflow());
		assertTrue(RegionShortcutWrapper.REPLICATE_PERSISTENT_OVERFLOW.isOverflow());
		assertFalse(RegionShortcutWrapper.REPLICATE_PROXY.isOverflow());
		assertFalse(RegionShortcutWrapper.UNSPECIFIED.isOverflow());
	}

	@Test
	public void testIsPartition() {
		assertFalse(RegionShortcutWrapper.LOCAL.isPartition());
		assertFalse(RegionShortcutWrapper.LOCAL_HEAP_LRU.isPartition());
		assertFalse(RegionShortcutWrapper.LOCAL_OVERFLOW.isPartition());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT.isPartition());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_HEAP_LRU.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_OVERFLOW.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_PERSISTENT.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_PERSISTENT_OVERFLOW.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_PROXY.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_PROXY_REDUNDANT.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_HEAP_LRU.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_OVERFLOW.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT.isPartition());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW.isPartition());
		assertFalse(RegionShortcutWrapper.REPLICATE.isPartition());
		assertFalse(RegionShortcutWrapper.REPLICATE_HEAP_LRU.isPartition());
		assertFalse(RegionShortcutWrapper.REPLICATE_OVERFLOW.isPartition());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT.isPartition());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT_OVERFLOW.isPartition());
		assertFalse(RegionShortcutWrapper.REPLICATE_PROXY.isPartition());
		assertFalse(RegionShortcutWrapper.UNSPECIFIED.isPartition());
	}

	@Test
	public void testIsPersistent() {
		assertFalse(RegionShortcutWrapper.LOCAL.isPersistent());
		assertFalse(RegionShortcutWrapper.LOCAL_HEAP_LRU.isPersistent());
		assertFalse(RegionShortcutWrapper.LOCAL_OVERFLOW.isPersistent());
		assertTrue(RegionShortcutWrapper.LOCAL_PERSISTENT.isPersistent());
		assertTrue(RegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isPersistent());
		assertFalse(RegionShortcutWrapper.PARTITION.isPersistent());
		assertFalse(RegionShortcutWrapper.PARTITION_HEAP_LRU.isPersistent());
		assertFalse(RegionShortcutWrapper.PARTITION_OVERFLOW.isPersistent());
		assertTrue(RegionShortcutWrapper.PARTITION_PERSISTENT.isPersistent());
		assertTrue(RegionShortcutWrapper.PARTITION_PERSISTENT_OVERFLOW.isPersistent());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY.isPersistent());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY_REDUNDANT.isPersistent());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT.isPersistent());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_HEAP_LRU.isPersistent());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_OVERFLOW.isPersistent());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT.isPersistent());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW.isPersistent());
		assertFalse(RegionShortcutWrapper.REPLICATE.isPersistent());
		assertFalse(RegionShortcutWrapper.REPLICATE_HEAP_LRU.isPersistent());
		assertFalse(RegionShortcutWrapper.REPLICATE_OVERFLOW.isPersistent());
		assertTrue(RegionShortcutWrapper.REPLICATE_PERSISTENT.isPersistent());
		assertTrue(RegionShortcutWrapper.REPLICATE_PERSISTENT_OVERFLOW.isPersistent());
		assertFalse(RegionShortcutWrapper.REPLICATE_PROXY.isPersistent());
		assertFalse(RegionShortcutWrapper.UNSPECIFIED.isPersistent());
	}

	@Test
	public void testIsPersistentOverflow() {
		assertFalse(RegionShortcutWrapper.LOCAL.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.LOCAL_HEAP_LRU.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.LOCAL_OVERFLOW.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT.isPersistentOverflow());
		assertTrue(RegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_HEAP_LRU.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_OVERFLOW.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT.isPersistentOverflow());
		assertTrue(RegionShortcutWrapper.PARTITION_PERSISTENT_OVERFLOW.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY_REDUNDANT.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_HEAP_LRU.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_OVERFLOW.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT.isPersistentOverflow());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.REPLICATE.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.REPLICATE_HEAP_LRU.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.REPLICATE_OVERFLOW.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT.isPersistentOverflow());
		assertTrue(RegionShortcutWrapper.REPLICATE_PERSISTENT_OVERFLOW.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.REPLICATE_PROXY.isPersistentOverflow());
		assertFalse(RegionShortcutWrapper.UNSPECIFIED.isPersistentOverflow());
	}
	@Test
	public void testIsProxy() {
		assertFalse(RegionShortcutWrapper.LOCAL.isProxy());
		assertFalse(RegionShortcutWrapper.LOCAL_HEAP_LRU.isProxy());
		assertFalse(RegionShortcutWrapper.LOCAL_OVERFLOW.isProxy());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT.isProxy());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION_HEAP_LRU.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION_OVERFLOW.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT_OVERFLOW.isProxy());
		assertTrue(RegionShortcutWrapper.PARTITION_PROXY.isProxy());
		assertTrue(RegionShortcutWrapper.PARTITION_PROXY_REDUNDANT.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_HEAP_LRU.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_OVERFLOW.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT.isProxy());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW.isProxy());
		assertFalse(RegionShortcutWrapper.REPLICATE.isProxy());
		assertFalse(RegionShortcutWrapper.REPLICATE_HEAP_LRU.isProxy());
		assertFalse(RegionShortcutWrapper.REPLICATE_OVERFLOW.isProxy());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT.isProxy());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT_OVERFLOW.isProxy());
		assertTrue(RegionShortcutWrapper.REPLICATE_PROXY.isProxy());
		assertFalse(RegionShortcutWrapper.UNSPECIFIED.isProxy());
	}
	@Test
	public void testIsRedundant() {
		assertFalse(RegionShortcutWrapper.LOCAL.isRedundant());
		assertFalse(RegionShortcutWrapper.LOCAL_HEAP_LRU.isRedundant());
		assertFalse(RegionShortcutWrapper.LOCAL_OVERFLOW.isRedundant());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT.isRedundant());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isRedundant());
		assertFalse(RegionShortcutWrapper.PARTITION.isRedundant());
		assertFalse(RegionShortcutWrapper.PARTITION_HEAP_LRU.isRedundant());
		assertFalse(RegionShortcutWrapper.PARTITION_OVERFLOW.isRedundant());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT.isRedundant());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT_OVERFLOW.isRedundant());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY.isRedundant());
		assertTrue(RegionShortcutWrapper.PARTITION_PROXY_REDUNDANT.isRedundant());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT.isRedundant());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_HEAP_LRU.isRedundant());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_OVERFLOW.isRedundant());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT.isRedundant());
		assertTrue(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW.isRedundant());
		assertFalse(RegionShortcutWrapper.REPLICATE.isRedundant());
		assertFalse(RegionShortcutWrapper.REPLICATE_HEAP_LRU.isRedundant());
		assertFalse(RegionShortcutWrapper.REPLICATE_OVERFLOW.isRedundant());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT.isRedundant());
		assertFalse(RegionShortcutWrapper.REPLICATE_PERSISTENT_OVERFLOW.isRedundant());
		assertFalse(RegionShortcutWrapper.REPLICATE_PROXY.isRedundant());
		assertFalse(RegionShortcutWrapper.UNSPECIFIED.isRedundant());
	}

	@Test
	public void testIsReplicate() {
		assertFalse(RegionShortcutWrapper.LOCAL.isReplicate());
		assertFalse(RegionShortcutWrapper.LOCAL_HEAP_LRU.isReplicate());
		assertFalse(RegionShortcutWrapper.LOCAL_OVERFLOW.isReplicate());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT.isReplicate());
		assertFalse(RegionShortcutWrapper.LOCAL_PERSISTENT_OVERFLOW.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_HEAP_LRU.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_OVERFLOW.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_PERSISTENT_OVERFLOW.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_PROXY_REDUNDANT.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_HEAP_LRU.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_OVERFLOW.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT.isReplicate());
		assertFalse(RegionShortcutWrapper.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW.isReplicate());
		assertTrue(RegionShortcutWrapper.REPLICATE.isReplicate());
		assertTrue(RegionShortcutWrapper.REPLICATE_HEAP_LRU.isReplicate());
		assertTrue(RegionShortcutWrapper.REPLICATE_OVERFLOW.isReplicate());
		assertTrue(RegionShortcutWrapper.REPLICATE_PERSISTENT.isReplicate());
		assertTrue(RegionShortcutWrapper.REPLICATE_PERSISTENT_OVERFLOW.isReplicate());
		assertTrue(RegionShortcutWrapper.REPLICATE_PROXY.isReplicate());
		assertFalse(RegionShortcutWrapper.UNSPECIFIED.isReplicate());
	}
}
