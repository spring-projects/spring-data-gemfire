/*
 * Copyright 2010-2018 the original author or authors.
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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.EvictionAlgorithm;
import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The RegionShortcutsIntegrationTest class is a test suite of test cases testing the use of RegionShortcuts in the
 * Spring Data GemFire XML Namespace!
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.Region
 * @since 1.4.0
 */
@ContextConfiguration("region-datapolicy-shortcuts.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class RegionDataPolicyShortcutsIntegrationTest {

	@Resource(name = "LocalWithDataPolicy")
	private Region localWithDataPolicy;

	@Resource(name = "LocalWithShortcut")
	private Region localWithShortcut;

	@Resource(name = "PartitionWithDataPolicy")
	private Region partitionWithDataPolicy;

	@Resource(name = "PartitionWithShortcut")
	private Region partitionWithShortcut;

	@Resource(name = "ReplicateWithDataPolicy")
	private Region replicateWithDataPolicy;

	@Resource(name = "ReplicateWithShortcut")
	private Region replicateWithShortcut;

	@Resource(name = "ShortcutDefaults")
	private Region shortcutDefaults;

	@Resource(name = "ShortcutOverrides")
	private Region shortcutOverrides;

	@Test
	public void testLocalRegionWithDataPolicy() {
		assertNotNull("A reference to the 'LocalWithDataPolicy' Region was not property configured!", localWithDataPolicy);
		assertEquals("LocalWithDataPolicy", localWithDataPolicy.getName());
		assertEquals("/LocalWithDataPolicy", localWithDataPolicy.getFullPath());
		assertNotNull(localWithDataPolicy.getAttributes());
		assertEquals(DataPolicy.NORMAL, localWithDataPolicy.getAttributes().getDataPolicy());
	}

	@Test
	public void testLocalRegionWithShortcut() {
		assertNotNull("A reference to the 'LocalWithShortcut' Region was not property configured!", localWithShortcut);
		assertEquals("LocalWithShortcut", localWithShortcut.getName());
		assertEquals("/LocalWithShortcut", localWithShortcut.getFullPath());
		assertNotNull(localWithShortcut.getAttributes());
		assertEquals(DataPolicy.PERSISTENT_REPLICATE, localWithShortcut.getAttributes().getDataPolicy());
	}

	@Test
	public void testPartitionRegionWithDataPolicy() {
		assertNotNull("A reference to the 'PartitionWithDataPolicy' Region was not property configured!", partitionWithDataPolicy);
		assertEquals("PartitionWithDataPolicy", partitionWithDataPolicy.getName());
		assertEquals("/PartitionWithDataPolicy", partitionWithDataPolicy.getFullPath());
		assertNotNull(partitionWithDataPolicy.getAttributes());
		assertEquals(DataPolicy.PARTITION, partitionWithDataPolicy.getAttributes().getDataPolicy());
	}

	@Test
	public void testPartitionRegionWithShortcut() {
		assertNotNull("A reference to the 'PartitionWithShortcut' Region was not property configured!", partitionWithShortcut);
		assertEquals("PartitionWithShortcut", partitionWithShortcut.getName());
		assertEquals("/PartitionWithShortcut", partitionWithShortcut.getFullPath());
		assertNotNull(partitionWithShortcut.getAttributes());
		assertEquals(DataPolicy.PERSISTENT_PARTITION, partitionWithShortcut.getAttributes().getDataPolicy());
	}

	@Test
	public void testReplicateRegionWithDataPolicy() {
		assertNotNull("A reference to the 'ReplicateWithDataPolicy' Region was not property configured!", replicateWithDataPolicy);
		assertEquals("ReplicateWithDataPolicy", replicateWithDataPolicy.getName());
		assertEquals("/ReplicateWithDataPolicy", replicateWithDataPolicy.getFullPath());
		assertNotNull(replicateWithDataPolicy.getAttributes());
		assertEquals(DataPolicy.REPLICATE, replicateWithDataPolicy.getAttributes().getDataPolicy());
	}

	@Test
	public void testReplicateRegionWithShortcut() {
		assertNotNull("A reference to the 'ReplicateWithShortcut' Region was not property configured!", replicateWithShortcut);
		assertEquals("ReplicateWithShortcut", replicateWithShortcut.getName());
		assertEquals("/ReplicateWithShortcut", replicateWithShortcut.getFullPath());
		assertNotNull(replicateWithShortcut.getAttributes());
		assertEquals(DataPolicy.PERSISTENT_REPLICATE, replicateWithShortcut.getAttributes().getDataPolicy());
	}

	@Test
	public void testShortcutDefaultsRegion() {
		assertNotNull("A reference to the 'ShortcutDefaults' Region was not properly configured!", shortcutDefaults);
		assertEquals("ShortcutDefaults", shortcutDefaults.getName());
		assertEquals("/ShortcutDefaults", shortcutDefaults.getFullPath());

		assertNotNull(shortcutDefaults.getAttributes());
		assertFalse(shortcutDefaults.getAttributes().getCloningEnabled());
		assertTrue(shortcutDefaults.getAttributes().getConcurrencyChecksEnabled());
		assertEquals(DataPolicy.PERSISTENT_PARTITION, shortcutDefaults.getAttributes().getDataPolicy());
		assertFalse(shortcutDefaults.getAttributes().isDiskSynchronous());
		assertTrue(shortcutDefaults.getAttributes().getIgnoreJTA());
		assertEquals(101, shortcutDefaults.getAttributes().getInitialCapacity());
		assertEquals(new Float(0.85f), new Float(shortcutDefaults.getAttributes().getLoadFactor()));
		assertEquals(Long.class, shortcutDefaults.getAttributes().getKeyConstraint());
		assertEquals(String.class, shortcutDefaults.getAttributes().getValueConstraint());

		assertNotNull(shortcutDefaults.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, shortcutDefaults.getAttributes().getEvictionAttributes().getAction());
		assertEquals(EvictionAlgorithm.LRU_HEAP, shortcutDefaults.getAttributes().getEvictionAttributes().getAlgorithm());

		assertNotNull(shortcutDefaults.getAttributes().getPartitionAttributes());
		assertEquals(1, shortcutDefaults.getAttributes().getPartitionAttributes().getRedundantCopies());
		assertEquals(177, shortcutDefaults.getAttributes().getPartitionAttributes().getTotalNumBuckets());
	}

	@Test
	public void testShortcutOverridesRegion() {
		assertNotNull("A reference to the 'ShortcutOverrides' Region was not properly configured!", shortcutOverrides);
		assertEquals("ShortcutOverrides", shortcutOverrides.getName());
		assertEquals("/ShortcutOverrides", shortcutOverrides.getFullPath());

		assertNotNull(shortcutOverrides.getAttributes());
		assertTrue(shortcutOverrides.getAttributes().getCloningEnabled());
		assertFalse(shortcutOverrides.getAttributes().getConcurrencyChecksEnabled());
		assertEquals(DataPolicy.PARTITION, shortcutOverrides.getAttributes().getDataPolicy());
		assertTrue(shortcutOverrides.getAttributes().isDiskSynchronous());
		assertFalse(shortcutOverrides.getAttributes().getIgnoreJTA());
		assertEquals(51, shortcutOverrides.getAttributes().getInitialCapacity());
		assertEquals(new Float(0.72f), new Float(shortcutOverrides.getAttributes().getLoadFactor()));
		assertEquals(String.class, shortcutOverrides.getAttributes().getKeyConstraint());
		assertEquals(Object.class, shortcutOverrides.getAttributes().getValueConstraint());

		assertNotNull(shortcutOverrides.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.LOCAL_DESTROY, shortcutOverrides.getAttributes().getEvictionAttributes().getAction());
		assertEquals(8192, shortcutOverrides.getAttributes().getEvictionAttributes().getMaximum());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, shortcutOverrides.getAttributes().getEvictionAttributes().getAlgorithm());

		assertNotNull(shortcutOverrides.getAttributes().getPartitionAttributes());
		assertEquals(3, shortcutOverrides.getAttributes().getPartitionAttributes().getRedundantCopies());
		assertEquals(111, shortcutOverrides.getAttributes().getPartitionAttributes().getTotalNumBuckets());
	}

}
