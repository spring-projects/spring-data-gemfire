/*
 * Copyright 2017-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.assertj.core.api.Assertions.assertThat;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.RegionShortcut;

import org.junit.Test;

/**
 * Unit tests for {@link RegionShortcutToDataPolicyConverter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.RegionShortcut
 * @see org.springframework.data.gemfire.RegionShortcutToDataPolicyConverter
 * @since 2.0.2
 */
public class RegionShortcutToDataPolicyConverterUnitTests {

	protected void assertDataPolicy(DataPolicy actual, DataPolicy expected) {
		assertThat(actual).isEqualTo(expected);
	}

	protected void assertDataPolicyDefault(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.DEFAULT);
	}

	protected void assertDataPolicyEmpty(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.EMPTY);
	}

	protected void assertDataPolicyNormal(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.NORMAL);
	}

	protected void assertDataPolicyPersistentPartition(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.PERSISTENT_PARTITION);
	}

	protected void assertDataPolicyPersistentReplicate(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.PERSISTENT_REPLICATE);
	}

	protected void assertDataPolicyPartition(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.PARTITION);
	}

	protected void assertDataPolicyReplicate(DataPolicy actual) {
		assertDataPolicy(actual, DataPolicy.REPLICATE);
	}

	protected DataPolicy convert(RegionShortcut regionShortcut) {
		return RegionShortcutToDataPolicyConverter.INSTANCE.convert(regionShortcut);
	}

	@Test
	public void nullRegionShortcutIsDataPolicyDefault() {
		assertDataPolicyDefault(convert(null));
	}

	@Test
	public void regionShortcutLocalIsDataPolicyNormal() {
		assertDataPolicyNormal(convert(RegionShortcut.LOCAL));
	}

	@Test
	public void regionShortcutLocalHeapLruIsDataPolicyNormal() {
		assertDataPolicyNormal(convert(RegionShortcut.LOCAL_HEAP_LRU));
	}

	@Test
	public void regionShortcutLocalOverflowIsDataPolicyNormal() {
		assertDataPolicyNormal(convert(RegionShortcut.LOCAL_HEAP_LRU));
	}

	@Test
	public void regionShortcutLocalPersistentIsDataPolicyPersistentReplicate() {
		assertDataPolicyPersistentReplicate(convert(RegionShortcut.LOCAL_PERSISTENT));
	}

	@Test
	public void regionShortcutLocalPersistentOverflowIsDataPolicyPersistentReplicate() {
		assertDataPolicyPersistentReplicate(convert(RegionShortcut.LOCAL_PERSISTENT_OVERFLOW));
	}

	@Test
	public void regionShortcutPartitionIsDataPolicyPartition() {
		assertDataPolicyPartition(convert(RegionShortcut.PARTITION));
	}

	@Test
	public void regionShortcutPartitionHeapLruIsDataPolicyPartition() {
		assertDataPolicyPartition(convert(RegionShortcut.PARTITION_HEAP_LRU));
	}

	@Test
	public void regionShortcutPartitionOverflowIsDataPolicyPartition() {
		assertDataPolicyPartition(convert(RegionShortcut.PARTITION_OVERFLOW));
	}

	@Test
	public void regionShortcutPartitionPersistentIsDataPolicyPersistentPartition() {
		assertDataPolicyPersistentPartition(convert(RegionShortcut.PARTITION_PERSISTENT));
	}

	@Test
	public void regionShortcutPartitionPersistentOverflowIsDataPolicyPersistentPartition() {
		assertDataPolicyPersistentPartition(convert(RegionShortcut.PARTITION_PERSISTENT_OVERFLOW));
	}

	@Test
	public void regionShortcutPartitionProxyIsDataPolicyPartition() {
		assertDataPolicyPartition(convert(RegionShortcut.PARTITION_PROXY));
	}

	@Test
	public void regionShortcutPartitionProxyRedundantIsDataPolicyPartition() {
		assertDataPolicyPartition(convert(RegionShortcut.PARTITION_PROXY_REDUNDANT));
	}

	@Test
	public void regionShortcutPartitionRedundantIsDataPolicyPartition() {
		assertDataPolicyPartition(convert(RegionShortcut.PARTITION_REDUNDANT));
	}

	@Test
	public void regionShortcutPartitionRedundantHeapLruIsDataPolicyPartition() {
		assertDataPolicyPartition(convert(RegionShortcut.PARTITION_REDUNDANT_HEAP_LRU));
	}

	@Test
	public void regionShortcutPartitionRedundantOverflowIsDataPolicyPartition() {
		assertDataPolicyPartition(convert(RegionShortcut.PARTITION_REDUNDANT_HEAP_LRU));
	}

	@Test
	public void regionShortcutPartitionRedundantPersistentIsDataPolicyPartition() {
		assertDataPolicyPersistentPartition(convert(RegionShortcut.PARTITION_REDUNDANT_PERSISTENT));
	}

	@Test
	public void regionShortcutPartitionRedundantPersistentOverflowIsDataPolicyPartition() {
		assertDataPolicyPersistentPartition(convert(RegionShortcut.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW));
	}

	@Test
	public void regionShortcutReplicateIsDataPolicyReplicate() {
		assertDataPolicyReplicate(convert(RegionShortcut.REPLICATE));
	}

	@Test
	public void regionShortcutReplicateHeapLruIsDataPolicyReplicate() {
		assertDataPolicyReplicate(convert(RegionShortcut.REPLICATE_HEAP_LRU));
	}

	@Test
	public void regionShortcutReplicateOverflowIsDataPolicyReplicate() {
		assertDataPolicyReplicate(convert(RegionShortcut.REPLICATE_OVERFLOW));
	}

	@Test
	public void regionShortcutReplicatePersistentIsDataPolicyPersistentReplicate() {
		assertDataPolicyPersistentReplicate(convert(RegionShortcut.REPLICATE_PERSISTENT));
	}

	@Test
	public void regionShortcutReplicatePersistentOverflowIsDataPolicyPersistentReplicate() {
		assertDataPolicyPersistentReplicate(convert(RegionShortcut.REPLICATE_PERSISTENT_OVERFLOW));
	}

	@Test
	public void regionShortcutReplicateProxyIsDataPolicyPersistentReplicate() {
		assertDataPolicyEmpty(convert(RegionShortcut.REPLICATE_PROXY));
	}
}
