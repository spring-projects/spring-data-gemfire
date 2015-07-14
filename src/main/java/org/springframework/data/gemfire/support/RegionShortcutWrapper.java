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

package org.springframework.data.gemfire.support;

import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.RegionShortcut;

/**
 * The RegionShortcutWrapper enum is a Java enumerated type that wraps GemFire's RegionShortcuts
 * with Spring Data GemFire RegionShortcutWrapper enumerated values.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.cache.RegionShortcut
 * @since 1.4.0
 */
@SuppressWarnings("unused")
public enum RegionShortcutWrapper {
	LOCAL(RegionShortcut.LOCAL),
	LOCAL_HEAP_LRU(RegionShortcut.LOCAL_HEAP_LRU),
	LOCAL_OVERFLOW(RegionShortcut.LOCAL_OVERFLOW),
	LOCAL_PERSISTENT(RegionShortcut.LOCAL_PERSISTENT),
	LOCAL_PERSISTENT_OVERFLOW(RegionShortcut.LOCAL_PERSISTENT_OVERFLOW),
	PARTITION(RegionShortcut.PARTITION),
	PARTITION_HDFS(RegionShortcut.PARTITION_HDFS),
	PARTITION_HEAP_LRU(RegionShortcut.PARTITION_HEAP_LRU),
	PARTITION_OVERFLOW(RegionShortcut.PARTITION_OVERFLOW),
	PARTITION_PERSISTENT(RegionShortcut.PARTITION_PERSISTENT),
	PARTITION_PERSISTENT_OVERFLOW(RegionShortcut.PARTITION_PERSISTENT_OVERFLOW),
	PARTITION_PROXY(RegionShortcut.PARTITION_PROXY),
	PARTITION_PROXY_REDUNDANT(RegionShortcut.PARTITION_PROXY_REDUNDANT),
	PARTITION_REDUNDANT(RegionShortcut.PARTITION_REDUNDANT),
	PARTITION_REDUNDANT_HEAP_LRU(RegionShortcut.PARTITION_REDUNDANT_HEAP_LRU),
	PARTITION_REDUNDANT_HDFS(RegionShortcut.PARTITION_REDUNDANT_HDFS),
	PARTITION_REDUNDANT_OVERFLOW(RegionShortcut.PARTITION_REDUNDANT_OVERFLOW),
	PARTITION_REDUNDANT_PERSISTENT(RegionShortcut.PARTITION_REDUNDANT_PERSISTENT),
	PARTITION_REDUNDANT_PERSISTENT_OVERFLOW(RegionShortcut.PARTITION_REDUNDANT_PERSISTENT_OVERFLOW),
	PARTITION_REDUNDANT_WRITEONLY_HDFS_STORE(RegionShortcut.PARTITION_REDUNDANT_WRITEONLY_HDFS_STORE),
	PARTITION_WRITEONLY_HDFS_STORE(RegionShortcut.PARTITION_WRITEONLY_HDFS_STORE),
	REPLICATE(RegionShortcut.REPLICATE),
	REPLICATE_HEAP_LRU(RegionShortcut.REPLICATE_HEAP_LRU),
	REPLICATE_OVERFLOW(RegionShortcut.REPLICATE_OVERFLOW),
	REPLICATE_PERSISTENT(RegionShortcut.REPLICATE_PERSISTENT),
	REPLICATE_PERSISTENT_OVERFLOW(RegionShortcut.REPLICATE_PERSISTENT_OVERFLOW),
	REPLICATE_PROXY(RegionShortcut.REPLICATE_PROXY),
	UNSPECIFIED(null);

	private final RegionShortcut regionShortcut;

	RegionShortcutWrapper(final RegionShortcut regionShortcut) {
		this.regionShortcut = regionShortcut;
	}

	public static RegionShortcutWrapper valueOf(final RegionShortcut regionShortcut) {
		for (RegionShortcutWrapper wrapper : values()) {
			if (ObjectUtils.nullSafeEquals(wrapper.getRegionShortcut(), regionShortcut)) {
				return wrapper;
			}
		}

		return RegionShortcutWrapper.UNSPECIFIED;
	}

	public boolean isHeapLru() {
		return name().contains("HEAP_LRU");
	}

	public boolean isLocal() {
		return name().contains("LOCAL");
	}

	public boolean isOverflow() {
		return name().contains("OVERFLOW");
	}

	public boolean isPartition() {
		return name().contains("PARTITION");
	}

	public boolean isPersistent() {
		return name().contains("PERSISTENT");
	}

	public boolean isPersistentOverflow() {
		return (isOverflow() && isPersistent());
	}

	public boolean isProxy() {
		return name().contains("PROXY");
	}

	public boolean isRedundant() {
		return name().contains("REDUNDANT");
	}

	public boolean isReplicate() {
		return name().contains("REPLICATE");
	}

	public RegionShortcut getRegionShortcut() {
		return regionShortcut;
	}

}
