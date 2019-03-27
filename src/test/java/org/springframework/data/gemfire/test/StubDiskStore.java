/*
 * Copyright 2002-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.DiskStoreFactory;

/**
 * @author David Turanski
 * @author John Blum
 */
public class StubDiskStore implements DiskStoreFactory {

	private static Map<String, DiskStore> diskStores = new HashMap<String, DiskStore>();

	private boolean allowForceCompaction;
	private boolean autoCompact;

	private float diskUsageCriticalPercentage;
	private float diskUsageWarningPercentage;

	private int compactionThreshold;
	private int queueSize;
	private int writeBufferSize;

	private int[] diskDirSizes;

	private long maxOpLogSize;
	private long timeInterval;

	private File[] diskDirs;

	public static DiskStore getDiskStore(final String name) {
		return diskStores.get(name);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setAllowForceCompaction(boolean)
	 */
	@Override
	public DiskStoreFactory setAllowForceCompaction(boolean allowForceCompaction) {
		this.allowForceCompaction = allowForceCompaction;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setAutoCompact(boolean)
	 */
	@Override
	public DiskStoreFactory setAutoCompact(boolean isAutoCompact) {
		this.autoCompact = isAutoCompact;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setCompactionThreshold(int)
	 */
	@Override
	public DiskStoreFactory setCompactionThreshold(int compactionThreshold) {
		this.compactionThreshold = compactionThreshold;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setMaxOplogSize(long)
	 */
	@Override
	public DiskStoreFactory setMaxOplogSize(long maxOplogSize) {
		this.maxOpLogSize = maxOplogSize;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setTimeInterval(long)
	 */
	@Override
	public DiskStoreFactory setTimeInterval(long timeInterval) {
		this.timeInterval = timeInterval;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setWriteBufferSize(int)
	 */
	@Override
	public DiskStoreFactory setWriteBufferSize(int writeBufferSize) {
		this.writeBufferSize = writeBufferSize;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setQueueSize(int)
	 */
	@Override
	public DiskStoreFactory setQueueSize(int queueSize) {
		this.queueSize = queueSize;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setDiskDirs(java.io.File[])
	 */
	@Override
	public DiskStoreFactory setDiskDirs(File[] diskDirs) {
		this.diskDirs = diskDirs;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setDiskDirsAndSizes(java.io.File[], int[])
	 */
	@Override
	public DiskStoreFactory setDiskDirsAndSizes(File[] diskDirs, int[] diskDirSizes) {
		this.diskDirs = diskDirs;
		this.diskDirSizes = diskDirSizes;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setDiskUsageCriticalPercentage(float)
	 */
	@Override
	public DiskStoreFactory setDiskUsageCriticalPercentage(final float diskUsageCriticalPercentage) {
		this.diskUsageCriticalPercentage = diskUsageCriticalPercentage;
		return this;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.DiskStoreFactory#setDiskUsageWarningPercentage(float)
	 */
	@Override
	public DiskStoreFactory setDiskUsageWarningPercentage(final float diskUsageWarningPercentage) {
		this.diskUsageWarningPercentage = diskUsageWarningPercentage;
		return this;
	}

	/* (non-Javadoc)
		 * @see org.apache.geode.cache.DiskStoreFactory#create(java.lang.String)
		 */
	@Override
	public DiskStore create(final String name) {
		DiskStore mockDiskStore = mock(DiskStore.class, name);

		when(mockDiskStore.getName()).thenReturn(name);
		when(mockDiskStore.getAllowForceCompaction()).thenReturn(allowForceCompaction);
		when(mockDiskStore.getAutoCompact()).thenReturn(autoCompact);
		when(mockDiskStore.getCompactionThreshold()).thenReturn(compactionThreshold);
		when(mockDiskStore.getDiskUsageCriticalPercentage()).thenReturn(diskUsageCriticalPercentage);
		when(mockDiskStore.getDiskUsageWarningPercentage()).thenReturn(diskUsageWarningPercentage);
		when(mockDiskStore.getDiskDirs()).thenReturn(diskDirs);
		when(mockDiskStore.getDiskDirSizes()).thenReturn(diskDirSizes);
		when(mockDiskStore.getMaxOplogSize()).thenReturn(maxOpLogSize);
		when(mockDiskStore.getQueueSize()).thenReturn(queueSize);
		when(mockDiskStore.getTimeInterval()).thenReturn(timeInterval);
		when(mockDiskStore.getWriteBufferSize()).thenReturn(writeBufferSize);

		diskStores.put(name, mockDiskStore);

		return mockDiskStore;
	}

}
