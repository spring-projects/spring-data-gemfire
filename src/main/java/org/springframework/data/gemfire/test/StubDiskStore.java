/*
 * Copyright 2002-2012 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.test;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.DiskStoreFactory;

/**
 * @author David Turanski
 *
 */
public class StubDiskStore implements DiskStore, DiskStoreFactory {
   static Map<String,DiskStore> diskStores = new HashMap<String,DiskStore>();
	

	private String name;
	private boolean autoCompact;
	private int compactionThreshold;
	private boolean allowForceCompaction;
	private long maxOpLogSize;
	private long timeInterval;
	private int writeBufferSize;
	private File[] diskDirs;
	private int[] diskDirSizes;
	private UUID diskStoreUUID;
	private int queueSize;

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getName()
	 */
	@Override
	public String getName() { 
		return this.name;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getAutoCompact()
	 */
	@Override
	public boolean getAutoCompact() {
		return this.autoCompact;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getCompactionThreshold()
	 */
	@Override
	public int getCompactionThreshold() {
		return this.compactionThreshold;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getAllowForceCompaction()
	 */
	@Override
	public boolean getAllowForceCompaction() {
		return this.allowForceCompaction;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getMaxOplogSize()
	 */
	@Override
	public long getMaxOplogSize() {
		return this.maxOpLogSize;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getTimeInterval()
	 */
	@Override
	public long getTimeInterval() {
		return this.timeInterval;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getWriteBufferSize()
	 */
	@Override
	public int getWriteBufferSize() {
		return this.writeBufferSize;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getDiskDirs()
	 */
	@Override
	public File[] getDiskDirs() {
		return this.diskDirs;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getDiskDirSizes()
	 */
	@Override
	public int[] getDiskDirSizes() {
		return this.diskDirSizes;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getDiskStoreUUID()
	 */
	@Override
	public UUID getDiskStoreUUID() {
		return this.diskStoreUUID;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#getQueueSize()
	 */
	@Override
	public int getQueueSize() {
		return this.queueSize;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#flush()
	 */
	@Override
	public void flush() {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#forceRoll()
	 */
	@Override
	public void forceRoll() {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStore#forceCompaction()
	 */
	@Override
	public boolean forceCompaction() {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#setAutoCompact(boolean)
	 */
	@Override
	public DiskStoreFactory setAutoCompact(boolean isAutoCompact) {
		this.autoCompact = isAutoCompact;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#setCompactionThreshold(int)
	 */
	@Override
	public DiskStoreFactory setCompactionThreshold(int compactionThreshold) {
		this.compactionThreshold = compactionThreshold;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#setAllowForceCompaction(boolean)
	 */
	@Override
	public DiskStoreFactory setAllowForceCompaction(boolean allowForceCompaction) {
		this.allowForceCompaction = allowForceCompaction;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#setMaxOplogSize(long)
	 */
	@Override
	public DiskStoreFactory setMaxOplogSize(long maxOplogSize) {
		this.maxOpLogSize = maxOplogSize;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#setTimeInterval(long)
	 */
	@Override
	public DiskStoreFactory setTimeInterval(long timeInterval) {
		this.timeInterval = timeInterval;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#setWriteBufferSize(int)
	 */
	@Override
	public DiskStoreFactory setWriteBufferSize(int writeBufferSize) {
		this.writeBufferSize = writeBufferSize;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#setQueueSize(int)
	 */
	@Override
	public DiskStoreFactory setQueueSize(int queueSize) {
		this.queueSize = queueSize;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#setDiskDirs(java.io.File[])
	 */
	@Override
	public DiskStoreFactory setDiskDirs(File[] diskDirs) {
		this.diskDirs = diskDirs;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#setDiskDirsAndSizes(java.io.File[], int[])
	 */
	@Override
	public DiskStoreFactory setDiskDirsAndSizes(File[] diskDirs, int[] diskDirSizes) {
		this.diskDirs = diskDirs;
		this.diskDirSizes = diskDirSizes;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.DiskStoreFactory#create(java.lang.String)
	 */
	@Override
	public DiskStore create(String name) {
		this.name = name;
		diskStores.put(name,this);
		return this;
	}
}
