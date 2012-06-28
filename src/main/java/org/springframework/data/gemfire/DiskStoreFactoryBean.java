/*
 * Copyright 2010-2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import java.io.File;
import java.util.List;

import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.DiskStoreFactory;
import com.gemstone.gemfire.cache.GemFireCache;

/**
 * FactoryBean for creating a DiskStore
 * @author David Turanski
 */
public class DiskStoreFactoryBean implements FactoryBean<DiskStore>, InitializingBean, BeanNameAware {

	private DiskStoreFactory diskStoreFactory;

	private Boolean autoCompact;

	private Boolean allowForceCompaction;

	private Integer maxOplogSize;

	private Integer timeInterval;

	private Integer queueSize;

	private Integer compactionThreshold;

	private Integer writeBufferSize;

	private GemFireCache cache;

	private String name;

	private List<DiskDir> diskDirs;

	@Override
	public DiskStore getObject() throws Exception {
		return diskStoreFactory.create(name == null ? DiskStoreFactory.DEFAULT_DISK_STORE_NAME : name);
	}

	@Override
	public Class<?> getObjectType() {
		return DiskStore.class;
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		Assert.notNull(cache, "Cache property must be set");
		diskStoreFactory = cache.createDiskStoreFactory();

		if (allowForceCompaction != null) {
			diskStoreFactory.setAllowForceCompaction(allowForceCompaction);
		}
		if (compactionThreshold != null) {
			diskStoreFactory.setCompactionThreshold(compactionThreshold);
		}
		if (autoCompact != null) {
			diskStoreFactory.setAutoCompact(autoCompact);
		}
		if (queueSize != null) {
			diskStoreFactory.setQueueSize(queueSize);
		}
		if (writeBufferSize != null) {
			diskStoreFactory.setWriteBufferSize(writeBufferSize);
		}
		if (timeInterval != null) {
			diskStoreFactory.setTimeInterval(timeInterval);
		}
		if (maxOplogSize != null) {
			diskStoreFactory.setMaxOplogSize(maxOplogSize);
		}

		if (!CollectionUtils.isEmpty(diskDirs)) {
			File[] diskDirFiles = new File[diskDirs.size()];
			int[] diskDirSizes = new int[diskDirs.size()];
			for (int i = 0; i < diskDirs.size(); i++) {
				DiskDir diskDir = diskDirs.get(i);
				diskDirFiles[i] = new File(diskDir.location);
				diskDirSizes[i] = diskDir.maxSize == null ? DiskStoreFactory.DEFAULT_DISK_DIR_SIZE : diskDir.maxSize;
			}
			diskStoreFactory.setDiskDirsAndSizes(diskDirFiles, diskDirSizes);
		}
	}

	public void setCache(GemFireCache cache) {
		this.cache = cache;
	}

	public void setAutoCompact(Boolean autoCompact) {
		this.autoCompact = autoCompact;
	}

	public void setAllowForceCompaction(Boolean allowForceCompaction) {
		this.allowForceCompaction = allowForceCompaction;
	}

	public void setMaxOplogSize(Integer maxOplogSize) {
		this.maxOplogSize = maxOplogSize;
	}

	public void setTimeInterval(Integer timeInterval) {
		this.timeInterval = timeInterval;
	}

	public void setQueueSize(Integer queueSize) {
		this.queueSize = queueSize;
	}

	public void setCompactionThreshold(Integer compactionThreshold) {
		this.compactionThreshold = compactionThreshold;
	}

	public void setWriteBufferSize(Integer writeBufferSize) {
		this.writeBufferSize = writeBufferSize;
	}

	public void setDiskDirs(List<DiskDir> diskDirs) {
		this.diskDirs = diskDirs;
	}

	@Override
	public void setBeanName(String name) {
		this.name = name;
	}

	public static class DiskDir {
		final String location;

		final Integer maxSize;

		public DiskDir(String location, int maxSize) {
			this.location = location;
			this.maxSize = maxSize;
		}

		public DiskDir(String location) {
			this.location = location;
			this.maxSize = null;
		}
	}
}
