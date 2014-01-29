/*
 * Copyright 2010-2013 the original author or authors.
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
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.DiskStoreFactory;
import com.gemstone.gemfire.cache.GemFireCache;

/**
 * FactoryBean for creating a GemFire DiskStore.
 * <p/>
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 */
@SuppressWarnings("unused")
public class DiskStoreFactoryBean implements FactoryBean<DiskStore>, InitializingBean, BeanNameAware {

	private DiskStoreFactory diskStoreFactory;

	private Boolean allowForceCompaction;
	private Boolean autoCompact;

	private DiskStore diskStore;

	private GemFireCache cache;

	private Integer compactionThreshold;
	private Integer maxOplogSize;
	private Integer queueSize;
	private Integer timeInterval;
	private Integer writeBufferSize;

	private List<DiskDir> diskDirs;

	private String name;

	@Override
	public DiskStore getObject() throws Exception {
		return diskStore;
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
		Assert.state(cache != null, String.format("A reference to the GemFire Cache must be set for Disk Store '%1$s'.",
			getName()));

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

		diskStore = diskStoreFactory.create(getName());

		Assert.notNull(diskStore, String.format("The DiskStore with name '%1$s' failed to be created successfully.",
			diskStore.getName()));
	}

	public void setCache(GemFireCache cache) {
		this.cache = cache;
	}

	public void setAllowForceCompaction(Boolean allowForceCompaction) {
		this.allowForceCompaction = allowForceCompaction;
	}

	public void setAutoCompact(Boolean autoCompact) {
		this.autoCompact = autoCompact;
	}

	@Override
	public void setBeanName(String name) {
		this.name = name;
	}

	public void setCompactionThreshold(Integer compactionThreshold) {
		validateCompactionThreshold(compactionThreshold);
		this.compactionThreshold = compactionThreshold;
	}

	protected void validateCompactionThreshold(final Integer compactionThreshold) {
		Assert.isTrue(compactionThreshold == null || (compactionThreshold >= 0 && compactionThreshold <= 100),
			String.format("The DiskStore's (%1$s) compaction threshold (%2$d) must be an integer value between 0 and 100 inclusive.",
				this.name, compactionThreshold));
	}

	public void setDiskDirs(List<DiskDir> diskDirs) {
		this.diskDirs = diskDirs;
	}

	public void setMaxOplogSize(Integer maxOplogSize) {
		this.maxOplogSize = maxOplogSize;
	}

	public void setQueueSize(Integer queueSize) {
		this.queueSize = queueSize;
	}

	public void setTimeInterval(Integer timeInterval) {
		this.timeInterval = timeInterval;
	}

	public void setWriteBufferSize(Integer writeBufferSize) {
		this.writeBufferSize = writeBufferSize;
	}

	/* package-private */ final String getName() {
		return (StringUtils.hasText(name) ? name : DiskStoreFactory.DEFAULT_DISK_STORE_NAME);
	}

	public static class DiskDir {
		final Integer maxSize;
		final String location;

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
