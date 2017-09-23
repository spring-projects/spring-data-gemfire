/*
 * Copyright 2016 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.DiskStore;
import org.junit.After;
import org.junit.Test;
import org.mockito.stubbing.Answer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;

/**
 * Unit tests for the {@link EnableDiskStore} and {@link EnableDiskStores} annotations as well as
 * the {@link DiskStoreConfiguration} and {@link DiskStoresConfiguration} classes.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.DiskStore
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStore
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStores
 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration
 * @see org.springframework.data.gemfire.config.annotation.DiskStoresConfiguration
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @since 1.9.0
 */
public class EnableDiskStoresConfigurationUnitTests {

	private static final AtomicInteger MOCK_ID = new AtomicInteger(0);

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	/* (non-Javadoc) */
	private void assertDiskStore(DiskStore diskStore, String name, boolean allowForceCompaction, boolean autoCompact,
			int compactionThreshold, float diskUsageCriticalPercentage, float diskUsageWarningPercentage,
			long maxOplogSize, int queueSize, long timeInterval, int writeBufferSize) {

		assertThat(diskStore).isNotNull();
		assertThat(diskStore.getName()).isEqualTo(name);
		assertThat(diskStore.getAllowForceCompaction()).isEqualTo(allowForceCompaction);
		assertThat(diskStore.getAutoCompact()).isEqualTo(autoCompact);
		assertThat(diskStore.getCompactionThreshold()).isEqualTo(compactionThreshold);
		assertThat(diskStore.getDiskUsageCriticalPercentage()).isEqualTo(diskUsageCriticalPercentage);
		assertThat(diskStore.getDiskUsageWarningPercentage()).isEqualTo(diskUsageWarningPercentage);
		assertThat(diskStore.getMaxOplogSize()).isEqualTo(maxOplogSize);
		assertThat(diskStore.getQueueSize()).isEqualTo(queueSize);
		assertThat(diskStore.getTimeInterval()).isEqualTo(timeInterval);
		assertThat(diskStore.getWriteBufferSize()).isEqualTo(writeBufferSize);
	}

	/* (non-Javadoc) */
	private void assertDiskStoreDirectoryLocations(DiskStore diskStore, File... diskDirectories) {

		assertThat(diskStore).isNotNull();

		File[] diskStoreDirectories = diskStore.getDiskDirs();

		assertThat(diskStoreDirectories).isNotNull();
		assertThat(diskStoreDirectories.length).isEqualTo(diskDirectories.length);

		int index = 0;

		for (File diskDirectory : diskDirectories) {
			assertThat(diskStoreDirectories[index++]).isEqualTo(diskDirectory);
		}
	}

	/* (non-Javadoc) */
	private void assertDiskStoreDirectorySizes(DiskStore diskStore, int... diskDirectorySizes) {

		assertThat(diskStore).isNotNull();

		int[] diskStoreDirectorySizes = diskStore.getDiskDirSizes();

		assertThat(diskStoreDirectorySizes).isNotNull();
		assertThat(diskStoreDirectorySizes.length).isEqualTo(diskDirectorySizes.length);

		int index = 0;

		for (int size : diskDirectorySizes) {
			assertThat(diskStoreDirectorySizes[index++]).isEqualTo(size);
		}
	}

	/* (non-Javadoc) */
	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	/* (non-Javadoc) */
	private File newFile(String location) {
		return new File(location);
	}

	@Test
	public void enableSingleDiskStore() {

		this.applicationContext = newApplicationContext(SingleDiskStoreConfiguration.class);

		DiskStore testDiskStore = this.applicationContext.getBean("TestDiskStore", DiskStore.class);

		assertDiskStore(testDiskStore, "TestDiskStore", true, true, 75, 95.0f, 75.0f, 8192L, 100, 2000L, 65536);
		assertDiskStoreDirectoryLocations(testDiskStore, newFile("/absolute/path/to/gemfire/disk/directory"),
			newFile("relative/path/to/gemfire/disk/directory"));
		assertDiskStoreDirectorySizes(testDiskStore, 1024, 4096);
	}

	@Test
	public void enableMultipleDiskStores() {

		this.applicationContext = newApplicationContext(MultipleDiskStoresConfiguration.class);

		DiskStore testDiskStoreOne = this.applicationContext.getBean("TestDiskStoreOne", DiskStore.class);

		assertDiskStore(testDiskStoreOne, "TestDiskStoreOne", false, true, 75, 99.0f, 90.0f, 2048L, 100, 1000L, 32768);

		DiskStore testDiskStoreTwo = this.applicationContext.getBean("TestDiskStoreTwo", DiskStore.class);

		assertDiskStore(testDiskStoreTwo, "TestDiskStoreTwo", true, true, 85, 99.0f, 90.0f, 4096L, 0, 1000L, 32768);
	}

	static String mockName(String baseName) {
		return String.format("%s%d", baseName, MOCK_ID.incrementAndGet());
	}

	/* (non-Javadoc) */
	protected static <R> Answer<R> newGetter(AtomicReference<R> returnValue) {
		return invocation -> returnValue.get();
	}

	/* (non-Javadoc) */
	protected static <T, R> Answer<R> newSetter(Class<T> parameterType, AtomicReference<T> argument, R returnValue) {
		return invocation -> {
			argument.set(invocation.getArgument(0));
			return returnValue;
		};
	}

	@PeerCacheApplication
	@EnableGemFireMockObjects
	@EnableDiskStore(name = "TestDiskStore", allowForceCompaction = true, autoCompact = true, compactionThreshold = 75,
		diskUsageCriticalPercentage = 95.0f, diskUsageWarningPercentage = 75.0f, maxOplogSize = 8192L, queueSize = 100,
		timeInterval = 2000L, writeBufferSize = 65536, diskDirectories = {
			@EnableDiskStore.DiskDirectory(location = "/absolute/path/to/gemfire/disk/directory", maxSize = 1024),
			@EnableDiskStore.DiskDirectory(location = "relative/path/to/gemfire/disk/directory", maxSize = 4096)
	})
	static class SingleDiskStoreConfiguration {

	}

	@PeerCacheApplication
	@EnableGemFireMockObjects
	@EnableDiskStores(autoCompact = true, compactionThreshold = 75, maxOplogSize = 2048L, diskStores = {
		@EnableDiskStore(name = "TestDiskStoreOne", queueSize = 100),
		@EnableDiskStore(name = "TestDiskStoreTwo", allowForceCompaction = true,
			compactionThreshold = 85, maxOplogSize = 4096)
	})
	static class MultipleDiskStoresConfiguration {

	}
}
