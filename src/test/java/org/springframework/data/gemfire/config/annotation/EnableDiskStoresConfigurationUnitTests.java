/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyFloat;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.DiskStoreFactory;

import org.junit.After;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Unit tests for the {@link EnableDiskStore} and {@link EnableDiskStores} annotations as well as
 * the {@link DiskStoreConfiguration} and {@link DiskStoresConfiguration} classes.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @since 1.9.0
 */
public class EnableDiskStoresConfigurationUnitTests {

	private static final AtomicInteger MOCK_ID = new AtomicInteger(0);

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		if (applicationContext != null) {
			applicationContext.close();
		}
	}

	/* (non-Javadoc) */
	protected void assertDiskStore(DiskStore diskStore, String name, boolean allowForceCompaction, boolean autoCompact,
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
	protected void assertDiskStoreDirectoryLocations(DiskStore diskStore, File... diskDirectories) {
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
	protected void assertDiskStoreDirectorySizes(DiskStore diskStore, int... diskDirectorySizes) {
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
	protected File newFile(String location) {
		return new File(location);
	}

	/* (non-Javadoc) */
	protected ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	@Test
	public void enableSingleDiskStore() {
		applicationContext = newApplicationContext(SingleDiskStoreConfiguration.class);

		DiskStore testDiskStore = applicationContext.getBean("TestDiskStore", DiskStore.class);

		assertDiskStore(testDiskStore, "TestDiskStore", true, true, 75, 95.0f, 75.0f, 8192L, 100, 2000L, 65536);
		assertDiskStoreDirectoryLocations(testDiskStore, newFile("/absolute/path/to/gemfire/disk/directory"),
			newFile("relative/path/to/gemfire/disk/directory"));
		assertDiskStoreDirectorySizes(testDiskStore, 1024, 4096);
	}

	@Test
	public void enablesMultipleDiskStores() {
		applicationContext = newApplicationContext(MultipleDiskStoresConfiguration.class);

		DiskStore testDiskStoreOne = applicationContext.getBean("TestDiskStoreOne", DiskStore.class);

		assertDiskStore(testDiskStoreOne, "TestDiskStoreOne", false, true, 75, 99.0f, 90.0f, 2048L, 100, 1000L, 32768);

		DiskStore testDiskStoreTwo = applicationContext.getBean("TestDiskStoreTwo", DiskStore.class);

		assertDiskStore(testDiskStoreTwo, "TestDiskStoreTwo", true, true, 85, 99.0f, 90.0f, 4096L, 0, 1000L, 32768);
	}

	static String mockName(String baseName) {
		return String.format("%s%d", baseName, MOCK_ID.incrementAndGet());
	}

	/* (non-Javadoc) */
	protected static <R> Answer<R> newGetter(final AtomicReference<R> returnValue) {
		return new Answer<R>() {
			@Override
			public R answer(InvocationOnMock invocation) throws Throwable {
				return returnValue.get();
			}
		};
	}

	/* (non-Javadoc) */
	protected static <T, R> Answer<R> newSetter(final Class<T> parameterType, final AtomicReference<T> argument,
		final R returnValue) {

		return new Answer<R>() {
			@Override
			public R answer(InvocationOnMock invocation) throws Throwable {
				argument.set(invocation.getArgumentAt(0, parameterType));
				return returnValue;
			}
		};
	}

	@Configuration
	@SuppressWarnings("unused")
	static class CacheConfiguration {

		@Bean
		Cache gemfireCache() {
			Cache mockCache = mock(Cache.class);

			when(mockCache.createDiskStoreFactory()).thenAnswer(new Answer<DiskStoreFactory>() {
				@Override
				public DiskStoreFactory answer(InvocationOnMock invocation) throws Throwable {
					final DiskStoreFactory mockDiskStoreFactory =
						mock(DiskStoreFactory.class, mockName("MockDiskStoreFactory"));

					final AtomicReference<Boolean> allowForceCompaction = new AtomicReference<Boolean>(false);
					final AtomicReference<Boolean> autoCompact = new AtomicReference<Boolean>(false);
					final AtomicReference<Integer> compactionThreshold = new AtomicReference<Integer>(50);
					final AtomicReference<File[]> diskDirectories = new AtomicReference<File[]>(new File[0]);
					final AtomicReference<int[]> diskDirectorySizes = new AtomicReference<int[]>(new int[0]);
					final AtomicReference<Float> diskUsageCriticalPercentage = new AtomicReference<Float>(99.0f);
					final AtomicReference<Float> diskUsageWarningPercentage = new AtomicReference<Float>(90.0f);
					final AtomicReference<Long> maxOplogSize = new AtomicReference<Long>(1024L);
					final AtomicReference<Integer> queueSize = new AtomicReference<Integer>(0);
					final AtomicReference<Long> timeInterval = new AtomicReference<Long>(1000L);
					final AtomicReference<Integer> writeBufferSize = new AtomicReference<Integer>(32768);

					when(mockDiskStoreFactory.setAllowForceCompaction(anyBoolean())).thenAnswer(
						newSetter(Boolean.TYPE, allowForceCompaction, mockDiskStoreFactory));

					when(mockDiskStoreFactory.setAutoCompact(anyBoolean())).thenAnswer(
						newSetter(Boolean.TYPE, autoCompact, mockDiskStoreFactory));

					when(mockDiskStoreFactory.setCompactionThreshold(anyInt())).thenAnswer(
						newSetter(Integer.TYPE, compactionThreshold, mockDiskStoreFactory));

					when(mockDiskStoreFactory.setDiskDirsAndSizes(any(File[].class), any(int[].class))).thenAnswer(
						new Answer<DiskStoreFactory>() {
							@Override public DiskStoreFactory answer(InvocationOnMock invocation) throws Throwable {
								File[] localDiskDirectories = invocation.getArgumentAt(0, File[].class);
								int[] localDiskDirectorySizes = invocation.getArgumentAt(1, int[].class);

								diskDirectories.set(localDiskDirectories);
								diskDirectorySizes.set(localDiskDirectorySizes);

								return mockDiskStoreFactory;
							}
						}
					);

					when(mockDiskStoreFactory.setDiskUsageCriticalPercentage(anyFloat())).thenAnswer(
						newSetter(Float.TYPE, diskUsageCriticalPercentage, mockDiskStoreFactory));

					when(mockDiskStoreFactory.setDiskUsageWarningPercentage(anyFloat())).thenAnswer(
						newSetter(Float.TYPE, diskUsageWarningPercentage, mockDiskStoreFactory));

					when(mockDiskStoreFactory.setMaxOplogSize(anyLong())).thenAnswer(
						newSetter(Long.TYPE, maxOplogSize, mockDiskStoreFactory));

					when(mockDiskStoreFactory.setQueueSize(anyInt())).thenAnswer(
						newSetter(Integer.TYPE, queueSize, mockDiskStoreFactory));

					when(mockDiskStoreFactory.setTimeInterval(anyLong())).thenAnswer(
						newSetter(Long.TYPE, timeInterval, mockDiskStoreFactory));

					when(mockDiskStoreFactory.setWriteBufferSize(anyInt())).thenAnswer(
						newSetter(Integer.TYPE, writeBufferSize, mockDiskStoreFactory));

					when(mockDiskStoreFactory.create(anyString())).thenAnswer(new Answer<DiskStore>() {
						@Override
						public DiskStore answer(InvocationOnMock invocation) throws Throwable {
							String diskStoreName = invocation.getArgumentAt(0, String.class);

							DiskStore mockDiskStore = mock(DiskStore.class, diskStoreName);

							when(mockDiskStore.getAllowForceCompaction()).thenAnswer(newGetter(allowForceCompaction));
							when(mockDiskStore.getAutoCompact()).thenAnswer(newGetter(autoCompact));
							when(mockDiskStore.getCompactionThreshold()).thenAnswer(newGetter(compactionThreshold));
							when(mockDiskStore.getDiskDirs()).thenAnswer(newGetter(diskDirectories));
							when(mockDiskStore.getDiskDirSizes()).thenAnswer(newGetter(diskDirectorySizes));
							when(mockDiskStore.getDiskUsageCriticalPercentage()).thenAnswer(newGetter(diskUsageCriticalPercentage));
							when(mockDiskStore.getDiskUsageWarningPercentage()).thenAnswer(newGetter(diskUsageWarningPercentage));
							when(mockDiskStore.getMaxOplogSize()).thenAnswer(newGetter(maxOplogSize));
							when(mockDiskStore.getName()).thenReturn(diskStoreName);
							when(mockDiskStore.getQueueSize()).thenAnswer(newGetter(queueSize));
							when(mockDiskStore.getTimeInterval()).thenAnswer(newGetter(timeInterval));
							when(mockDiskStore.getWriteBufferSize()).thenAnswer(newGetter(writeBufferSize));

							return mockDiskStore;
						};
					});

					return mockDiskStoreFactory;
				}
			});

			return mockCache;
		}
	}

	@EnableDiskStore(name = "TestDiskStore", allowForceCompaction = true, autoCompact = true, compactionThreshold = 75,
		diskUsageCriticalPercentage = 95.0f, diskUsageWarningPercentage = 75.0f, maxOplogSize = 8192L, queueSize = 100,
		timeInterval = 2000L, writeBufferSize = 65536, diskDirectories = {
			@EnableDiskStore.DiskDirectory(location = "/absolute/path/to/gemfire/disk/directory", maxSize = 1024),
			@EnableDiskStore.DiskDirectory(location = "relative/path/to/gemfire/disk/directory", maxSize = 4096)
	})
	static class SingleDiskStoreConfiguration extends CacheConfiguration {

	}

	@EnableDiskStores(autoCompact = true, compactionThreshold = 75, maxOplogSize = 2048L, diskStores = {
		@EnableDiskStore(name = "TestDiskStoreOne", queueSize = 100),
		@EnableDiskStore(name = "TestDiskStoreTwo", autoCompact = false, allowForceCompaction = true,
			compactionThreshold = 85, maxOplogSize = 4096)
	})
	static class MultipleDiskStoresConfiguration extends CacheConfiguration {

	}
}
