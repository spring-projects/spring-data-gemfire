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

package org.springframework.data.gemfire.config.support;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;

import org.apache.geode.cache.GemFireCache;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.DiskStoreFactoryBean;
import org.springframework.data.gemfire.config.annotation.PeerCacheApplication;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link DiskStoreDirectoryBeanPostProcessor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.DiskStoreFactoryBean
 * @see org.springframework.data.gemfire.config.support.DiskStoreDirectoryBeanPostProcessor
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 1.5.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
public class DiskStoreDirectoryBeanPostProcessorIntegrationTests {

	@BeforeClass
	public static void testSuiteSetup() {
		assertThat(new File("./gemfire/disk-stores/ds2/local").mkdirs()).isTrue();
	}

	@AfterClass
	public static void testSuiteTearDown() {
		assertThat(FileSystemUtils.deleteRecursive(new File("./gemfire"))).isTrue();
		assertThat(FileSystemUtils.deleteRecursive(new File("./gfe"))).isTrue();
	}

	@Test
	public void diskStoreDirectoriesExist() {
		assertThat(new File("./gemfire/disk-stores/ds1").isDirectory()).isTrue();
		assertThat(new File("./gemfire/disk-stores/ds2/local").isDirectory()).isTrue();
		assertThat(new File("./gemfire/disk-stores/ds2/remote").isDirectory()).isTrue();
		assertThat(new File("./gfe/ds/store3/local").isDirectory()).isTrue();
	}

	@PeerCacheApplication(logLevel = "error")
	@SuppressWarnings("unused")
	static class DiskStoreDirectoryBeanPostProcessorConfiguration {

		DiskStoreFactoryBean.DiskDir newDiskDir(String location) {
			return new DiskStoreFactoryBean.DiskDir(location);
		}

		@Bean
		DiskStoreFactoryBean diskStoreOne(GemFireCache gemfireCache) {
			DiskStoreFactoryBean diskStoreOne = new DiskStoreFactoryBean();
			diskStoreOne.setCache(gemfireCache);
			diskStoreOne.setDiskDirs(Collections.singletonList(newDiskDir("./gemfire/disk-stores/ds1")));
			return diskStoreOne;
		}

		@Bean
		DiskStoreFactoryBean diskStoreTwo(GemFireCache gemfireCache) {
			DiskStoreFactoryBean diskStoreTwo = new DiskStoreFactoryBean();
			diskStoreTwo.setCache(gemfireCache);
			diskStoreTwo.setDiskDirs(Arrays.asList(newDiskDir("./gemfire/disk-stores/ds2/local"),
				newDiskDir("./gemfire/disk-stores/ds2/remote")));
			return diskStoreTwo;
		}

		@Bean
		DiskStoreFactoryBean diskStoreThree(GemFireCache gemfireCache) {
			DiskStoreFactoryBean diskStoreThree = new DiskStoreFactoryBean();
			diskStoreThree.setCache(gemfireCache);
			diskStoreThree.setDiskDirs(Collections.singletonList(newDiskDir("./gfe/ds/store3/local")));
			return diskStoreThree;
		}
	}
}
