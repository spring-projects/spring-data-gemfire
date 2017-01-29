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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.Properties;

import org.apache.geode.cache.DiskStore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The DiskStoreNamespaceTest class is a test suite of test cases testing the contract and functionality of using
 * Spring Data GemFire's XML namespace to configure GemFire Disk Stores.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 2.0.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "diskstore-ns.xml", initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class DiskStoreNamespaceTest {

	@Autowired
	@Qualifier("fullyConfiguredDiskStore")
	private DiskStore diskStore;

	@Autowired
	@Qualifier("props")
	private Properties props;

	@Test
	public void testDiskStoreConfiguration() {
		assertNotNull("The 'fullyConfiguredDiskStore' was not properly configured and initialized", diskStore);
		assertEquals("fullyConfiguredDiskStore", diskStore.getName());
		assertEquals(Boolean.valueOf(props.getProperty("allowForceCompaction")), diskStore.getAllowForceCompaction());
		assertEquals(Boolean.valueOf(props.getProperty("autoCompact")), diskStore.getAutoCompact());
		assertEquals(Long.valueOf(props.getProperty("compactionThreshold")),
			Long.valueOf(diskStore.getCompactionThreshold()));
		assertEquals(Double.valueOf(props.getProperty("diskUsageCriticalPercentage")),
			Double.valueOf(diskStore.getDiskUsageCriticalPercentage()));
		assertEquals(Double.valueOf(props.getProperty("diskUsageWarningPercentage")),
			Double.valueOf(diskStore.getDiskUsageWarningPercentage()));
		assertEquals(Long.valueOf(props.getProperty("maxOplogSize")), Long.valueOf(diskStore.getMaxOplogSize()));
		assertEquals(Long.valueOf(props.getProperty("queueSize")), Long.valueOf(diskStore.getQueueSize()));
		assertEquals(Long.valueOf(props.getProperty("timeInterval")), Long.valueOf(diskStore.getTimeInterval()));
		assertEquals(Long.valueOf(props.getProperty("writeBufferSize")), Long.valueOf(diskStore.getWriteBufferSize()));
		assertNotNull(diskStore.getDiskDirs());
		assertEquals(1, diskStore.getDiskDirs().length);
		assertEquals(new File(props.getProperty("location")), diskStore.getDiskDirs()[0]);
		assertEquals(Long.valueOf(props.getProperty("maxSize")), Long.valueOf(diskStore.getDiskDirSizes()[0]));
	}

}
