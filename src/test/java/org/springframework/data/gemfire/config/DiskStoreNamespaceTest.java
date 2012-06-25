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
package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;

import java.io.File;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.DiskStoreFactory;

/**
 * @author David Turanski
 * 
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("diskstore-ns-new.xml")
public class DiskStoreNamespaceTest {
	private static File diskStoreDir;

	@Autowired
	DiskStore diskStore1;

	@Test
	public void testDiskStore() {
		assertEquals("diskStore1", diskStore1.getName());
		assertEquals(50, diskStore1.getQueueSize());
		assertEquals(true, diskStore1.getAutoCompact());
		assertEquals(DiskStoreFactory.DEFAULT_COMPACTION_THRESHOLD, diskStore1.getCompactionThreshold());
		assertEquals(9999, diskStore1.getTimeInterval());
		assertEquals(10, diskStore1.getMaxOplogSize());
		assertEquals(diskStoreDir, diskStore1.getDiskDirs()[0]);
	}

	@BeforeClass
	public static void setUp() {
		String path = "./build/tmp";
		diskStoreDir = new File(path);
		if (!diskStoreDir.exists()) {
			diskStoreDir.mkdir();
		}
	}

	@AfterClass
	public static void tearDown() {
		for (File file : diskStoreDir.listFiles()) {
			file.delete();
		}
		diskStoreDir.delete();
	}
}
