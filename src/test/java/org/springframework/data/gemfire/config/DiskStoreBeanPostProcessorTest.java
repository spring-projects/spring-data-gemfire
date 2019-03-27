/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.FileSystemUtils;

/**
 * The DiskStoreBeanPostProcessorTest class is a test suite of test cases testing the contract and functionality of
 * the DiskStoreBeanPostProcessor class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.config.DiskStoreBeanPostProcessor
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
public class DiskStoreBeanPostProcessorTest {

	@BeforeClass
	public static void testSuiteSetup() {
		assertTrue("Failed to created directory './gemfire/disk-stores/local/ds2'!",
			new File("./gemfire/disk-stores/local/ds2").mkdirs());
	}

	@AfterClass
	public static void testSuiteTearDown() {
		assertTrue("Failed to delete directory './gemfire'!", FileSystemUtils.deleteRecursively(new File("./gemfire")));
		assertTrue("Failed to delete directory './gfe'!", FileSystemUtils.deleteRecursively(new File("./gfe")));
	}

	@Test
	public void testDiskStoreDirectoryLocationsExist() {
		assertTrue(new File("./gemfire/disk-stores/ds1").isDirectory());
		assertTrue(new File("./gemfire/disk-stores/local/ds2").isDirectory());
		assertTrue(new File("./gemfire/disk-stores/remote/ds2").isDirectory());
		assertTrue(new File("./gfe/ds/local/store3").isDirectory());
	}

}
