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

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.EntryOperation;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.EvictionAlgorithm;
import org.apache.geode.cache.PartitionResolver;
import org.apache.geode.cache.Region;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The PersistentPartitionRegionTemplateTest class is a test suite of test cases testing the functionality of
 * Spring Data GemFire's Region templates with a 'persistent', PARTITION Region configuration.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.PartitionedRegionFactoryBean
 * @see org.apache.geode.cache.Region
 * @link https://jira.spring.io/browse/SGF-384
 * @since 1.6.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class TemplatePersistentPartitionRegionNamespaceTest {

	@Autowired
	private DiskStore exampleDataStore;

	@Resource(name = "Example")
	private Region<?, ?> example;

	@After
	public void tearDown() {
		for (File diskDirectory : exampleDataStore.getDiskDirs()) {
			FileSystemUtils.deleteRecursive(FileSystemUtils.getRootRelativeToWorkingDirectoryOrPath(
				diskDirectory.getAbsoluteFile()));
		}
	}

	@Test
	public void testExampleTemplatedPersistentPartitionRegion() {
		assertNotNull("The '/Example' PARTITION Region was not properly configured and initialized!", example);
		assertEquals("Example", example.getName());
		assertEquals("/Example", example.getFullPath());
		assertNotNull(example.getAttributes());
		assertEquals(DataPolicy.PERSISTENT_PARTITION, example.getAttributes().getDataPolicy());
		assertNotNull(example.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAlgorithm.LRU_HEAP, example.getAttributes().getEvictionAttributes().getAlgorithm());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, example.getAttributes().getEvictionAttributes().getAction());
		assertNotNull(example.getAttributes().getPartitionAttributes());
		assertEquals(1, example.getAttributes().getPartitionAttributes().getRedundantCopies());
		assertEquals(163, example.getAttributes().getPartitionAttributes().getTotalNumBuckets());
		assertNotNull(example.getAttributes().getPartitionAttributes().getPartitionResolver());
		assertEquals("TestPartitionResolver", example.getAttributes().getPartitionAttributes().getPartitionResolver().getName());
	}

	public static final class TestPartitionResolver<K, V> implements PartitionResolver<K, V> {

		private String name;

		@Override
		public Object getRoutingObject(final EntryOperation<K, V> kvEntryOperation) {
			throw new UnsupportedOperationException("Not Implemented!");
		}

		@Override
		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		@Override
		public void close() {
		}
	}

}
