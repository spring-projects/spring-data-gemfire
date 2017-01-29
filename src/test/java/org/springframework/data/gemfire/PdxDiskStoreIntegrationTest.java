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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.Serializable;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;

/**
 * The PdxDiskStoreTest class is a test suite containing tests to reproduce the issue in JIRA SGF-197.
 *
 * @author John Blum
 * @link https://jira.springsource.org/browse/SGF-197
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.annotation.DirtiesContext
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.3.3
 */
@ContextConfiguration("/org/springframework/data/gemfire/pdxdiskstore-config.xml")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_EACH_TEST_METHOD)
@RunWith(SpringJUnit4ClassRunner.class)
public class PdxDiskStoreIntegrationTest {

	protected static final int NUMBER_OF_REGION_ENTRIES = 1000;

	@Resource(name = "pdxDataRegion")
	private Region<KeyHolder<String>, ValueHolder<Integer>> pdxDataRegion;

	protected static void assertRegionExists(final String expectedRegionName, final String expectedRegionPath, final Region region) {
		assertNotNull(region);
		assertEquals(String.format("Expected Region with name %1$s; but was %2$s!",
			expectedRegionName, region.getName()), expectedRegionName, region.getName());
		assertEquals(String.format("Expected Region with path %1$s; but was %2$s!",
			expectedRegionPath, region.getFullPath()), expectedRegionPath, region.getFullPath());
	}

	protected static boolean createDirectory(final File path) {
		return (path != null && (path.isDirectory() || path.mkdirs()));
	}

	protected static File createFile(final String pathname) {
		return new File(pathname);
	}

	protected static void deleteRecursive(final File path) {
		if (path.isDirectory()) {
			for (File file : path.listFiles()) {
				deleteRecursive(file);
			}
		}

		path.delete();
	}

	@BeforeClass
	public static void setupBeforeClass() {
		assertTrue(createDirectory(createFile("./gemfire/data-store")));
		assertTrue(createDirectory(createFile("./gemfire/pdx-store")));
	}

	@AfterClass
	public static void tearDownAfterClass() {
		deleteRecursive(createFile("./gemfire"));
	}

	@Before
	public void setup() {
		assertNotNull("The PdxData GemFire Region was not created successfully!", pdxDataRegion);

		if (pdxDataRegion.size() == 0) {
			System.out.printf("Creating entries for Region (%1$s)...%n", pdxDataRegion.getName());
			for (int index = 1; index <= NUMBER_OF_REGION_ENTRIES; index++) {
				pdxDataRegion.put(new KeyHolder<String>("key" + index), new ValueHolder<Integer>(index));
			}
		}
	}

	@Test
	public void testPersistentRegionWithDataCreation() {
		assertRegionExists("PdxData", "/PdxData", pdxDataRegion);
		assertEquals(NUMBER_OF_REGION_ENTRIES, pdxDataRegion.size());
	}

	@Test
	public void testPersistentRegionWithDataRecovery() {
		assertRegionExists("PdxData", "/PdxData", pdxDataRegion);
		assertEquals(NUMBER_OF_REGION_ENTRIES, pdxDataRegion.size());
	}

	protected static class AbstractHolderSupport {

		protected static boolean equals(final Object obj1, final Object obj2) {
			return (obj1 != null && obj1.equals(obj2));
		}

		protected static int hashCode(final Object obj) {
			return (obj == null ? 0 : obj.hashCode());
		}
	}

	@SuppressWarnings("unused")
	public static class KeyHolder<T extends Serializable> extends AbstractHolderSupport {

		private T key;

		public KeyHolder() {
		}

		public KeyHolder(final T key) {
			Assert.notNull(key, "The key cannot be null!");
			this.key = key;
		}

		public T getKey() {
			return key;
		}

		public void setKey(final T key) {
			this.key = key;
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == this) {
				return true;
			}

			if (!(obj instanceof KeyHolder)) {
				return false;
			}

			final KeyHolder that = (KeyHolder) obj;

			return equals(this.getKey(), that.getKey());
		}

		@Override
		public int hashCode() {
			int hashValue = 17;
			hashValue = 37 * hashValue + hashCode(this.getKey());
			return hashValue;
		}

		@Override
		public String toString() {
			return String.valueOf(getKey());
		}
	}

	@SuppressWarnings("unused")
	public static class ValueHolder<T extends Serializable> extends AbstractHolderSupport {

		private T value;

		public ValueHolder() {
		}

		public ValueHolder(final T value) {
			this.value = value;
		}

		public T getValue() {
			return value;
		}

		public void setValue(final T value) {
			this.value = value;
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == this) {
				return true;
			}

			if (!(obj instanceof ValueHolder)) {
				return false;
			}

			final ValueHolder that = (ValueHolder) obj;

			return equals(this.getValue(), that.getValue());
		}

		@Override
		public int hashCode() {
			int hashValue = 17;
			hashValue = 17 * hashValue + hashCode(this.getValue());
			return hashValue;
		}

		@Override
		public String toString() {
			return String.valueOf(getValue());
		}
	}

}
