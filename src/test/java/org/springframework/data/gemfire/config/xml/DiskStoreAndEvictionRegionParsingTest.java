/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilenameFilter;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.DiskStoreFactory;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.EvictionAlgorithm;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.Region.Entry;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.Scope;
import org.apache.geode.cache.util.ObjectSizer;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.SimpleObjectSizer;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.FileSystemUtils;

/**
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="diskstore-ns.xml", initializers=GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
// TODO move test cases into a DiskStoreIntegrationTests class
public class DiskStoreAndEvictionRegionParsingTest {

	private static File diskStoreDirectory;

	@Autowired
	private ApplicationContext applicationContext;

	@Autowired
	@Qualifier("diskStore1")
	private DiskStore diskStore;

	@BeforeClass
	public static void setUp() {
		diskStoreDirectory = new File("./tmp");
		assertTrue(diskStoreDirectory.isDirectory() || diskStoreDirectory.mkdirs());
	}

	@AfterClass
	public static void tearDown() {
		FileSystemUtils.deleteRecursively(diskStoreDirectory);

		for (String name : new File(".").list(new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return name.startsWith("BACKUPds");
			}
		})) {
			new File(name).delete();
		}
	}

	@Test
	public void testDiskStore() {
		assertNotNull(applicationContext.getBean("ds2"));
		applicationContext.getBean("diskStore1");
 		assertNotNull(diskStore);
		assertEquals("diskStore1", diskStore.getName());
		assertEquals(50, diskStore.getQueueSize());
		assertEquals(true, diskStore.getAutoCompact());
		assertEquals(DiskStoreFactory.DEFAULT_COMPACTION_THRESHOLD, diskStore.getCompactionThreshold());
		assertEquals(9999, diskStore.getTimeInterval());
		assertEquals(1, diskStore.getMaxOplogSize());
		assertEquals(diskStoreDirectory, diskStore.getDiskDirs()[0]);
		Cache cache = applicationContext.getBean("gemfireCache", Cache.class);
		assertSame(diskStore, cache.findDiskStore("diskStore1"));
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testReplicatedDataRegionAttributes() throws Exception {
		assertTrue(applicationContext.containsBean("replicated-data"));

		RegionFactoryBean replicatedDataRegionFactoryBean = applicationContext.getBean("&replicated-data", RegionFactoryBean.class);

		assertTrue(replicatedDataRegionFactoryBean instanceof ReplicatedRegionFactoryBean);
		assertEquals(DataPolicy.REPLICATE, replicatedDataRegionFactoryBean.getDataPolicy());
		assertFalse(replicatedDataRegionFactoryBean.getDataPolicy().withPersistence());
		assertEquals("diskStore1", TestUtils.readField("diskStoreName", replicatedDataRegionFactoryBean));
		assertNull(TestUtils.readField("scope", replicatedDataRegionFactoryBean));

		Region replicatedDataRegion = applicationContext.getBean("replicated-data", Region.class);

		RegionAttributes replicatedDataRegionAttributes = TestUtils.readField("attributes", replicatedDataRegionFactoryBean);

		assertNotNull(replicatedDataRegionAttributes);
		assertEquals(Scope.DISTRIBUTED_NO_ACK, replicatedDataRegionAttributes.getScope());

		EvictionAttributes replicatedDataEvictionAttributes = replicatedDataRegionAttributes.getEvictionAttributes();

		assertNotNull(replicatedDataEvictionAttributes);
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, replicatedDataEvictionAttributes.getAction());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, replicatedDataEvictionAttributes.getAlgorithm());
		assertEquals(50, replicatedDataEvictionAttributes.getMaximum());
		assertNull(replicatedDataEvictionAttributes.getObjectSizer());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testPartitionDataOptions() throws Exception {
		assertTrue(applicationContext.containsBean("partition-data"));
		RegionFactoryBean fb = applicationContext.getBean("&partition-data", RegionFactoryBean.class);
		assertTrue(fb instanceof PartitionedRegionFactoryBean);
		assertTrue((Boolean) TestUtils.readField("persistent", fb));
		RegionAttributes attrs = TestUtils.readField("attributes", fb);

		EvictionAttributes evicAttr = attrs.getEvictionAttributes();
		assertEquals(EvictionAction.LOCAL_DESTROY, evicAttr.getAction());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, evicAttr.getAlgorithm());
		// for some reason GemFire resets this to 56 on my machine (not sure
		// why)
		// assertEquals(10, evicAttr.getMaximum());
		ObjectSizer sizer = evicAttr.getObjectSizer();
		assertEquals(SimpleObjectSizer.class, sizer.getClass());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testEntryTtl() throws Exception {
		assertTrue(applicationContext.containsBean("replicated-data"));
		RegionFactoryBean fb = applicationContext.getBean("&replicated-data", RegionFactoryBean.class);
		RegionAttributes attrs = TestUtils.readField("attributes", fb);

		ExpirationAttributes entryTTL = attrs.getEntryTimeToLive();
		assertEquals(100, entryTTL.getTimeout());
		assertEquals(ExpirationAction.DESTROY, entryTTL.getAction());

		ExpirationAttributes entryTTI = attrs.getEntryIdleTimeout();
		assertEquals(200, entryTTI.getTimeout());
		assertEquals(ExpirationAction.INVALIDATE, entryTTI.getAction());

		ExpirationAttributes regionTTL = attrs.getRegionTimeToLive();
		assertEquals(300, regionTTL.getTimeout());
		assertEquals(ExpirationAction.DESTROY, regionTTL.getAction());

		ExpirationAttributes regionTTI = attrs.getRegionIdleTimeout();
		assertEquals(400, regionTTI.getTimeout());
		assertEquals(ExpirationAction.INVALIDATE, regionTTI.getAction());
	}


	@Test
	@SuppressWarnings("rawtypes")
	public void testCustomExpiry() throws Exception {
		assertTrue(applicationContext.containsBean("replicated-data-custom-expiry"));
		RegionFactoryBean fb = applicationContext.getBean("&replicated-data-custom-expiry", RegionFactoryBean.class);
		RegionAttributes attrs = TestUtils.readField("attributes", fb);

		assertNotNull(attrs.getCustomEntryIdleTimeout());
		assertNotNull(attrs.getCustomEntryTimeToLive());

		assertTrue(attrs.getCustomEntryIdleTimeout() instanceof TestCustomExpiry);
		assertTrue(attrs.getCustomEntryTimeToLive() instanceof TestCustomExpiry);
	}

	public static class TestCustomExpiry<K,V> implements CustomExpiry<K,V> {
		@Override
		public ExpirationAttributes getExpiry(Entry<K, V> entry) {
			return null;
		}

		@Override
		public void close() {

		}
	}
}
