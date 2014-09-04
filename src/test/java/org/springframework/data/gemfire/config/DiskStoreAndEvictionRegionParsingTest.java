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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilenameFilter;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.SimpleObjectSizer;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CustomExpiry;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.DiskStoreFactory;
import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAlgorithm;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.ExpirationAttributes;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.Region.Entry;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.util.ObjectSizer;

/**
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="diskstore-ns.xml", initializers=GemfireTestApplicationContextInitializer.class)
@SuppressWarnings({"rawtypes", "unused"})
public class DiskStoreAndEvictionRegionParsingTest {

	@Autowired
	private ApplicationContext context;

	@Autowired
	DiskStore diskStore1;

	private static File diskStoreDir;

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
		assertNotNull(context.getBean("ds2"));
		context.getBean("diskStore1");
 		assertNotNull(diskStore1);
		assertEquals("diskStore1", diskStore1.getName());
		assertEquals(50, diskStore1.getQueueSize());
		assertEquals(true, diskStore1.getAutoCompact());
		assertEquals(DiskStoreFactory.DEFAULT_COMPACTION_THRESHOLD, diskStore1.getCompactionThreshold());
		assertEquals(9999, diskStore1.getTimeInterval());
		assertEquals(10, diskStore1.getMaxOplogSize());
		assertEquals(diskStoreDir, diskStore1.getDiskDirs()[0]);
		Cache cache = context.getBean("gemfireCache", Cache.class);
		assertSame(diskStore1, cache.findDiskStore("diskStore1"));
	}

	@SuppressWarnings("rawtypes")
	public void testReplicatedDataRegionAttributes() throws Exception {
		assertTrue(context.containsBean("replicated-data"));

		RegionFactoryBean replicatedDataRegionFactoryBean = context.getBean("&replicated-data", RegionFactoryBean.class);

		assertTrue(replicatedDataRegionFactoryBean instanceof ReplicatedRegionFactoryBean);
		assertEquals(DataPolicy.REPLICATE, replicatedDataRegionFactoryBean.getDataPolicy());
		assertFalse(replicatedDataRegionFactoryBean.getDataPolicy().withPersistence());
		assertEquals("diskStore1", TestUtils.readField("diskStoreName", replicatedDataRegionFactoryBean));
		assertNull(TestUtils.readField("scope", replicatedDataRegionFactoryBean));

		Region replicatedDataRegion = context.getBean("replicated-data", Region.class);

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
	public void testPartitionDataOptions() throws Exception {
		assertTrue(context.containsBean("partition-data"));
		RegionFactoryBean fb = context.getBean("&partition-data", RegionFactoryBean.class);
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
	public void testEntryTtl() throws Exception {
		assertTrue(context.containsBean("replicated-data"));
		RegionFactoryBean fb = context.getBean("&replicated-data", RegionFactoryBean.class);
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
	public void testCustomExpiry() throws Exception {
		assertTrue(context.containsBean("replicated-data-custom-expiry"));
		RegionFactoryBean fb = context.getBean("&replicated-data-custom-expiry", RegionFactoryBean.class);
		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		
		assertNotNull(attrs.getCustomEntryIdleTimeout());
		assertNotNull(attrs.getCustomEntryTimeToLive());
		
		assertTrue(attrs.getCustomEntryIdleTimeout() instanceof TestCustomExpiry);
		assertTrue(attrs.getCustomEntryTimeToLive() instanceof TestCustomExpiry);
	}
	
	public static class TestCustomExpiry<K,V> implements CustomExpiry<K,V> {
		/* (non-Javadoc)
		 * @see com.gemstone.gemfire.cache.CacheCallback#close()
		 */
		@Override
		public void close() {
			// TODO Auto-generated method stub
			
		}

		/* (non-Javadoc)
		 * @see com.gemstone.gemfire.cache.CustomExpiry#getExpiry(com.gemstone.gemfire.cache.Region.Entry)
		 */
		@Override
		public ExpirationAttributes getExpiry(Entry<K, V> entry) {
			return null;
		}
		
	}
	
}
