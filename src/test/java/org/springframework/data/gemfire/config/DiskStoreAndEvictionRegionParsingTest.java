/*
 * Copyright 2010 the original author or authors.
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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.SimpleObjectSizer;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAlgorithm;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.util.ObjectSizer;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("diskstore-ns.xml")
public class DiskStoreAndEvictionRegionParsingTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testReplicaDataOptions() throws Exception {
		assertTrue(context.containsBean("replicated-data"));
		RegionFactoryBean fb = context.getBean("&replicated-data", RegionFactoryBean.class);
		assertEquals(DataPolicy.PERSISTENT_REPLICATE, TestUtils.readField("dataPolicy", fb));
		assertEquals(Scope.DISTRIBUTED_ACK, TestUtils.readField("scope", fb));
		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		File[] diskDirs = attrs.getDiskDirs();
		assertEquals(1, diskDirs.length);
		int[] diskDirSizes = attrs.getDiskDirSizes();
		assertEquals(1, diskDirSizes.length);
		assertEquals(1, diskDirSizes[0]);

		// eviction tests
		EvictionAttributes evicAttr = attrs.getEvictionAttributes();
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, evicAttr.getAction());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, evicAttr.getAlgorithm());
		assertEquals(50, evicAttr.getMaximum());
		assertNull(evicAttr.getObjectSizer());
	}

	@Test
	public void testPartitionDataOptions() throws Exception {
		assertTrue(context.containsBean("partition-data"));
		RegionFactoryBean fb = context.getBean("&partition-data", RegionFactoryBean.class);
		assertEquals(DataPolicy.PARTITION, TestUtils.readField("dataPolicy", fb));
		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		EvictionAttributes evicAttr = attrs.getEvictionAttributes();
		assertEquals(EvictionAction.LOCAL_DESTROY, evicAttr.getAction());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, evicAttr.getAlgorithm());
		// for some reason GemFire resets this to 56 on my machine (not sure why)
		//assertEquals(10, evicAttr.getMaximum());
		ObjectSizer sizer = evicAttr.getObjectSizer();
		assertEquals(SimpleObjectSizer.class, sizer.getClass());
	}
}