/*
 * Copyright 2010-2011 the original author or authors.
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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.SimplePartitionResolver;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.PartitionAttributes;
import com.gemstone.gemfire.cache.RegionAttributes;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("partitioned-ns.xml")
public class PartitionedRegionNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testBasicPartition() throws Exception {
		assertTrue(context.containsBean("simple"));
	}

	@Test
	public void testPartitionOptions() throws Exception {
		assertTrue(context.containsBean("options"));
		RegionFactoryBean fb = context.getBean("&options", RegionFactoryBean.class);
		assertEquals(DataPolicy.PARTITION, TestUtils.readField("dataPolicy", fb));
		assertEquals(null, TestUtils.readField("scope", fb));
		assertEquals("redundant", TestUtils.readField("name", fb));

		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		assertTrue(attrs.getStatisticsEnabled());

		PartitionAttributes pAttr = attrs.getPartitionAttributes();

		assertEquals(1, pAttr.getRedundantCopies());
		assertEquals(4, pAttr.getTotalNumBuckets());
		assertSame(SimplePartitionResolver.class, pAttr.getPartitionResolver().getClass());
	}

	@Test
	public void testComplexPartition() throws Exception {
		assertTrue(context.containsBean("complex"));
		RegionFactoryBean fb = context.getBean("&complex", RegionFactoryBean.class);
		CacheListener[] listeners = TestUtils.readField("cacheListeners", fb);
		assertFalse(ObjectUtils.isEmpty(listeners));
		assertEquals(2, listeners.length);
		assertSame(listeners[0], context.getBean("c-listener"));

		assertSame(context.getBean("c-loader"), TestUtils.readField("cacheLoader", fb));
		assertSame(context.getBean("c-writer"), TestUtils.readField("cacheWriter", fb));

		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		PartitionAttributes pAttr = attrs.getPartitionAttributes();

		assertEquals(20, pAttr.getLocalMaxMemory());
	}
}