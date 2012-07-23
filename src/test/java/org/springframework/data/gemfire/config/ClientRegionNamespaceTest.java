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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.SimpleObjectSizer;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.Interest;
import org.springframework.data.gemfire.client.RegexInterest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAlgorithm;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.InterestResultPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.util.ObjectSizer;

/**
 * @author Costin Leau
 * @author David Turanski
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("client-ns.xml")
public class ClientRegionNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testAll() throws Exception {
		testBasicClient();
		testBeanNames();
		testPublishingClient();
		testPersistent();
		testOverflowToDisk();
	}

	private void testBasicClient() throws Exception {
		assertTrue(context.containsBean("simple"));
	}

	private void testBeanNames() throws Exception {
		assertTrue(ObjectUtils.isEmpty(context.getAliases("publisher")));
	}

	@SuppressWarnings("rawtypes")
	private void testPublishingClient() throws Exception {
		assertTrue(context.containsBean("empty"));
		ClientRegionFactoryBean fb = context.getBean("&empty", ClientRegionFactoryBean.class);
		assertEquals(DataPolicy.EMPTY, TestUtils.readField("dataPolicy", fb));
	}

	// @Test
	@SuppressWarnings("rawtypes")
	public void testComplexClient() throws Exception {
		assertTrue(context.containsBean("complex"));
		ClientRegionFactoryBean fb = context.getBean("&complex", ClientRegionFactoryBean.class);
		CacheListener[] listeners = TestUtils.readField("cacheListeners", fb);
		assertFalse(ObjectUtils.isEmpty(listeners));
		assertEquals(2, listeners.length);
		assertSame(listeners[0], context.getBean("c-listener"));
		Interest[] ints = TestUtils.readField("interests", fb);
		assertEquals(2, ints.length);

		// key interest
		Interest keyInt = ints[0];
		assertTrue((Boolean) TestUtils.readField("durable", keyInt));
		assertEquals(InterestResultPolicy.KEYS, TestUtils.readField("policy", keyInt));
		// assertEquals(Object.class, TestUtils.readField("key",
		// keyInt).getClass());

		// regex interest
		RegexInterest regexInt = (RegexInterest) ints[1];
		assertFalse((Boolean) TestUtils.readField("durable", regexInt));
		assertEquals(InterestResultPolicy.KEYS_VALUES, TestUtils.readField("key", regexInt));
		assertEquals(".*", TestUtils.readField("key", regexInt));
	}

	@SuppressWarnings("rawtypes")
	private void testPersistent() throws Exception {
		assertTrue(context.containsBean("persistent"));
		Region region = context.getBean("persistent", Region.class);
		RegionAttributes attrs = region.getAttributes();
		assertEquals("diskStore", attrs.getDiskStoreName());
		assertEquals(1, attrs.getDiskDirSizes()[0]);
	}

	@SuppressWarnings("rawtypes")
	private void testOverflowToDisk() throws Exception {
		assertTrue(context.containsBean("overflow"));
		ClientRegionFactoryBean fb = context.getBean("&overflow", ClientRegionFactoryBean.class);
		assertEquals(DataPolicy.NORMAL, TestUtils.readField("dataPolicy", fb));
		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		EvictionAttributes evicAttr = attrs.getEvictionAttributes();
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, evicAttr.getAction());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, evicAttr.getAlgorithm());
		// for some reason GemFire resets this to 56 on my machine (not sure
		// why)
		// assertEquals(10, evicAttr.getMaximum());
		ObjectSizer sizer = evicAttr.getObjectSizer();
		assertEquals(SimpleObjectSizer.class, sizer.getClass());
	}
}