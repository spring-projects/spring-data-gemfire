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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.Scope;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("replicated-ns.xml")
public class ReplicatedRegionNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testBasicReplica() throws Exception {
		assertTrue(context.containsBean("simple"));
	}

	@Test
	public void testPublishingReplica() throws Exception {
		assertTrue(context.containsBean("pub"));
		RegionFactoryBean fb = context.getBean("&pub", RegionFactoryBean.class);
		assertEquals(DataPolicy.REPLICATE, TestUtils.readField("dataPolicy", fb));
		assertEquals(Scope.DISTRIBUTED_ACK, TestUtils.readField("scope", fb));
		assertEquals("publisher", TestUtils.readField("name", fb));
		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		assertFalse(attrs.getPublisher());
	}

	@Test
	public void testComplexReplica() throws Exception {
		assertTrue(context.containsBean("complex"));
		RegionFactoryBean fb = context.getBean("&complex", RegionFactoryBean.class);
		CacheListener[] listeners = TestUtils.readField("cacheListeners", fb);
		assertFalse(ObjectUtils.isEmpty(listeners));
		assertEquals(2, listeners.length);
		assertSame(listeners[0], context.getBean("c-listener"));

		assertSame(context.getBean("c-loader"), TestUtils.readField("cacheLoader", fb));
		assertSame(context.getBean("c-writer"), TestUtils.readField("cacheWriter", fb));
	}

	@Test
	public void testRegionLookup() throws Exception {
		assertTrue(context.containsBean("lookup"));
		RegionLookupFactoryBean lfb = context.getBean("&lookup", RegionLookupFactoryBean.class);
		assertEquals("simple", TestUtils.readField("name", lfb));
		assertEquals(context.getBean("simple"), context.getBean("lookup"));
	}
}