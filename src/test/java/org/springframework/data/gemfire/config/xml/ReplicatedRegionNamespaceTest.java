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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.compression.Compressor;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.SimpleCacheListener;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

/**
 * The ReplicatedRegionNamespaceTest class is a test suite of test cases testing the contract and functionality
 * of GemFire Replicated Region support in SDG.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.ReplicatedRegionFactoryBean
 * @see org.springframework.data.gemfire.config.xml.ReplicatedRegionParser
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="replicated-ns.xml", initializers=GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class ReplicatedRegionNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testSimpleReplicateRegion() throws Exception {
		assertTrue(context.containsBean("simple"));

		RegionFactoryBean simpleRegionFactoryBean = context.getBean("&simple", RegionFactoryBean.class);

		assertEquals("simple", TestUtils.readField("beanName", simpleRegionFactoryBean));
		assertEquals(false, TestUtils.readField("close", simpleRegionFactoryBean));
		assertNull(TestUtils.readField("scope", simpleRegionFactoryBean));

		RegionAttributes simpleRegionAttributes = TestUtils.readField("attributes", simpleRegionFactoryBean);

		assertNotNull(simpleRegionAttributes);
		assertFalse(simpleRegionAttributes.getConcurrencyChecksEnabled());
		assertEquals(Scope.DISTRIBUTED_NO_ACK, simpleRegionAttributes.getScope());
	}

	@Test
	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testPublishReplicateRegion() throws Exception {
		assertTrue(context.containsBean("pub"));

		RegionFactoryBean publisherRegionFactoryBean = context.getBean("&pub", RegionFactoryBean.class);

		assertTrue(publisherRegionFactoryBean instanceof ReplicatedRegionFactoryBean);
		assertEquals("publisher", TestUtils.readField("name", publisherRegionFactoryBean));
		assertEquals(Scope.DISTRIBUTED_ACK, TestUtils.readField("scope", publisherRegionFactoryBean));

		RegionAttributes publisherRegionAttributes = TestUtils.readField("attributes", publisherRegionFactoryBean);

		assertTrue(publisherRegionAttributes.getConcurrencyChecksEnabled());
		assertFalse(publisherRegionAttributes.getPublisher());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testComplexReplicateRegion() throws Exception {
		assertTrue(context.containsBean("complex"));

		RegionFactoryBean complexRegionFactoryBean = context.getBean("&complex", RegionFactoryBean.class);

		assertNotNull(complexRegionFactoryBean);
		assertEquals("complex", TestUtils.readField("beanName", complexRegionFactoryBean));

		CacheListener[] cacheListeners = TestUtils.readField("cacheListeners", complexRegionFactoryBean);

		assertFalse(ObjectUtils.isEmpty(cacheListeners));
		assertEquals(2, cacheListeners.length);
		assertSame(context.getBean("c-listener"), cacheListeners[0]);
		assertTrue(cacheListeners[1] instanceof SimpleCacheListener);
		assertNotSame(cacheListeners[0], cacheListeners[1]);
		assertSame(context.getBean("c-loader"), TestUtils.readField("cacheLoader", complexRegionFactoryBean));
		assertSame(context.getBean("c-writer"), TestUtils.readField("cacheWriter", complexRegionFactoryBean));
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testReplicatedRegionWithAttributes() throws Exception {
		assertTrue(context.containsBean("replicated-with-attributes"));

		Region region = context.getBean("replicated-with-attributes", Region.class);

		assertNotNull("The 'replicated-with-attributes' Region was not properly configured and initialized!", region);

		RegionAttributes regionAttributes = region.getAttributes();

		assertNotNull(regionAttributes);
		assertFalse(regionAttributes.getCloningEnabled());
		assertEquals(10, regionAttributes.getConcurrencyLevel());
		assertTrue(regionAttributes.isDiskSynchronous());
		assertTrue(regionAttributes.getEnableAsyncConflation());
		assertTrue(regionAttributes.getEnableSubscriptionConflation());
		assertTrue(regionAttributes.getIgnoreJTA());
		assertEquals(10, regionAttributes.getInitialCapacity());
		assertFalse(regionAttributes.getIndexMaintenanceSynchronous());
		assertEquals(String.class, regionAttributes.getKeyConstraint());
		assertEquals(0.50, regionAttributes.getLoadFactor(), 0.001);
		assertTrue(regionAttributes.isLockGrantor());
		assertTrue(regionAttributes.getMulticastEnabled());
		assertEquals(Scope.GLOBAL, regionAttributes.getScope());
		assertEquals(String.class, regionAttributes.getValueConstraint());
	}

	@Test
	public void testReplicatedWithSynchronousIndexUpdates() {
		assertTrue(context.containsBean("replicated-with-synchronous-index-updates"));

		Region region = context.getBean("replicated-with-synchronous-index-updates", Region.class);

		assertNotNull(String.format("The '%1$s' Region was not properly configured and initialized!",
			"replicated-with-synchronous-index-updates"), region);

		RegionAttributes regionAttributes = region.getAttributes();

		assertNotNull(regionAttributes);
		assertTrue(regionAttributes.getIndexMaintenanceSynchronous());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testRegionLookup() throws Exception {
		Cache cache = context.getBean(Cache.class);
		Region existing = cache.createRegionFactory().create("existing");

		assertTrue(context.containsBean("lookup"));

		RegionLookupFactoryBean regionLookupFactoryBean = context.getBean("&lookup", RegionLookupFactoryBean.class);

		assertNotNull(regionLookupFactoryBean);
		assertEquals("existing", TestUtils.readField("name", regionLookupFactoryBean));
		assertSame(existing, context.getBean("lookup"));
	}

	@Test
	public void testCompressedReplicateRegion() {
		assertTrue(context.containsBean("Compressed"));

		Region<?, ?> compressed = context.getBean("Compressed", Region.class);

		assertNotNull("The 'Compressed' REPLICATE Region was not properly configured and initialized!", compressed);
		assertEquals("Compressed", compressed.getName());
		assertEquals(Region.SEPARATOR + "Compressed", compressed.getFullPath());
		assertNotNull(compressed.getAttributes());
		assertEquals(DataPolicy.REPLICATE, compressed.getAttributes().getDataPolicy());
		assertEquals(Scope.DISTRIBUTED_NO_ACK, compressed.getAttributes().getScope());
		assertTrue(compressed.getAttributes().getCompressor() instanceof TestCompressor);
		assertEquals("XYZ", compressed.getAttributes().getCompressor().toString());
	}

	public static class TestCompressor implements Compressor {

		private String name;

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public byte[] compress(final byte[] input) {
			throw new UnsupportedOperationException("Not Implemented!");
		}

		@Override
		public byte[] decompress(final byte[] input) {
			throw new UnsupportedOperationException("Not Implemented!");
		}

		@Override
		public String toString() {
			return this.name;
		}
	}
}
