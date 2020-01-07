/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.Scope;
import org.apache.geode.compression.Compressor;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.PeerRegionFactoryBean;
import org.springframework.data.gemfire.ResolvableRegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.SimpleCacheListener;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.mock.context.GemFireMockObjectsApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.ObjectUtils;

/**
 * Unit tests for the Replicated Region Namespace.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.ReplicatedRegionFactoryBean
 * @see org.springframework.data.gemfire.config.xml.ReplicatedRegionParser
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(locations = "replicated-ns.xml",
	initializers = GemFireMockObjectsApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class ReplicatedRegionNamespaceTest {

	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void testSimpleReplicateRegion() throws Exception {

		assertTrue(applicationContext.containsBean("simple"));

		PeerRegionFactoryBean simpleRegionFactoryBean =
			applicationContext.getBean("&simple", PeerRegionFactoryBean.class);

		assertEquals("simple", TestUtils.readField("beanName", simpleRegionFactoryBean));
		assertEquals(false, TestUtils.readField("close", simpleRegionFactoryBean));
		assertNull(TestUtils.readField("scope", simpleRegionFactoryBean));

		RegionAttributes simpleRegionAttributes = TestUtils.readField("attributes", simpleRegionFactoryBean);

		assertNotNull(simpleRegionAttributes);
		assertFalse(simpleRegionAttributes.getConcurrencyChecksEnabled());
		assertEquals(13, simpleRegionAttributes.getConcurrencyLevel());
		assertEquals(Scope.DISTRIBUTED_NO_ACK, simpleRegionAttributes.getScope());
	}

	@Test
	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testPublishReplicateRegion() throws Exception {

		assertTrue(applicationContext.containsBean("pub"));

		PeerRegionFactoryBean publisherRegionFactoryBean =
			applicationContext.getBean("&pub", PeerRegionFactoryBean.class);

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

		assertTrue(applicationContext.containsBean("complex"));

		PeerRegionFactoryBean complexRegionFactoryBean =
			applicationContext.getBean("&complex", PeerRegionFactoryBean.class);

		assertNotNull(complexRegionFactoryBean);
		assertEquals("complex", TestUtils.readField("beanName", complexRegionFactoryBean));

		CacheListener[] cacheListeners = TestUtils.readField("cacheListeners", complexRegionFactoryBean);

		assertFalse(ObjectUtils.isEmpty(cacheListeners));
		assertEquals(2, cacheListeners.length);
		assertSame(applicationContext.getBean("c-listener"), cacheListeners[0]);
		assertTrue(cacheListeners[1] instanceof SimpleCacheListener);
		assertNotSame(cacheListeners[0], cacheListeners[1]);
		assertSame(applicationContext.getBean("c-loader"), TestUtils.readField("cacheLoader", complexRegionFactoryBean));
		assertSame(applicationContext.getBean("c-writer"), TestUtils.readField("cacheWriter", complexRegionFactoryBean));
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testReplicatedRegionWithAttributes() throws Exception {

		assertTrue(applicationContext.containsBean("replicated-with-attributes"));

		Region region = applicationContext.getBean("replicated-with-attributes", Region.class);

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
		assertTrue(regionAttributes.getOffHeap());
		assertEquals(Scope.GLOBAL, regionAttributes.getScope());
		assertEquals(String.class, regionAttributes.getValueConstraint());
	}

	@Test
	public void testReplicatedWithSynchronousIndexUpdates() {

		assertTrue(applicationContext.containsBean("replicated-with-synchronous-index-updates"));

		Region region = applicationContext.getBean("replicated-with-synchronous-index-updates", Region.class);

		assertNotNull(String.format("The '%1$s' Region was not properly configured and initialized!",
			"replicated-with-synchronous-index-updates"), region);

		RegionAttributes regionAttributes = region.getAttributes();

		assertNotNull(regionAttributes);
		assertTrue(regionAttributes.getIndexMaintenanceSynchronous());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testRegionLookup() throws Exception {

		Cache cache = applicationContext.getBean(Cache.class);

		Region existing = cache.createRegionFactory().create("existing");

		assertTrue(applicationContext.containsBean("lookup"));

		ResolvableRegionFactoryBean regionFactoryBean =
			applicationContext.getBean("&lookup", ResolvableRegionFactoryBean.class);

		assertNotNull(regionFactoryBean);
		assertEquals("existing", TestUtils.readField("name", regionFactoryBean));
		assertSame(existing, applicationContext.getBean("lookup"));
	}

	@Test
	public void testCompressedReplicateRegion() {

		assertTrue(applicationContext.containsBean("Compressed"));

		Region<?, ?> compressed = applicationContext.getBean("Compressed", Region.class);

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
