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
import static org.junit.Assert.assertNotSame;
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
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.SimpleCacheListener;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.mock.context.GemFireMockObjectsApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.ObjectUtils;

/**
 * The LocalRegionNamespaceTest class is a test suite of test cases testing the contract and functionality
 * of GemFire's Local Region support in SDG.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.LocalRegionFactoryBean
 * @see org.springframework.data.gemfire.config.xml.LocalRegionParser
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(locations="local-ns.xml",
	initializers = GemFireMockObjectsApplicationContextInitializer.class)
public class LocalRegionNamespaceTest {

	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void testSimpleLocalRegion() throws Exception {

		assertTrue(applicationContext.containsBean("simple"));

		Region<?, ?> simple = applicationContext.getBean("simple", Region.class);

		assertNotNull("The 'simple' Region was not properly configured or initialized!", simple);
		assertEquals("simple", simple.getName());
		assertEquals(Region.SEPARATOR + "simple", simple.getFullPath());
		assertNotNull(simple.getAttributes());
		assertEquals(DataPolicy.NORMAL, simple.getAttributes().getDataPolicy());
	}

	@Test
	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testPublisherLocalRegion() throws Exception {

		assertTrue(applicationContext.containsBean("pub"));

		RegionFactoryBean publisherRegionFactoryBean = applicationContext.getBean("&pub", RegionFactoryBean.class);

		assertNotNull(publisherRegionFactoryBean);
		assertEquals(DataPolicy.NORMAL, TestUtils.readField("dataPolicy", publisherRegionFactoryBean));
		assertEquals("publisher", TestUtils.readField("name", publisherRegionFactoryBean));
		assertEquals(Scope.LOCAL, TestUtils.readField("scope", publisherRegionFactoryBean));

		RegionAttributes publisherRegionAttributes = TestUtils.readField("attributes", publisherRegionFactoryBean);

		assertNotNull(publisherRegionAttributes);
		assertFalse(publisherRegionAttributes.getPublisher());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testComplexLocal() throws Exception {

		assertTrue(applicationContext.containsBean("complex"));

		RegionFactoryBean complexRegionFactoryBean = applicationContext.getBean("&complex", RegionFactoryBean.class);

		assertNotNull(complexRegionFactoryBean);

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
	public void testLocalWithAttributes() throws Exception {

		assertTrue(applicationContext.containsBean("local-with-attributes"));

		Region region = applicationContext.getBean("local-with-attributes", Region.class);

		assertNotNull("The 'local-with-attributes' Region was not properly configured and initialized!", region);
		assertEquals("local-with-attributes", region.getName());
		assertEquals(Region.SEPARATOR + "local-with-attributes", region.getFullPath());

		RegionAttributes localRegionAttributes = region.getAttributes();

		assertEquals(DataPolicy.PRELOADED, localRegionAttributes.getDataPolicy());
		assertTrue(localRegionAttributes.isDiskSynchronous());
		assertTrue(localRegionAttributes.getIgnoreJTA());
		assertFalse(localRegionAttributes.getIndexMaintenanceSynchronous());
		assertEquals(10, localRegionAttributes.getInitialCapacity());
		assertEquals(String.class, localRegionAttributes.getKeyConstraint());
		assertEquals("0.9", String.valueOf(localRegionAttributes.getLoadFactor()));
		assertTrue(localRegionAttributes.getOffHeap());
		assertEquals(String.class, localRegionAttributes.getValueConstraint());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testRegionLookup() throws Exception {

		Cache cache = applicationContext.getBean(Cache.class);

		Region existing = cache.createRegionFactory().create("existing");

		assertTrue(applicationContext.containsBean("lookup"));

		RegionLookupFactoryBean localRegionFactoryBean = applicationContext.getBean("&lookup", RegionLookupFactoryBean.class);

		assertEquals("existing", TestUtils.readField("name", localRegionFactoryBean));
		assertSame(existing, applicationContext.getBean("lookup"));
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testLocalPersistent() {

		Region persistentLocalRegion = applicationContext.getBean("persistent", Region.class);

		assertNotNull("The 'persistent' Local Region was not properly configured and initialized!", persistentLocalRegion);
		assertEquals("persistent", persistentLocalRegion.getName());
		assertEquals(Region.SEPARATOR + "persistent", persistentLocalRegion.getFullPath());

		RegionAttributes persistentRegionAttributes = persistentLocalRegion.getAttributes();

		assertNotNull(persistentRegionAttributes);
		assertTrue(persistentRegionAttributes.getDataPolicy().withPersistence());
	}

	@Test
	public void testCompressedLocalRegion() {

		assertTrue(applicationContext.containsBean("Compressed"));

		Region<?, ?> compressed = applicationContext.getBean("Compressed", Region.class);

		assertNotNull("The 'Compressed' Local Region was not properly configured and initialized!", compressed);
		assertEquals("Compressed", compressed.getName());
		assertEquals(Region.SEPARATOR + "Compressed", compressed.getFullPath());
		assertNotNull(compressed.getAttributes());
		assertEquals(DataPolicy.NORMAL, compressed.getAttributes().getDataPolicy());
		assertEquals(Scope.LOCAL, compressed.getAttributes().getScope());
		assertTrue(compressed.getAttributes().getCompressor() instanceof TestCompressor);
		assertEquals("ABC", compressed.getAttributes().getCompressor().toString());
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
