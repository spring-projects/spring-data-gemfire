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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.FixedPartitionAttributes;
import org.apache.geode.cache.PartitionAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.partition.PartitionListener;
import org.apache.geode.cache.partition.PartitionListenerAdapter;
import org.apache.geode.compression.Compressor;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.PeerRegionFactoryBean;
import org.springframework.data.gemfire.SimpleCacheListener;
import org.springframework.data.gemfire.SimplePartitionResolver;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.mock.context.GemFireMockObjectsApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.ObjectUtils;

/**
 * Unit tests for the Partitioned Region Namespace.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.PartitionedRegionFactoryBean
 * @see org.springframework.data.gemfire.config.xml.PartitionedRegionParser
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(locations = "partitioned-ns.xml",
	initializers = GemFireMockObjectsApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class PartitionedRegionNamespaceTest {

	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void testSimplePartitionRegion() throws Exception {

		assertTrue(applicationContext.containsBean("simple"));

		Region<?, ?> simple = applicationContext.getBean("simple", Region.class);

		assertNotNull(simple);
		assertEquals("simple", simple.getName());
		assertEquals(Region.SEPARATOR + "simple", simple.getFullPath());
		assertNotNull(simple.getAttributes());
		assertEquals(DataPolicy.PARTITION, simple.getAttributes().getDataPolicy());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testOptionsPartitionRegion() throws Exception {

		assertTrue(applicationContext.containsBean("options"));
		assertTrue(applicationContext.containsBean("redundant"));

		Region<?, ?> options = applicationContext.getBean("options", Region.class);

		assertThat(options).isNotNull();
		assertThat(options.getAttributes()).isNotNull();
		assertThat(options.getName()).isEqualTo("redundant");
		assertThat(options.getAttributes().getOffHeap()).isTrue();

		PeerRegionFactoryBean optionsRegionFactoryBean = applicationContext.getBean("&options", PeerRegionFactoryBean.class);

		assertTrue(optionsRegionFactoryBean instanceof PartitionedRegionFactoryBean);
		assertNull(TestUtils.readField("scope", optionsRegionFactoryBean));
		assertEquals("redundant", TestUtils.readField("name", optionsRegionFactoryBean));

		RegionAttributes optionsRegionAttributes = optionsRegionFactoryBean.getAttributes();

		assertNotNull(optionsRegionAttributes);
		assertTrue(optionsRegionAttributes.getOffHeap());
		assertTrue(optionsRegionAttributes.getStatisticsEnabled());

		PartitionAttributes optionsRegionPartitionAttributes = optionsRegionAttributes.getPartitionAttributes();

		assertNotNull(optionsRegionPartitionAttributes);
		assertEquals(1, optionsRegionPartitionAttributes.getRedundantCopies());
		assertEquals(4, optionsRegionPartitionAttributes.getTotalNumBuckets());
		assertTrue(optionsRegionPartitionAttributes.getPartitionResolver() instanceof SimplePartitionResolver);
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testComplexPartitionRegion() throws Exception {

		assertTrue(applicationContext.containsBean("complex"));

		PeerRegionFactoryBean complexRegionFactoryBean = applicationContext.getBean("&complex", PeerRegionFactoryBean.class);

		CacheListener[] cacheListeners = TestUtils.readField("cacheListeners", complexRegionFactoryBean);

		assertFalse(ObjectUtils.isEmpty(cacheListeners));
		assertEquals(2, cacheListeners.length);
		assertSame(cacheListeners[0], applicationContext.getBean("c-listener"));
		assertTrue(cacheListeners[1] instanceof SimpleCacheListener);

		assertSame(applicationContext.getBean("c-loader"), TestUtils.readField("cacheLoader", complexRegionFactoryBean));
		assertSame(applicationContext.getBean("c-writer"), TestUtils.readField("cacheWriter", complexRegionFactoryBean));

		RegionAttributes complexRegionAttributes = TestUtils.readField("attributes", complexRegionFactoryBean);

		assertNotNull(complexRegionAttributes);

		PartitionAttributes complexRegionPartitionAttributes = complexRegionAttributes.getPartitionAttributes();

		assertNotNull(complexRegionPartitionAttributes);
		assertEquals(20, complexRegionPartitionAttributes.getLocalMaxMemory());
		assertNotNull(complexRegionPartitionAttributes.getPartitionListeners());
		assertEquals(1, complexRegionPartitionAttributes.getPartitionListeners().length);
		assertTrue(complexRegionPartitionAttributes.getPartitionListeners()[0] instanceof TestPartitionListener);
	}

	@Test
	public void testCompressedPartitionRegion() {

		assertTrue(applicationContext.containsBean("compressed"));

		Region<?, ?> compressed = applicationContext.getBean("compressed", Region.class);

		assertNotNull("The 'compressed' PARTITION Region was not properly configured and initialized!", compressed);
		assertEquals("compressed", compressed.getName());
		assertEquals(Region.SEPARATOR + "compressed", compressed.getFullPath());
		assertNotNull(compressed.getAttributes());
		assertEquals(DataPolicy.PARTITION, compressed.getAttributes().getDataPolicy());
		assertTrue(compressed.getAttributes().getCompressor() instanceof TestCompressor);
		assertEquals("testCompressor", compressed.getAttributes().getCompressor().toString());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testFixedPartitionRegion() throws Exception {

		PeerRegionFactoryBean fixedRegionFactoryBean = applicationContext.getBean("&fixed", PeerRegionFactoryBean.class);

		assertNotNull(fixedRegionFactoryBean);

		RegionAttributes fixedRegionAttributes = TestUtils.readField("attributes", fixedRegionFactoryBean);

		assertNotNull(fixedRegionAttributes);

		PartitionAttributes fixedRegionPartitionAttributes = fixedRegionAttributes.getPartitionAttributes();

		assertNotNull(fixedRegionPartitionAttributes);

		assertNotNull(fixedRegionPartitionAttributes.getFixedPartitionAttributes());
		assertEquals(3, fixedRegionPartitionAttributes.getFixedPartitionAttributes().size());

		FixedPartitionAttributes fixedPartitionAttributes =
			(FixedPartitionAttributes) fixedRegionPartitionAttributes.getFixedPartitionAttributes().get(0);

		assertEquals(3, fixedPartitionAttributes.getNumBuckets());
		assertTrue(fixedPartitionAttributes.isPrimary());
	}

	@Test
	public void testMultiplePartitionListeners() {

		assertTrue(applicationContext.containsBean("listeners"));

		Region<?, ?> listeners = applicationContext.getBean("listeners", Region.class);

		assertNotNull("The 'listeners' PARTITION Region was not properly configured and initialized!", listeners);
		assertEquals("listeners", listeners.getName());
		assertEquals(Region.SEPARATOR + "listeners", listeners.getFullPath());
		assertNotNull(listeners.getAttributes());
		assertEquals(DataPolicy.PARTITION, listeners.getAttributes().getDataPolicy());

		PartitionAttributes listenersPartitionAttributes = listeners.getAttributes().getPartitionAttributes();

		assertNotNull(listenersPartitionAttributes);
		assertNotNull(listenersPartitionAttributes.getPartitionListeners());
		assertEquals(4, listenersPartitionAttributes.getPartitionListeners().length);

		List<String> expectedNames = Arrays.asList("X", "Y", "Z", "ABC");

		for (PartitionListener listener : listenersPartitionAttributes.getPartitionListeners()) {
			assertTrue(listener instanceof TestPartitionListener);
			assertTrue(expectedNames.contains(listener.toString()));
		}
	}

	@Test
	public void testSinglePartitionListeners() {

		assertTrue(applicationContext.containsBean("listenerRef"));

		Region<?, ?> listeners = applicationContext.getBean("listenerRef", Region.class);

		assertNotNull("The 'listenerRef' PARTITION Region was not properly configured and initialized!", listeners);
		assertEquals("listenerRef", listeners.getName());
		assertEquals(Region.SEPARATOR + "listenerRef", listeners.getFullPath());
		assertNotNull(listeners.getAttributes());
		assertEquals(DataPolicy.PARTITION, listeners.getAttributes().getDataPolicy());

		PartitionAttributes listenersPartitionAttributes = listeners.getAttributes().getPartitionAttributes();

		assertNotNull(listenersPartitionAttributes);
		assertNotNull(listenersPartitionAttributes.getPartitionListeners());
		assertEquals(1, listenersPartitionAttributes.getPartitionListeners().length);
		assertTrue(listenersPartitionAttributes.getPartitionListeners()[0] instanceof TestPartitionListener);
		assertEquals("ABC", listenersPartitionAttributes.getPartitionListeners()[0].toString());
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

	public static class TestPartitionListener extends PartitionListenerAdapter {

		private String name;

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public String toString() {
			return this.name;
		}
	}
}
