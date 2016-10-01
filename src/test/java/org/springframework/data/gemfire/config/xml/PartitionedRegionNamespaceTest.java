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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.FixedPartitionAttributes;
import com.gemstone.gemfire.cache.PartitionAttributes;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.partition.PartitionListener;
import com.gemstone.gemfire.cache.partition.PartitionListenerAdapter;
import com.gemstone.gemfire.compression.Compressor;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.SimpleCacheListener;
import org.springframework.data.gemfire.SimplePartitionResolver;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

/**
 * The PartitionRegionNamespaceTest class is a test suite of test cases testing the contract and functionality
 * of the GemFire Partition Region support in SDG.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.PartitionedRegionFactoryBean
 * @see org.springframework.data.gemfire.config.xml.PartitionedRegionParser
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "partitioned-ns.xml", initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class PartitionedRegionNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testSimplePartitionRegion() throws Exception {
		assertTrue(context.containsBean("simple"));

		Region<?, ?> simple = context.getBean("simple", Region.class);

		assertNotNull(simple);
		assertEquals("simple", simple.getName());
		assertEquals(Region.SEPARATOR + "simple", simple.getFullPath());
		assertNotNull(simple.getAttributes());
		assertEquals(DataPolicy.PARTITION, simple.getAttributes().getDataPolicy());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testOptionsPartitionRegion() throws Exception {
		assertTrue(context.containsBean("options"));

		RegionFactoryBean optionsRegionFactoryBean = context.getBean("&options", RegionFactoryBean.class);

		assertTrue(optionsRegionFactoryBean instanceof PartitionedRegionFactoryBean);
		assertEquals(null, TestUtils.readField("scope", optionsRegionFactoryBean));
		assertEquals("redundant", TestUtils.readField("name", optionsRegionFactoryBean));

		RegionAttributes optionsRegionAttributes = TestUtils.readField("attributes", optionsRegionFactoryBean);

		assertNotNull(optionsRegionAttributes);
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
		assertTrue(context.containsBean("complex"));

		RegionFactoryBean complexRegionFactoryBean = context.getBean("&complex", RegionFactoryBean.class);

		CacheListener[] cacheListeners = TestUtils.readField("cacheListeners", complexRegionFactoryBean);

		assertFalse(ObjectUtils.isEmpty(cacheListeners));
		assertEquals(2, cacheListeners.length);
		assertSame(cacheListeners[0], context.getBean("c-listener"));
		assertTrue(cacheListeners[1] instanceof SimpleCacheListener);

		assertSame(context.getBean("c-loader"), TestUtils.readField("cacheLoader", complexRegionFactoryBean));
		assertSame(context.getBean("c-writer"), TestUtils.readField("cacheWriter", complexRegionFactoryBean));

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
		assertTrue(context.containsBean("compressed"));

		Region<?, ?> compressed = context.getBean("compressed", Region.class);

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
		RegionFactoryBean fixedRegionFactoryBean = context.getBean("&fixed", RegionFactoryBean.class);

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
		assertTrue(context.containsBean("listeners"));

		Region<?, ?> listeners = context.getBean("listeners", Region.class);

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
		assertTrue(context.containsBean("listenerRef"));

		Region<?, ?> listeners = context.getBean("listenerRef", Region.class);

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
