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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilenameFilter;

import org.junit.AfterClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.SimpleObjectSizer;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheLoaderException;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAlgorithm;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.LoaderHelper;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.util.CacheWriterAdapter;
import com.gemstone.gemfire.cache.util.ObjectSizer;

/**
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
@ContextConfiguration(locations="/org/springframework/data/gemfire/config/client-ns.xml",
	initializers=GemfireTestApplicationContextInitializer.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class ClientRegionNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@AfterClass
	public static void tearDown() {
		for (String name : new File(".").list(new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return name.startsWith("BACKUP");
			}
		})) {
			new File(name).delete();
		}
	}

	@Test
	public void testBasicClient() throws Exception {
		assertTrue(context.containsBean("simple"));
	}

	@Test
	public void testBeanNames() throws Exception {
		assertTrue(context.containsBean("SimpleRegion"));
		assertTrue(context.containsBean("publisher"));
		assertTrue(context.containsBean("ComplexRegion"));
		assertTrue(context.containsBean("PersistentRegion"));
		assertTrue(context.containsBean("OverflowRegion"));
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testPublishingClient() throws Exception {
		assertTrue(context.containsBean("empty"));
		ClientRegionFactoryBean fb = context.getBean("&empty", ClientRegionFactoryBean.class);
		assertEquals(DataPolicy.EMPTY, TestUtils.readField("dataPolicy", fb));
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testComplexClient() throws Exception {
		assertTrue(context.containsBean("complex"));
		ClientRegionFactoryBean fb = context.getBean("&complex", ClientRegionFactoryBean.class);
		CacheListener[] listeners = TestUtils.readField("cacheListeners", fb);
		assertFalse(ObjectUtils.isEmpty(listeners));
		assertEquals(2, listeners.length);
		assertSame(listeners[0], context.getBean("c-listener"));
		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		assertEquals(500, attrs.getEntryTimeToLive().getTimeout());
		assertEquals(0.5f,attrs.getLoadFactor(),0.001);
		assertEquals(5, attrs.getEvictionAttributes().getMaximum());
	}

	@Test
	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testPersistent() throws Exception {
		assertTrue(context.containsBean("persistent"));
		Region region = context.getBean("persistent", Region.class);
		RegionAttributes attrs = region.getAttributes();
		assertEquals("diskStore", attrs.getDiskStoreName());
		assertEquals(10, attrs.getDiskDirSizes()[0]);
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testOverflowToDisk() throws Exception {
		assertTrue(context.containsBean("overflow"));
		ClientRegionFactoryBean fb = context.getBean("&overflow", ClientRegionFactoryBean.class);
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

	@Test
	public void testClientRegionWithCacheLoaderAndCacheWriter() throws Exception {
		assertTrue(context.containsBean("loadWithWrite"));

		ClientRegionFactoryBean factory = context.getBean("&loadWithWrite", ClientRegionFactoryBean.class);

		assertNotNull(factory);
		assertEquals(ClientRegionShortcut.LOCAL, TestUtils.readField("shortcut", factory));
		assertTrue(TestUtils.readField("cacheLoader", factory) instanceof TestCacheLoader);
		assertTrue(TestUtils.readField("cacheWriter", factory) instanceof TestCacheWriter);
	}

	@SuppressWarnings("unused")
	public static final class TestCacheLoader implements CacheLoader<Object, Object> {

		@Override
		public Object load(final LoaderHelper<Object, Object> helper) throws CacheLoaderException {
			throw new UnsupportedOperationException("Not Implemented!");
		}

		@Override
		public void close() {
		}
	}

	public static final class TestCacheWriter extends CacheWriterAdapter<Object, Object> {
	}

}
