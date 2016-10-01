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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FilenameFilter;
import java.util.concurrent.atomic.AtomicReference;

import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheLoaderException;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAlgorithm;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.InterestResultPolicy;
import com.gemstone.gemfire.cache.LoaderHelper;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.util.CacheWriterAdapter;
import com.gemstone.gemfire.compression.Compressor;

import org.junit.AfterClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.SimpleCacheListener;
import org.springframework.data.gemfire.SimpleObjectSizer;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.Interest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

/**
 * The ClientRegionNamespaceTest class is a test suite of test cases testing the contract and functionality
 * of GemFire Client Region namespace support in SDG.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.xml.ClientRegionParser
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="client-ns.xml")
@SuppressWarnings("unused")
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
	public void testBeanNames() throws Exception {
		assertTrue(context.containsBean("SimpleRegion"));
		assertTrue(context.containsBean("Publisher"));
		assertTrue(context.containsBean("ComplexRegion"));
		assertTrue(context.containsBean("PersistentRegion"));
		assertTrue(context.containsBean("OverflowRegion"));
		assertTrue(context.containsBean("Compressed"));
	}

	@Test
	public void testSimpleClientRegion() throws Exception {
		assertTrue(context.containsBean("simple"));

		Region<?, ?> simple = context.getBean("simple", Region.class);

		assertNotNull("The 'SimpleRegion' Client Region was not properly configured and initialized!", simple);
		assertEquals("SimpleRegion", simple.getName());
		assertEquals(Region.SEPARATOR + "SimpleRegion", simple.getFullPath());
		assertNotNull(simple.getAttributes());
		assertEquals(DataPolicy.NORMAL, simple.getAttributes().getDataPolicy());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testPublishingClientRegion() throws Exception {
		assertTrue(context.containsBean("empty"));

		ClientRegionFactoryBean emptyClientRegionFactoryBean = context.getBean("&empty", ClientRegionFactoryBean.class);

		assertNotNull(emptyClientRegionFactoryBean);
		assertEquals(DataPolicy.EMPTY, TestUtils.readField("dataPolicy", emptyClientRegionFactoryBean));
		assertEquals("empty", TestUtils.readField("beanName", emptyClientRegionFactoryBean));
		assertEquals("Publisher", TestUtils.readField("name", emptyClientRegionFactoryBean));
		assertEquals("gemfire-pool", TestUtils.readField("poolName", emptyClientRegionFactoryBean));
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testComplexClientRegion() throws Exception {
		assertTrue(context.containsBean("complex"));

		ClientRegionFactoryBean complexClientRegionFactoryBean = context.getBean("&complex", ClientRegionFactoryBean.class);

		assertNotNull(complexClientRegionFactoryBean);

		CacheListener[] cacheListeners = TestUtils.readField("cacheListeners", complexClientRegionFactoryBean);

		assertFalse(ObjectUtils.isEmpty(cacheListeners));
		assertEquals(2, cacheListeners.length);
		assertSame(cacheListeners[0], context.getBean("c-listener"));
		assertTrue(cacheListeners[1] instanceof SimpleCacheListener);
		assertNotSame(cacheListeners[0], cacheListeners[1]);

		RegionAttributes complexRegionAttributes = TestUtils.readField("attributes", complexClientRegionFactoryBean);

		assertNotNull(complexRegionAttributes);
		assertEquals(0.5f, complexRegionAttributes.getLoadFactor(), 0.001);
		assertEquals(ExpirationAction.INVALIDATE, complexRegionAttributes.getEntryTimeToLive().getAction());
		assertEquals(500, complexRegionAttributes.getEntryTimeToLive().getTimeout());
		assertEquals(5, complexRegionAttributes.getEvictionAttributes().getMaximum());
	}

	@Test
	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testPersistentClientRegion() throws Exception {
		assertTrue(context.containsBean("persistent"));

		Region persistent = context.getBean("persistent", Region.class);

		assertNotNull("The 'PersistentRegion' Region was not properly configured and initialized!", persistent);
		assertEquals("PersistentRegion", persistent.getName());
		assertEquals(Region.SEPARATOR + "PersistentRegion", persistent.getFullPath());

		RegionAttributes persistentRegionAttributes = persistent.getAttributes();

		assertEquals(DataPolicy.PERSISTENT_REPLICATE, persistentRegionAttributes.getDataPolicy());
		assertEquals("diskStore", persistentRegionAttributes.getDiskStoreName());
		assertEquals("gemfire-pool", persistentRegionAttributes.getPoolName());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testOverflowClientRegion() throws Exception {
		assertTrue(context.containsBean("overflow"));

		ClientRegionFactoryBean overflowClientRegionFactoryBean = context.getBean("&overflow", ClientRegionFactoryBean.class);

		assertNotNull(overflowClientRegionFactoryBean);
		assertEquals("diskStore", TestUtils.readField("diskStoreName", overflowClientRegionFactoryBean));
		assertEquals("gemfire-pool", TestUtils.readField("poolName", overflowClientRegionFactoryBean));

		RegionAttributes overflowRegionAttributes = TestUtils.readField("attributes", overflowClientRegionFactoryBean);

		assertNotNull(overflowRegionAttributes);
		assertEquals(DataPolicy.NORMAL, overflowRegionAttributes.getDataPolicy());

		EvictionAttributes overflowEvictionAttributes = overflowRegionAttributes.getEvictionAttributes();

		assertNotNull(overflowEvictionAttributes);
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, overflowEvictionAttributes.getAction());
		assertEquals(EvictionAlgorithm.LRU_MEMORY, overflowEvictionAttributes.getAlgorithm());
		assertEquals(10, overflowEvictionAttributes.getMaximum());
		assertTrue(overflowEvictionAttributes.getObjectSizer() instanceof SimpleObjectSizer);
	}

	@Test
	public void testClientRegionWithCacheLoaderAndCacheWriter() throws Exception {
		assertTrue(context.containsBean("loadWithWrite"));

		ClientRegionFactoryBean factory = context.getBean("&loadWithWrite", ClientRegionFactoryBean.class);

		assertNotNull(factory);
		assertEquals("LoadedFullOfWrites", TestUtils.readField("name", factory));
		assertEquals(ClientRegionShortcut.LOCAL, TestUtils.readField("shortcut", factory));
		assertTrue(TestUtils.readField("cacheLoader", factory) instanceof TestCacheLoader);
		assertTrue(TestUtils.readField("cacheWriter", factory) instanceof TestCacheWriter);
	}

	@Test
	public void testCompressedReplicateRegion() {
		assertTrue(context.containsBean("Compressed"));

		Region<?, ?> compressed = context.getBean("Compressed", Region.class);

		assertNotNull("The 'Compressed' Client Region was not properly configured and initialized!", compressed);
		assertEquals("Compressed", compressed.getName());
		assertEquals(Region.SEPARATOR + "Compressed", compressed.getFullPath());
		assertNotNull(compressed.getAttributes());
		assertEquals(DataPolicy.EMPTY, compressed.getAttributes().getDataPolicy());
		assertEquals("gemfire-pool", compressed.getAttributes().getPoolName());
		assertTrue(String.format("Expected 'TestCompressor'; but was '%1$s'!",
			ObjectUtils.nullSafeClassName(compressed.getAttributes().getCompressor())),
				compressed.getAttributes().getCompressor() instanceof TestCompressor);
		assertEquals("STD", compressed.getAttributes().getCompressor().toString());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testClientRegionWithAttributes() {
		assertTrue(context.containsBean("client-with-attributes"));

		Region<Long, String> clientRegion = context.getBean("client-with-attributes", Region.class);

		assertNotNull("The 'client-with-attributes' Client Region was not properly configured and initialized!", clientRegion);
		assertEquals("client-with-attributes", clientRegion.getName());
		assertEquals(Region.SEPARATOR + "client-with-attributes", clientRegion.getFullPath());
		assertNotNull(clientRegion.getAttributes());
		assertFalse(clientRegion.getAttributes().getCloningEnabled());
		assertTrue(clientRegion.getAttributes().getConcurrencyChecksEnabled());
		assertEquals(8, clientRegion.getAttributes().getConcurrencyLevel());
		assertEquals(DataPolicy.NORMAL, clientRegion.getAttributes().getDataPolicy());
		assertFalse(clientRegion.getAttributes().getDataPolicy().withPersistence());
		assertEquals(64, clientRegion.getAttributes().getInitialCapacity());
		assertEquals(Long.class, clientRegion.getAttributes().getKeyConstraint());
		assertEquals("0.85", String.valueOf(clientRegion.getAttributes().getLoadFactor()));
		assertEquals("gemfire-pool", clientRegion.getAttributes().getPoolName());
		assertEquals(String.class, clientRegion.getAttributes().getValueConstraint());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testClientRegionWithRegisteredInterests() throws Exception {
		assertTrue(context.containsBean("client-with-interests"));

		ClientRegionFactoryBean factory = context.getBean("&client-with-interests", ClientRegionFactoryBean.class);

		assertNotNull(factory);

		Interest<?>[] interests = TestUtils.readField("interests", factory);

		assertNotNull(interests);
		assertEquals(2, interests.length);

		assertInterest(true, false, InterestResultPolicy.KEYS, getInterestWithKey(".*", interests));
		assertInterest(true, false, InterestResultPolicy.KEYS_VALUES, getInterestWithKey("keyPrefix.*", interests));

		Region mockClientRegion = MockCacheFactoryBean.MOCK_REGION_REF.get();

		assertNotNull(mockClientRegion);

		verify(mockClientRegion, times(1)).registerInterest(eq(".*"), eq(InterestResultPolicy.KEYS),
			eq(true), eq(false));
		verify(mockClientRegion, times(1)).registerInterestRegex(eq("keyPrefix.*"), eq(InterestResultPolicy.KEYS_VALUES),
			eq(true), eq(false));
	}

	protected void assertInterest(final boolean expectedDurable, final boolean expectedReceiveValues,
			final InterestResultPolicy expectedPolicy, final Interest actualInterest) {
		assertNotNull(actualInterest);
		assertEquals(expectedDurable, actualInterest.isDurable());
		assertEquals(expectedReceiveValues, actualInterest.isReceiveValues());
		assertEquals(expectedPolicy, actualInterest.getPolicy());
	}

	protected Interest getInterestWithKey(final String key, final Interest... interests) {
		for (Interest interest : interests) {
			if (interest.getKey().equals(key)) {
				return interest;
			}
		}

		return null;
	}

	public static final class MockCacheFactoryBean implements FactoryBean<ClientCache>, InitializingBean {

		protected static final AtomicReference<Region> MOCK_REGION_REF = new AtomicReference<Region>(null);

		private ClientCache mockClientCache;

		@Override
		@SuppressWarnings("unchecked")
		public void afterPropertiesSet() throws Exception {
			mockClientCache = mock(ClientCache.class, ClientRegionNamespaceTest.class.getSimpleName()
				.concat(".MockClientCache"));

			MOCK_REGION_REF.compareAndSet(null, mock(Region.class, ClientRegionNamespaceTest.class.getSimpleName()
				.concat(".MockClientRegion")));

			when(mockClientCache.getRegion(anyString())).thenReturn(MOCK_REGION_REF.get());
		}

		@Override
		public ClientCache getObject() throws Exception {
			return mockClientCache;
		}

		@Override
		public Class<?> getObjectType() {
			return ClientCache.class;
		}

		@Override
		public boolean isSingleton() {
			return true;
		}
	}

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
