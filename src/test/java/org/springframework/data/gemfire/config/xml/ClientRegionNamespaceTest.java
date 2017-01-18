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

import static java.util.Arrays.stream;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.io.File;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.EvictionAlgorithm;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.InterestResultPolicy;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.util.CacheWriterAdapter;
import org.apache.geode.compression.Compressor;
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
 * Unit tests for Spring Data GemFire's XML namespace support for client {@link Region Regions}.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.xml.ClientRegionParser
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="client-ns.xml")
@SuppressWarnings("unused")
public class ClientRegionNamespaceTest {

	@Autowired
	private ApplicationContext applicationContext;

	@AfterClass
	public static void tearDown() {
		stream(nullSafeArray(new File(".").list((dir, name) -> name.startsWith("BACKUP")), String.class))
			.forEach(fileName -> new File(fileName).delete());
	}

	@Test
	public void testBeanNames() throws Exception {
		assertTrue(applicationContext.containsBean("SimpleRegion"));
		assertTrue(applicationContext.containsBean("Publisher"));
		assertTrue(applicationContext.containsBean("ComplexRegion"));
		assertTrue(applicationContext.containsBean("PersistentRegion"));
		assertTrue(applicationContext.containsBean("OverflowRegion"));
		assertTrue(applicationContext.containsBean("Compressed"));
	}

	@Test
	public void testSimpleClientRegion() throws Exception {
		assertTrue(applicationContext.containsBean("simple"));

		Region<?, ?> simple = applicationContext.getBean("simple", Region.class);

		assertNotNull("The 'SimpleRegion' Client Region was not properly configured and initialized!", simple);
		assertEquals("SimpleRegion", simple.getName());
		assertEquals(Region.SEPARATOR + "SimpleRegion", simple.getFullPath());
		assertNotNull(simple.getAttributes());
		assertEquals(DataPolicy.NORMAL, simple.getAttributes().getDataPolicy());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testPublishingClientRegion() throws Exception {
		assertTrue(applicationContext.containsBean("empty"));

		ClientRegionFactoryBean emptyClientRegionFactoryBean = applicationContext
			.getBean("&empty", ClientRegionFactoryBean.class);

		assertNotNull(emptyClientRegionFactoryBean);
		assertEquals(DataPolicy.EMPTY, TestUtils.readField("dataPolicy", emptyClientRegionFactoryBean));
		assertEquals("empty", TestUtils.readField("beanName", emptyClientRegionFactoryBean));
		assertEquals("Publisher", TestUtils.readField("name", emptyClientRegionFactoryBean));
		assertEquals("gemfire-pool", TestUtils.readField("poolName", emptyClientRegionFactoryBean));
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testComplexClientRegion() throws Exception {
		assertTrue(applicationContext.containsBean("complex"));

		ClientRegionFactoryBean complexClientRegionFactoryBean = applicationContext
			.getBean("&complex", ClientRegionFactoryBean.class);

		assertNotNull(complexClientRegionFactoryBean);

		CacheListener[] cacheListeners = TestUtils.readField("cacheListeners", complexClientRegionFactoryBean);

		assertFalse(ObjectUtils.isEmpty(cacheListeners));
		assertEquals(2, cacheListeners.length);
		assertSame(cacheListeners[0], applicationContext.getBean("c-listener"));
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
		assertTrue(applicationContext.containsBean("persistent"));

		Region persistent = applicationContext.getBean("persistent", Region.class);

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
		assertTrue(applicationContext.containsBean("overflow"));

		ClientRegionFactoryBean overflowClientRegionFactoryBean = applicationContext
			.getBean("&overflow", ClientRegionFactoryBean.class);

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
		assertTrue(applicationContext.containsBean("loadWithWrite"));

		ClientRegionFactoryBean factory = applicationContext.getBean("&loadWithWrite", ClientRegionFactoryBean.class);

		assertNotNull(factory);
		assertEquals("LoadedFullOfWrites", TestUtils.readField("name", factory));
		assertEquals(ClientRegionShortcut.LOCAL, TestUtils.readField("shortcut", factory));
		assertTrue(TestUtils.readField("cacheLoader", factory) instanceof TestCacheLoader);
		assertTrue(TestUtils.readField("cacheWriter", factory) instanceof TestCacheWriter);
	}

	@Test
	public void testCompressedReplicateRegion() {
		assertTrue(applicationContext.containsBean("Compressed"));

		Region<?, ?> compressed = applicationContext.getBean("Compressed", Region.class);

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
		assertTrue(applicationContext.containsBean("client-with-attributes"));

		Region<Long, String> clientRegion = applicationContext.getBean("client-with-attributes", Region.class);

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

		assertTrue(applicationContext.containsBean("client-with-interests"));

		ClientRegionFactoryBean factoryBean =
			applicationContext.getBean("&client-with-interests", ClientRegionFactoryBean.class);

		assertNotNull(factoryBean);

		Interest<?>[] interests = TestUtils.readField("interests", factoryBean);

		assertNotNull(interests);
		assertEquals(2, interests.length);

		assertInterest(true, false, InterestResultPolicy.KEYS, getInterestWithKey(".*", interests));
		assertInterest(true, false, InterestResultPolicy.KEYS_VALUES, getInterestWithKey("keyPrefix.*", interests));

		Region mockClientRegion = MockCacheFactoryBean.MOCK_REGION_REF.get();

		assertNotNull(mockClientRegion);

		verify(mockClientRegion, times(1)).registerInterest(eq(".*"),
			eq(InterestResultPolicy.KEYS), eq(true), eq(false));

		verify(mockClientRegion, times(1)).registerInterestRegex(eq("keyPrefix.*"),
			eq(InterestResultPolicy.KEYS_VALUES), eq(true), eq(false));
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

	static final class MockCacheFactoryBean implements FactoryBean<ClientCache>, InitializingBean {

		static final AtomicReference<Region> MOCK_REGION_REF = new AtomicReference<Region>(null);

		private ClientCache mockClientCache;

		@Override
		@SuppressWarnings("unchecked")
		public void afterPropertiesSet() throws Exception {
			this.mockClientCache = mock(ClientCache.class,
				ClientRegionNamespaceTest.class.getSimpleName().concat(".MockClientCache"));

			ClientRegionFactory mockClientRegionFactory = mock(ClientRegionFactory.class,
				ClientRegionNamespaceTest.class.getSimpleName().concat("MockClientRegionFactory"));

			when(this.mockClientCache.createClientRegionFactory(any(ClientRegionShortcut.class)))
				.thenReturn(mockClientRegionFactory);

			Region mockRegion = mock(Region.class,
				ClientRegionNamespaceTest.class.getSimpleName().concat(".MockClientRegion"));

			when(mockClientRegionFactory.create(anyString())).thenReturn(mockRegion);

			MOCK_REGION_REF.compareAndSet(null, mockRegion);
		}

		@Override
		public ClientCache getObject() throws Exception {
			return this.mockClientCache;
		}

		@Override
		public Class<?> getObjectType() {
			return ClientCache.class;
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
