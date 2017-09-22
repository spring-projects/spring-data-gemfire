/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.Resource;

import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.CacheWriterException;
import org.apache.geode.cache.EntryEvent;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.util.CacheWriterAdapter;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * The ClientRegionWithCacheLoaderWriterTest class is a test suite of test cases testing the addition of CacheLoaders
 * and CacheWriters to a client, local Region inside a GemFire Cache.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.CacheLoader
 * @see org.apache.geode.cache.CacheWriter
 * @see org.apache.geode.cache.Region
 * @since 1.3.3
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ClientRegionWithCacheLoaderWriterTest {

	private static final int REGION_SIZE = 100;

	@Autowired
	private ApplicationContext context;

	@Resource(name = "localAppDataRegion")
	private Region<Integer, Integer> localAppData;

	@Test
	public void testCacheLoaderWriter() {

		assertNotNull(localAppData);
		assertEquals(0, localAppData.size());

		for (int key = 0; key < REGION_SIZE; key++) {
			assertEquals(key + 1, localAppData.get(key).intValue());
		}

		assertEquals(REGION_SIZE, localAppData.size());

		for (int key = 0; key < REGION_SIZE; key++) {
			assertEquals(key + 1, localAppData.put(key, REGION_SIZE - key).intValue());
		}

		LocalAppDataCacheWriter localCacheWriter = context.getBean("localCacheWriter", LocalAppDataCacheWriter.class);

		assertNotNull(localCacheWriter);

		for (int key = 0; key < REGION_SIZE; key++) {
			assertEquals(REGION_SIZE - key, localCacheWriter.get(key).intValue());
		}
	}

	public static class LocalAppDataCacheLoader implements CacheLoader<Integer, Integer> {

		private static final AtomicInteger VALUE_GENERATOR = new AtomicInteger(0);

		@Override
		public Integer load(final LoaderHelper<Integer, Integer> helper) throws CacheLoaderException {
			return VALUE_GENERATOR.incrementAndGet();
		}

		@Override
		public void close() {
		}
	}

	public static class LocalAppDataCacheWriter extends CacheWriterAdapter<Integer, Integer> {

		private static final Map<Integer, Integer> data = new ConcurrentHashMap<>(REGION_SIZE);

		@Override
		public void beforeUpdate(final EntryEvent<Integer, Integer> event) throws CacheWriterException {
			data.put(event.getKey(), event.getNewValue());
		}

		public Integer get(final Integer key) {
			return data.get(key);
		}
	}

}
