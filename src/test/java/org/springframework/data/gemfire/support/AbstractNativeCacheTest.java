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

package org.springframework.data.gemfire.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.junit.Before;
import org.junit.Test;
import org.springframework.cache.Cache;

/**
 * Test for native cache implementations.
 * 
 * @author Costin Leau
 */
public abstract class AbstractNativeCacheTest<T> {

	protected static final String CACHE_NAME = "testCache";

	private T nativeCache;
	private Cache cache;

	@Before
	public void setUp() throws Exception {
		nativeCache = createNativeCache();
		cache = createCache(nativeCache);
		cache.clear();
	}

	protected abstract T createNativeCache() throws Exception;

	protected abstract Cache createCache(T nativeCache);

	@Test
	public void testCacheName() throws Exception {
		assertEquals(CACHE_NAME, cache.getName());
	}

	@Test
	public void testNativeCache() throws Exception {
		assertSame(nativeCache, cache.getNativeCache());
	}

	@Test
	public void testCachePut() throws Exception {
		Object key = "enescu";
		Object value = "george";

		assertNull(cache.get(key));
		cache.put(key, value);
		assertEquals(value, cache.get(key).get());
	}

	@Test
	public void testCacheClear() throws Exception {
		assertNull(cache.get("enescu"));
		cache.put("enescu", "george");
		assertNull(cache.get("vlaicu"));
		cache.put("vlaicu", "aurel");
		cache.clear();
		assertNull(cache.get("vlaicu"));
		assertNull(cache.get("enescu"));
	}

	@Test
	public void testCacheGetForClassType() {
		cache.put("one", Boolean.TRUE);
		cache.put("two", 'X');
		cache.put("three", 101);
		cache.put("four", Math.PI);
		cache.put("five", "TEST");

		assertEquals(Boolean.TRUE, cache.get("one", Boolean.class));
		assertEquals(new Character('X'), cache.get("two", Character.class));
		assertEquals(new Integer(101), cache.get("three", Integer.class));
		assertEquals(new Double(Math.PI), cache.get("four", Double.class));
		assertEquals("TEST", cache.get("five", String.class));
	}

}
