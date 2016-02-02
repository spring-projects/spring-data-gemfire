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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.springframework.cache.Cache;

/**
 * Abstract base test class for native cache implementations.
 * 
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.cache.Cache
 */
public abstract class AbstractNativeCacheTest<T> {

	protected static final String CACHE_NAME = "Example";

	private T nativeCache;
	private Cache cache;

	@Before
	public void setUp() throws Exception {
		nativeCache = createNativeCache();
		cache = createCache(nativeCache);
		cache.clear();
	}

	@SuppressWarnings("unchecked")
	protected <C extends Cache> C createCache() throws Exception {
		return (C) createCache(createNativeCache());
	}

	protected abstract Cache createCache(T nativeCache);

	protected abstract T createNativeCache() throws Exception;

	@Test
	public void cacheNameIsEqualToExpected() throws Exception {
		assertThat(cache.getName(), is(equalTo(CACHE_NAME)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void nativeCacheIsSameAsExpected() throws Exception {
		assertThat((T) cache.getNativeCache(), is(sameInstance(nativeCache)));
	}

	@Test
	public void cachePutIsSuccessful() throws Exception {
		assertThat(cache.get("enescu"), is(nullValue()));
		cache.put("enescu", "george");
		assertThat(String.valueOf(cache.get("enescu").get()), is(equalTo("george")));
	}

	@Test
	public void cacheGetForClassTypeIsSuccessful() {
		cache.put("one", Boolean.TRUE);
		cache.put("two", 'X');
		cache.put("three", 101);
		cache.put("four", Math.PI);
		cache.put("five", "TEST");

		assertThat(cache.get("one", Boolean.class), is(equalTo(Boolean.TRUE)));
		assertThat(cache.get("two", Character.class), is(equalTo('X')));
		assertThat(cache.get("three", Integer.class), is(equalTo(101)));
		assertThat(cache.get("four", Double.class), is(equalTo(Math.PI)));
		assertThat(cache.get("five", String.class), is(equalTo("TEST")));
	}

	@Test
	public void cacheClearIsSuccessful() throws Exception {
		assertThat(cache.get("enescu"), is(nullValue()));

		cache.put("enescu", "george");

		assertThat(cache.get("vlaicu"), is(nullValue()));

		cache.put("vlaicu", "aurel");

		assertThat(cache.get("enescu", String.class), is(equalTo("george")));
		assertThat(cache.get("vlaicu", String.class), is(equalTo("aurel")));

		cache.clear();

		assertThat(cache.get("vlaicu"), is(nullValue()));
		assertThat(cache.get("enescu"), is(nullValue()));
	}

}
