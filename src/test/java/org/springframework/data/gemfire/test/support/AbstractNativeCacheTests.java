/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.test.support;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.springframework.cache.Cache;

/**
 * Abstract base test class for native cache implementations.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.cache.Cache
 */
public abstract class AbstractNativeCacheTests<T> {

	protected static final String CACHE_NAME = "Example";

	private T nativeCache;
	private Cache cache;

	@Before
	public void setUp() throws Exception {
		nativeCache = newNativeCache();
		cache = newCache(nativeCache);
		cache.clear();
	}

	@SuppressWarnings("unchecked")
	protected <C extends Cache> C newCache() throws Exception {
		return (C) newCache(newNativeCache());
	}

	protected abstract Cache newCache(T nativeCache);

	protected abstract T newNativeCache() throws Exception;

	@Test
	public void cacheNameIsEqualToExpected() throws Exception {
		assertThat(cache.getName()).isEqualTo(CACHE_NAME);
	}

	@Test
	public void nativeCacheIsSameAsExpected() throws Exception {
		assertThat(cache.getNativeCache()).isSameAs(nativeCache);
	}

	@Test
	public void cachePutIsSuccessful() throws Exception {
		assertThat(cache.get("enescu")).isNull();

		cache.put("enescu", "george");

		assertThat(cache.get("enescu").get()).isEqualTo("george");
	}

	@Test
	public void cachePutThenClearIsSuccessful() throws Exception {
		cache.put("enescu", "george");
		cache.put("vlaicu", "aurel");

		assertThat(cache.get("enescu", String.class)).isEqualTo("george");
		assertThat(cache.get("vlaicu", String.class)).isEqualTo("aurel");

		cache.clear();

		assertThat(cache.get("vlaicu")).isNull();
		assertThat(cache.get("enescu")).isNull();
	}

	@Test
	public void cachePutThenGetForClassTypeIsSuccessful() {
		cache.put("one", Boolean.TRUE);
		cache.put("two", 'X');
		cache.put("three", 101);
		cache.put("four", Math.PI);
		cache.put("five", "TEST");

		assertThat(cache.get("one", Boolean.class)).isTrue();
		assertThat(cache.get("two", Character.class)).isEqualTo('X');
		assertThat(cache.get("three", Integer.class)).isEqualTo(101);
		assertThat(cache.get("four", Double.class)).isEqualTo(Math.PI);
		assertThat(cache.get("five", String.class)).isEqualTo("TEST");
	}
}
