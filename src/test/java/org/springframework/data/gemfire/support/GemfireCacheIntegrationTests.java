/*
 * Copyright 2010-104 the original author or authors.
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

package org.springframework.data.gemfire.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Properties;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicBoolean;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.Region;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.cache.Cache;
import org.springframework.data.gemfire.GemfireUtils;

/**
 * Integration Tests for {@link GemfireCache}.
 *
 * @author Costin Leau
 * @author John Blum
 * @author Oliver Gierke
 * @see org.junit.Test
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.springframework.cache.Cache
 * @see com.gemstone.gemfire.cache.Region
 */
public class GemfireCacheIntegrationTests extends AbstractNativeCacheTests<Region<Object, Object>> {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Override
	protected Cache newCache(Region<Object, Object> nativeCache) {
		return new GemfireCache(nativeCache);
	}

	@Override
	protected Region<Object, Object> newNativeCache() throws Exception {
		Properties gemfireProperties = new Properties();

		gemfireProperties.setProperty("name", GemfireCacheIntegrationTests.class.getName());
		gemfireProperties.setProperty("mcast-port", "0");
		gemfireProperties.setProperty("locators", "");
		gemfireProperties.setProperty("log-level", "warning");

		com.gemstone.gemfire.cache.Cache cache = GemfireUtils.getCache();

		cache = (cache != null ? cache : new CacheFactory(gemfireProperties).create());

		Region<Object, Object> region = cache.getRegion(CACHE_NAME);

		region = (region != null ? region : cache.createRegionFactory().create(CACHE_NAME));

		return region;
	}

	/**
	 * @see <a href="https://jira.spring.io/browse/SGF-317">Improve GemfireCache implementation to be able to build on Spring 4.1</a>
	 */
	@Test
	public void findsTypedValue() throws Exception {
		Cache cache = newCache();

		cache.put("key", "value");

		assertThat(cache.get("key", String.class)).isEqualTo("value");
	}

	/**
	 * @see <a href="https://jira.spring.io/browse/SGF-317">Improve GemfireCache implementation to be able to build on Spring 4.1</a>
	 */
	@Test
	public void skipTypeChecksIfTargetTypeIsNull() throws Exception {
		Cache cache = newCache();

		cache.put("key", 1);

		assertThat(cache.get("key", (Class<?>) null)).isEqualTo(1);
	}

	/**
	 * @see <a href="https://jira.spring.io/browse/SGF-317">Improve GemfireCache implementation to be able to build on Spring 4.1</a>
	 */
	@Test
	public void throwsIllegalStateExceptionIfTypedAccessDoesNotFindMatchingType() throws Exception {
		Cache cache = newCache();

		cache.put("key", "value");

		exception.expect(IllegalStateException.class);
		exception.expectMessage(Integer.class.getName());
		exception.expectMessage(String.format("Cached value [value] is not an instance of type [%s]",
			Integer.class.getName()));

		cache.get("key", Integer.class);
	}

	@Test
	public void cacheGetWithValueLoaderFindsValue() throws Exception {
		GemfireCache cache = newCache();

		cache.put("key", "value");

		assertThat(cache.get("key", TestValueLoader.NULL_VALUE)).isEqualTo("value");
		assertThat(TestValueLoader.NULL_VALUE.wasCalled()).isFalse();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void cacheGetWithValueLoaderUsesValueLoaderReturnsValue() throws Exception {
		GemfireCache cache = newCache();

		TestValueLoader<String> valueLoader = new TestValueLoader<String>("test");

		assertThat(cache.get("key", valueLoader)).isEqualTo("test");
		assertThat(valueLoader.wasCalled()).isTrue();
		assertThat(((Region<Object, String>) cache.getNativeCache()).get("key")).isEqualTo("test");
	}

	@Test
	@SuppressWarnings("unchecked")
	public void cacheGetWithValueLoaderUsesValueLoaderReturnsNull() throws Exception {
		GemfireCache cache = newCache();

		assertThat(cache.get("key", TestValueLoader.NULL_VALUE)).isNull();
		assertThat(TestValueLoader.NULL_VALUE.wasCalled()).isTrue();
		assertThat(cache.getNativeCache().containsKey("key")).isFalse();
	}

	@Test
	@SuppressWarnings("all")
	public void cacheGetWithValueLoaderUsesValueLoaderAndThrowsException() throws Exception {
		GemfireCache cache = newCache();

		try {
			TestValueLoader<Exception> exceptionThrowingValueLoader = new TestValueLoader<Exception>(
				new IllegalStateException("test"));

			exception.expect(Cache.ValueRetrievalException.class);
			exception.expectCause(is(IllegalStateException.class));

			cache.get("key", exceptionThrowingValueLoader);
		}
		finally {
			assertThat(cache.getNativeCache().containsKey("key")).isFalse();
		}
	}

	@Test
	public void cacheGetWithValueLoaderIsThreadSafe() throws Throwable {
		TestFramework.runOnce(new CacheGetWithValueLoaderIsThreadSafe());
	}

	@SuppressWarnings("unused")
	protected class CacheGetWithValueLoaderIsThreadSafe extends MultithreadedTestCase {

		private GemfireCache cache;

		private TestValueLoader<String> cacheLoader;

		@Override
		public void initialize(){
			super.initialize();

			cache = newCacheHandlesCheckedException();

			cacheLoader = new TestValueLoader<String>("test") {
				@Override public String call() throws Exception {
					waitForTick(2);
					return super.call();
				}
			};
		}

		<T extends Cache> T newCacheHandlesCheckedException() {
			try {
				return newCache();
			}
			catch (Exception e) {
				throw new RuntimeException("Failed to create Cache", e);
			}
		}

		public void thread1() {
			assertTick(0);

			Thread.currentThread().setName("Cache Loader Thread");

			String value = cache.get("key", cacheLoader);

			assertTick(2);
			assertThat(value).isEqualTo("test");
			assertThat(cacheLoader.wasCalled()).isTrue();
		}

		public void thread2() {
			waitForTick(1);

			Thread.currentThread().setName("Cache Reader Thread");

			TestValueLoader<String> illegalCacheLoader = new TestValueLoader<String>("illegal");

			String value = cache.get("key", illegalCacheLoader);

			assertTick(2);
			assertThat(value).isEqualTo("test");
			assertThat(illegalCacheLoader.wasCalled()).isFalse();
		}
	}

	protected static class TestValueLoader<T> implements Callable<T> {

		protected static final TestValueLoader<Object> NULL_VALUE = new TestValueLoader<Object>();

		private AtomicBoolean called = new AtomicBoolean(false);

		private final T value;

		public TestValueLoader() {
			this(null);
		}

		public TestValueLoader(T value) {
			this.value = value;
		}

		protected boolean wasCalled() {
			return called.compareAndSet(true, false);
		}

		@Override
		public T call() throws Exception {
			called.set(true);

			if (value instanceof Exception) {
				throw (Exception) value;
			}

			return value;
		}
	}

}
