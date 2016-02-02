/*
 * Copyright 2010-104 the original author or authors.
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
import static org.junit.Assert.assertThat;

import java.util.Properties;
import java.util.concurrent.Callable;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.cache.Cache;
import org.springframework.data.gemfire.GemfireUtils;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.Region;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Tests the interaction between the Spring Framework Cache Abstraction and GemFire as a provider
 * using Spring Data GemFire's extension.
 *
 * @author Costin Leau
 * @author John Blum
 * @author Oliver Gierke
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.springframework.cache.Cache
 * @see com.gemstone.gemfire.cache.Region
 */
public class GemfireCacheTest extends AbstractNativeCacheTest<Region<Object, Object>> {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Override
	protected Cache createCache(Region<Object, Object> nativeCache) {
		return new GemfireCache(nativeCache);
	}

	@Override
	protected Region<Object, Object> createNativeCache() throws Exception {
		Properties gemfireProperties = new Properties();

		gemfireProperties.setProperty("name", GemfireCacheTest.class.getName());
		gemfireProperties.setProperty("mcast-port", "0");
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
		Cache cache = createCache();

		cache.put("key", "value");

		assertThat(cache.get("key", String.class), is("value"));
	}

	/**
	 * @see <a href="https://jira.spring.io/browse/SGF-317">Improve GemfireCache implementation to be able to build on Spring 4.1</a>
	 */
	@Test
	public void skipTypeChecksIfTargetTypeIsNull() throws Exception {
		Cache cache = createCache();

		cache.put("key", "value");

		assertThat(cache.get("key", (Class<String>) null), is("value"));
	}

	/**
	 * @see <a href="https://jira.spring.io/browse/SGF-317">Improve GemfireCache implementation to be able to build on Spring 4.1</a>
	 */
	@Test
	public void throwsIllegalStateExceptionIfTypedAccessDoesNotFindMatchingType() throws Exception {
		Cache cache = createCache();

		cache.put("key", "value");

		expectedException.expect(IllegalStateException.class);
		expectedException.expectMessage(Integer.class.getName());
		expectedException.expectMessage("value");

		cache.get("key", Integer.class);
	}

	@Test
	public void cacheGetWithValueLoaderFindsValue() throws Exception {
		GemfireCache cache = createCache();

		cache.put("key", "value");

		assertThat(String.valueOf(cache.get("key", TestCacheLoader.NULL_VALUE)), is(equalTo("value")));
		assertThat(TestCacheLoader.NULL_VALUE.wasCalled(), is(false));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void cacheGetWithValueLoaderUsesValueLoader() throws Exception {
		GemfireCache cache = createCache();

		TestCacheLoader<String> cacheLoader = new TestCacheLoader<String>("test");

		assertThat(cache.get("key", cacheLoader), is(equalTo("test")));
		assertThat(cacheLoader.wasCalled(), is(true));
		assertThat(((Region<Object, String>) cache.getNativeCache()).get("key"), is(equalTo("test")));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void cacheGetWithValueLoaderUsesValueLoaderReturningNull() throws Exception {
		GemfireCache cache = createCache();

		assertThat(cache.get("key", TestCacheLoader.NULL_VALUE), is(nullValue()));
		assertThat(TestCacheLoader.NULL_VALUE.wasCalled(), is(true));
		assertThat(cache.getNativeCache().containsKey("key"), is(false));
	}

	@Test
	@SuppressWarnings("all")
	public void cacheGetWithValueLoaderUsesValueLoaderThrowingException() throws Exception {
		GemfireCache cache = createCache();

		TestCacheLoader<Exception> exceptionThrowingCacheLoader = new TestCacheLoader<Exception>(
			new IllegalStateException("test"));

		expectedException.expect(RuntimeException.class);
		expectedException.expectCause(is(nullValue(IllegalStateException.class)));
		expectedException.expectMessage(String.format("Failed to load value for key [key] using valueLoader [%1$s]",
			exceptionThrowingCacheLoader.getClass().getName()));

		cache.get("key", exceptionThrowingCacheLoader);
	}

	@Test
	public void cacheGetWithValueLoaderIsThreadSafe() throws Throwable {
		TestFramework.runOnce(new CacheGetWithValueLoaderIsThreadSafe());
	}

	@SuppressWarnings("unused")
	protected class CacheGetWithValueLoaderIsThreadSafe extends MultithreadedTestCase {

		private GemfireCache cache;

		private TestCacheLoader<String> cacheLoader;

		@Override
		public void initialize(){
			super.initialize();

			cache = createCacheHandlesCheckedException();

			cacheLoader = new TestCacheLoader<String>("test") {
				@Override public String call() throws Exception {
					waitForTick(2);
					return super.call();
				}
			};
		}

		<T extends Cache> T createCacheHandlesCheckedException() {
			try {
				return createCache();
			}
			catch (Exception e) {
				throw new RuntimeException("failed to create Cache", e);
			}
		}

		public void thread1() {
			assertTick(0);

			Thread.currentThread().setName("Cache Loader Thread");

			String value = cache.get("key", cacheLoader);

			assertTick(2);
			assertThat(value, is(equalTo("test")));
			assertThat(cacheLoader.wasCalled(), is(true));
		}

		public void thread2() {
			waitForTick(1);

			Thread.currentThread().setName("Cache Reader Thread");

			TestCacheLoader<String> illegalCacheLoader = new TestCacheLoader<String>("illegal");

			String value = cache.get("key", illegalCacheLoader);

			assertTick(2);
			assertThat(value, is(equalTo("test")));
			assertThat(illegalCacheLoader.wasCalled(), is(false));
		}
	}

	protected static class TestCacheLoader<T> implements Callable<T> {

		protected static final TestCacheLoader<Object> NULL_VALUE = new TestCacheLoader<Object>();

		private volatile boolean called;

		private final T value;

		public TestCacheLoader() {
			this(null);
		}

		public TestCacheLoader(T value) {
			this.value = value;
		}

		protected boolean wasCalled() {
			boolean called = this.called;
			this.called = false;
			return called;
		}

		@Override
		public T call() throws Exception {
			called = true;

			if (value instanceof Exception) {
				throw (Exception) value;
			}

			return value;
		}
	}

}
