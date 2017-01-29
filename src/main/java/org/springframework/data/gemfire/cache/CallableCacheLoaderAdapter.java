/*
 * Copyright 2016-2018 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.cache;

import java.util.concurrent.Callable;

import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.TimeoutException;
import org.springframework.util.Assert;

/**
 * The {@link CallableCacheLoaderAdapter} class is a {@link Callable} and GemFire {@link CacheLoader} implementation
 * that adapts the {@link Callable} interface into an instance of the {@link CacheLoader} interface.  This class is
 * useful in situations where GemFire developers have several {@link CacheLoader} implementations that they wish to
 * use with Spring's Cache Abstraction.
 *
 * @author John Blum
 * @see java.util.concurrent.Callable
 * @see org.apache.geode.cache.CacheLoader
 * @see org.apache.geode.cache.LoaderHelper
 * @see org.apache.geode.cache.Region
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public class CallableCacheLoaderAdapter<K, V> implements Callable<V>, CacheLoader<K, V> {

	private final K key;

	private final CacheLoader<K, V> cacheLoader;

	private final Object argument;

	private final Region<K, V> region;

	/**
	 * Constructs an instance of the CallableCacheLoaderAdapter that delegates to the given {@link CacheLoader}.
	 *
	 * @param delegate the {@link CacheLoader} delegated to by this adapter.
	 * @see #CallableCacheLoaderAdapter(CacheLoader, Object, Region, Object)
	 * @see org.apache.geode.cache.CacheLoader
	 */
	public CallableCacheLoaderAdapter(CacheLoader<K, V> delegate) {
		this(delegate, null, null, null);
	}

	/**
	 * Constructs an instance of the CallableCacheLoaderAdapter that delegates to the given {@link CacheLoader}
	 * and is initialized with the given key for which the value will be loaded along with the {@link Region}
	 * in which the entry (key/value) belongs.
	 *
	 * @param delegate the {@link CacheLoader} delegated to by this adapter.
	 * @param key the key for which the value will be loaded.
	 * @param region the {@link Region} in which the entry (key/value) belongs.
	 * @see #CallableCacheLoaderAdapter(CacheLoader, Object, Region, Object)
	 * @see org.apache.geode.cache.CacheLoader
	 * @see org.apache.geode.cache.Region
	 */
	public CallableCacheLoaderAdapter(CacheLoader<K, V> delegate, K key, Region<K, V> region) {
		this(delegate, key, region, null);
	}

	/**
	 * Constructs an instance of the CallableCacheLoaderAdapter that delegates to the given {@link CacheLoader}
	 * and is initialized with the given key for which the value will be loaded along with the {@link Region}
	 * in which the entry (key/value) belongs.  Additionally, an argument may be specified for use with the
	 * {@link CacheLoader} delegate.
	 *
	 * @param delegate the {@link CacheLoader} delegated to by this adapter.
	 * @param key the key for which the value will be loaded.
	 * @param region the {@link Region} in which the entry (key/value) belongs.
	 * @param argument the Object argument used with the {@link CacheLoader} delegate.
	 * @see #CallableCacheLoaderAdapter(CacheLoader, Object, Region, Object)
	 * @see org.apache.geode.cache.CacheLoader
	 * @see org.apache.geode.cache.Region
	 */
	public CallableCacheLoaderAdapter(CacheLoader<K, V> delegate, K key, Region<K, V> region, Object argument) {
		Assert.notNull(delegate, "CacheLoader must not be null");
		this.cacheLoader = delegate;
		this.argument = argument;
		this.key = key;
		this.region = region;
	}

	/**
	 * Gets the argument used by this {@link CacheLoader} to load the value for the specified key.
	 *
	 * @return an Object argument used by this {@link CacheLoader} when loading the value for the specified key.
	 */
	protected Object getArgument() {
		return argument;
	}

	/**
	 * The {@link CacheLoader} delegate used to actually load the cache value for the specified key.
	 *
	 * @return a reference to the actual {@link CacheLoader} used when loading the cache value for the specified key.
	 * @see org.apache.geode.cache.CacheLoader
	 */
	protected CacheLoader<K, V> getCacheLoader() {
		return cacheLoader;
	}

	/**
	 * The specified key for which a value will be loaded by this {@link CacheLoader}.
	 *
	 * @return the specified key for which the value will be loaded.
	 */
	protected K getKey() {
		return key;
	}

	/**
	 * Returns the Region to which the entry (key/value) belongs.
	 *
	 * @return the Region to which the entry belongs.
	 * @see org.apache.geode.cache.Region
	 */
	protected Region<K, V> getRegion() {
		return region;
	}

	/**
	 * Invoked to load a cache value for the specified key.  Delegates to {@link #load(LoaderHelper)}.
	 *
	 * @return the loaded cache value for the specified key.
	 * @throws java.lang.IllegalStateException if the {@link Region} or key references are null.
	 * @throws java.lang.Exception if the load operation fails.
	 * @see #load(LoaderHelper)
	 */
	public final V call() throws Exception {
		Assert.state(getKey() != null, "The key for which the value is loaded for cannot be null");
		Assert.state(getRegion() != null, "The Region to load cannot be null");

		return load(new LoaderHelper<K, V>() {
			public V netSearch(final boolean doNetLoad) throws CacheLoaderException, TimeoutException {
				throw new UnsupportedOperationException("not implemented");
			}

			public K getKey() {
				return CallableCacheLoaderAdapter.this.getKey();
			}

			public Region<K, V> getRegion() {
				return CallableCacheLoaderAdapter.this.getRegion();
			}

			public Object getArgument() {
				return CallableCacheLoaderAdapter.this.getArgument();
			}
		});
	}

	/**
	 * Closes any resources used by this {@link CacheLoader}.  Delegates to the underlying {@link CacheLoader}.
	 *
	 * @see #getCacheLoader()
	 */
	public void close() {
		getCacheLoader().close();
	}

	/**
	 * Loads a value for the specified cache (i.e. {@link Region}) and key with the help of the {@link LoaderHelper}.
	 * Delegates to the underlying {@link CacheLoader}.
	 *
	 * @param loaderHelper a {@link LoaderHelper} object passed in from cache service providing access to the key,
	 * {@link Region}, argument, and <code>netSearch</code>.
	 * @return the value supplied for the specified key, or null if no value can be supplied.  A local loader will
	 * always be invoked if one exists. Otherwise one remote loader is invoked. Returning <code>null</code> causes
	 * {@link Region#get(Object, Object)} to return <code>null</code>.
	 * @throws CacheLoaderException if an error occurs during the load operation. This exception, or any other
	 * Exception thrown by this method will be propagated back to the application from the
	 * {@link Region#get(Object)} method.
	 * @see org.apache.geode.cache.CacheLoader#load(LoaderHelper)
	 * @see org.apache.geode.cache.LoaderHelper
	 * @see #getCacheLoader()
	 */
	public V load(LoaderHelper<K, V> loaderHelper) throws CacheLoaderException {
		return getCacheLoader().load(loaderHelper);
	}
}
