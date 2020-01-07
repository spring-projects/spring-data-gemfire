/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire.cache;

import java.util.concurrent.Callable;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;

import org.springframework.cache.Cache;
import org.springframework.cache.support.SimpleValueWrapper;
import org.springframework.util.Assert;

/**
 * Spring Framework {@link Cache} implementation backed by a Pivotal GemFire {@link Region}.
 *
 * @author Costin Leau
 * @author John Blum
 * @author Oliver Gierke
 * @see org.springframework.cache.Cache
 * @see org.apache.geode.cache.Region
 */
public class GemfireCache implements Cache {

	private final Region region;

	/**
	 * Wraps a Pivotal GemFire {@link Region} in an instance of {@link GemfireCache} to adapt the Pivotal GemFire {@link Region}
	 * to function as a Spring {@link Cache} in Spring's caching infrastructure.
	 *
	 * @param region Pivotal GemFire {@link Region} to wrap.
	 * @return an instance of {@link GemfireCache} backed by the provided Pivotal GemFire {@link Region}.
	 * @see org.apache.geode.cache.Region
	 * @see org.springframework.cache.Cache
	 * @see #GemfireCache(Region)
	 */
	public static GemfireCache wrap(Region<?, ?> region) {
		return new GemfireCache(region);
	}

	/**
	 * Constructs an instance of {@link GemFireCache} initialized with the given Pivotal GemFire {@link Region}.
	 * The {@link Region} will function as the backing store and implementation for
	 * the Spring {@link Cache} interface.
	 *
	 * @param region Pivotal GemFire {@link Region} backing the Spring {@link Cache}.
	 * @throws IllegalArgumentException if {@link Region} is null.
	 */
	public GemfireCache(Region<?, ?> region) {
		Assert.notNull(region, "Region must not be null");
		this.region = region;
	}

	/**
	 * Returns the Pivotal GemFire {@link Region} used as the implementation for this Spring {@link Cache}.
	 *
	 * @return the Pivotal GemFire {@link Region} used as the implementation for this Spring {@link Cache}.
	 * @see org.apache.geode.cache.Region
	 */
	public Region getNativeCache() {
		return this.region;
	}

	/**
	 * Returns the name of this Spring {@link Cache}.
	 *
	 * @return the name of this Spring {@link Cache}.
	 * @see org.apache.geode.cache.Region#getName()
	 */
	public String getName() {
		return getNativeCache().getName();
	}

	/**
	 * Clears the entire contents of this Spring {@link Cache}.
	 *
	 * @see org.apache.geode.cache.Region#clear()
	 */
	public void clear() {
		getNativeCache().clear();
	}

	/**
	 * Evicts (destroys) the entry (key/value) mapped to the given key from this Spring {@link Cache}.
	 *
	 * @param key key used to identify the cache entry to evict.
	 * @see org.apache.geode.cache.Region#destroy(Object)
	 */
	public void evict(Object key) {
		getNativeCache().remove(key);
	}

	/**
	 * Returns the cache value for the given key wrapped in an instance of
	 * {@link org.springframework.cache.Cache.ValueWrapper}.
	 *
	 * @param key key identifying the the value to retrieve from the cache.
	 * @return the value cached with the given key.
	 * @see org.springframework.cache.Cache.ValueWrapper
	 * @see org.apache.geode.cache.Region#get(Object)
	 */
	public ValueWrapper get(Object key) {
		Object value = getNativeCache().get(key);

		return (value != null ? new SimpleValueWrapper(value) : null);
	}

	/**
	 * Returns the cache value for the given key cast to the specified {@link Class} type.
	 *
	 * @param <T> desired {@link Class} type of the cache value.
	 * @param key key identifying the the value to retrieve from the cache.
	 * @param type desired {@link Class} type of the value.
	 * @return the cache value for the given key cast to the specified {@link Class} type.
	 * @throws IllegalStateException if the value is not null and not an instance of the desired type.
	 * @see org.apache.geode.cache.Region#get(Object)
	 */
	@SuppressWarnings("unchecked")
	public <T> T get(Object key, Class<T> type) {
		Object value = getNativeCache().get(key);

		if (value != null && type != null && !type.isInstance(value)) {
			throw new IllegalStateException(String.format(
				"Cached value [%1$s] is not an instance of type [%2$s]",
					value, type.getName()));
		}

		return (T) value;
	}

	/**
	 * Returns the cache value for given key.  If the value is {@literal null}, then the provided
	 * {@link Callable} {@code valueLoader} will be called to obtain a value and add the entry
	 * to this cache.
	 *
	 * @param <T> {@link Class} type of the value.
	 * @param key key identifying the the value to retrieve from the cache.
	 * @param valueLoader {@link Callable} object used to load a value if the entry identified by the key
	 * does not already have value.
	 * @return the cache value of the given key or a value obtained by calling the {@link Callable} object
	 * if the value for key is {@literal null}.
	 * @throws org.springframework.cache.Cache.ValueRetrievalException if an error occurs while trying to
	 * load a value for given key using the {@link Callable}.
	 * @see #get(Object, Class)
	 */
	@SuppressWarnings("unchecked")
	public <T> T get(Object key, Callable<T> valueLoader) {
		T value = (T) get(key, Object.class);

		if (value == null) {
			synchronized (getNativeCache()) {
				value = (T) get(key, Object.class);

				if (value == null) {
					try {
						value = valueLoader.call();
						put(key, value);
					}
					catch (Exception e) {
						throw new ValueRetrievalException(key, valueLoader, e);
					}
				}
			}
		}

		return value;
	}

	/**
	 * Stores the given value in the cache referenced by the given key.  This operation will only store the value
	 * if the value is not {@literal null}.
	 *
	 * @param key key used to reference the value in the cache.
	 * @param value value to store in the cache referenced by the key.
	 * @see org.apache.geode.cache.Region#put(Object, Object)
	 */
	@SuppressWarnings("unchecked")
	public void put(Object key, Object value) {
		if (value != null) {
			getNativeCache().put(key, value);
		}
	}

	/**
	 * Implementation of {@link Cache#putIfAbsent(Object, Object)} satisfying the extension of
	 * the {@link Cache} interface in Spring 4.1. Don't add the {@link Override} annotation
	 * otherwise this will break the compilation on 4.0.
	 *
	 * @return the existing value if the given key is already mapped to a value.
	 * @see org.springframework.cache.Cache#putIfAbsent(java.lang.Object, java.lang.Object)
	 * @see org.apache.geode.cache.Region#putIfAbsent(Object, Object)
	 */
	@SuppressWarnings("unchecked")
	public ValueWrapper putIfAbsent(Object key, Object value) {
		Object existingValue = getNativeCache().putIfAbsent(key, value);

		return (existingValue != null ? new SimpleValueWrapper(existingValue) : null);
	}
}
