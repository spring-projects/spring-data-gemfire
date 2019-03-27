/*
 * Copyright 2010-2014 the original author or authors.
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

import java.util.concurrent.Callable;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;

import org.springframework.cache.Cache;
import org.springframework.cache.support.SimpleValueWrapper;
import org.springframework.util.ObjectUtils;

/**
 * Spring Framework {@link Cache} implementation backed by a GemFire {@link Region}.
 *
 * Supports GemFire 6.5 or higher.
 *
 * @author Costin Leau
 * @author John Blum
 * @author Oliver Gierke
 * @see org.springframework.cache.Cache
 * @see com.gemstone.gemfire.cache.Region
 */
public class GemfireCache implements Cache {

	@SuppressWarnings({ "rawtypes" })
	private final Region region;

	/**
	 * Creates a {@link GemFireCache} instance.
	 *
	 * @param region backing GemFire region
	 */
	public GemfireCache(final Region<?, ?> region) {
		this.region = region;
	}

	public String getName() {
		return region.getName();
	}

	public Region<?, ?> getNativeCache() {
		return region;
	}

	public void clear() {
		region.clear();
	}

	/**
	 * Evicts (destroys) the entry (key/value) mapped to the given key from this Spring {@link Cache}.
	 *
	 * @param key key used to identify the cache entry to evict.
	 * @see com.gemstone.gemfire.cache.Region#destroy(Object)
	 */
	public void evict(Object key) {
		getNativeCache().remove(key);
	}

	public ValueWrapper get(final Object key) {
		Object value = region.get(key);

		return (value == null ? null : new SimpleValueWrapper(value));
	}

	@SuppressWarnings("unchecked")
	public <T> T get(final Object key, final Class<T> type) {
		Object value = region.get(key);

		if (value != null && type != null && !type.isInstance(value)) {
			throw new IllegalStateException(String.format("Cached value is not of required type [%1$s]: %2$s",
				type.getName(), value));
		}

		return (T) value;
	}

	@SuppressWarnings("unchecked")
	public <T> T get(final Object key, final Callable<T> valueLoader) {
		T value = (T) get(key, Object.class);

		if (value == null) {
			synchronized (region) {
				value = (T) get(key, Object.class);

				if (value == null) {
					try {
						value = valueLoader.call();
						put(key, value);
					}
					catch (Exception e) {
						throw new RuntimeException(String.format(
							"Failed to load value for key [%1$s] using valueLoader [%2$s]", key,
								ObjectUtils.nullSafeClassName(valueLoader)));
						//TODO throw ValueRetrievalException when SDG is based on Spring Framework 4.3
						//throw new ValueRetrievalException(key, valueLoader, e);
					}
				}
			}
		}

		return value;
	}

	@SuppressWarnings("unchecked")
	public void put(final Object key, final Object value) {
		if (value != null) {
			region.put(key, value);
		}
	}

	/**
	 * Implementation to satisfy extension of the {@link Cache} interface in Spring 4.1. Don't add the {@link Override}
	 * annotation as this will break the compilation on 4.0.
	 *
	 * @see org.springframework.cache.Cache#putIfAbsent(java.lang.Object, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public ValueWrapper putIfAbsent(Object key, Object value) {
		Object existingValue = region.putIfAbsent(key, value);

		return (existingValue == null ? null : new SimpleValueWrapper(existingValue));
	}
}
