/*
 * Copyright 2010-2014 the original author or authors.
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

import org.springframework.cache.Cache;
import org.springframework.cache.support.SimpleValueWrapper;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;

/**
 * Spring Framework {@link Cache} implementation backed by a GemFire {@link Region}.
 *
 * Supports GemFire 6.5 or higher.
 *
 * @author Costin Leau
 * @author John Blum
 * @author Oliver Gierke
 */
public class GemfireCache implements Cache {

	@SuppressWarnings({ "rawtypes" })
	private final Region region;

	/**
	 * Creates a {@link GemFireCache} instance.
	 * 
	 * @param region backing GemFire region
	 */
	public GemfireCache(Region<?, ?> region) {
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

	public void evict(Object key) {
		region.destroy(key);
	}

	public ValueWrapper get(Object key) {
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
