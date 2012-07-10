/*
 * Copyright 2010-2012 the original author or authors.
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
 * Spring Framework {@link Cache} implementation using a GemFire {@link Region} underneath.
 * Supports Gemfire 6.5 or higher.
 * 
 * @author Costin Leau
 */
public class GemfireCache implements Cache {

	@SuppressWarnings("unchecked")
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

	@SuppressWarnings("unchecked")
	public ValueWrapper get(Object key) {
		Object value = region.get(key);

		return (value == null ? null : new SimpleValueWrapper(value));
	}

	public void put(Object key, Object value) {
		region.put(key, value);
	}
}