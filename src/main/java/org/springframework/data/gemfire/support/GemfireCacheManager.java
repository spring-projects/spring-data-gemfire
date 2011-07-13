/*
 * Copyright 2010-2011 the original author or authors.
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

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.support.AbstractCacheManager;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Region;

/**
 * Spring Framework {@link CacheManager} backed by a Gemfire {@link com.gemstone.gemfire.cache.Cache}. Automatically
 * discovers the created caches (or {@link Region}s in Gemfire terminology).
 * 
 * @author Costin Leau
 */
public class GemfireCacheManager extends AbstractCacheManager {

	private com.gemstone.gemfire.cache.Cache gemfireCache;

	@Override
	protected Collection<Cache> loadCaches() {
		Assert.notNull(gemfireCache, "a backing GemFire cache is required");
		Assert.isTrue(!gemfireCache.isClosed(), "the GemFire cache is closed; an open instance is required");

		Set<Region<?, ?>> regions = gemfireCache.rootRegions();
		Collection<Cache> caches = new LinkedHashSet<Cache>(regions.size());
		
		for (Region<?, ?> region : regions) {
			caches.add(new GemfireCache(region));
		}

		return caches;
	}

	@Override
	public Cache getCache(String name) {
		Cache cache = super.getCache(name);

		if (cache == null) {
			// check the gemfire cache again
			// in case the cache was added at runtime

			Region<?, ?> reg = gemfireCache.getRegion(name);
			if (reg != null) {
				cache = new GemfireCache(reg);
				getCacheMap().put(name, cache);
			}
		}

		return cache;
	}

	/**
	 * Sets the GemFire Cache backing this {@link CacheManager}.
	 * 
	 * @param gemfireCache
	 */
	public void setCache(com.gemstone.gemfire.cache.Cache gemfireCache) {
		this.gemfireCache = gemfireCache;
	}
}