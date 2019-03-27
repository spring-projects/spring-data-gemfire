/*
 * Copyright 2010-2013 the original author or authors.
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
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.cache.Cache
 * @see org.springframework.cache.CacheManager
 * @see org.springframework.cache.support.AbstractCacheManager
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.Region
 */
@SuppressWarnings("unused")
public class GemfireCacheManager extends AbstractCacheManager {

	private com.gemstone.gemfire.cache.Cache gemfireCache;

	private Set<Region<?,?>> regions;

	/**
	 * Loads the GemFire Cache Regions managed by this CacheManager.
	 *
	 * @return a Collection of GemFire Cache Regions (caches) to be managed by this SDG CacheManager.
	 * @see org.springframework.cache.Cache
	 * @see com.gemstone.gemfire.cache.Cache#rootRegions()
	 */
	@Override
	protected Collection<Cache> loadCaches() {
		if (regions == null) {
			Assert.state(gemfireCache != null, "A backing GemFire Cache is required.");
			Assert.state(!gemfireCache.isClosed(), "The GemFire Cache is closed; an open instance is required.");

			regions = gemfireCache.rootRegions();
		}  

		Collection<Cache> caches = new LinkedHashSet<Cache>(regions.size());

		for (Region<?,?> region: this.regions) {
			caches.add(new GemfireCache(region));
		}

		return caches;
	}

	/**
	 * Gets a Cache (GemFire Cache Region) by name.
	 *
	 * @param name a String indicating the name of the Cache to get.
	 * @return a Cache with the given name.
	 * @see org.springframework.cache.Cache
	 */
	@Override
	public Cache getCache(String name) {
		Cache cache = super.getCache(name);

		if (cache == null) {
			// check the GemFire Cache again in case the Cache (Region) was added at runtime
			Region<?, ?> region = gemfireCache.getRegion(name);

			if (region != null) {
				cache = new GemfireCache(region);
				addCache(cache);
			}
		}

		return cache;
	}

	/**
	 * Sets the GemFire Cache backing this {@link CacheManager}.
	 * 
	 * @param gemfireCache the GemFire Peer Cache instance.
	 * @see com.gemstone.gemfire.cache.Cache
	 */
	public void setCache(com.gemstone.gemfire.cache.Cache gemfireCache) {
		this.gemfireCache = gemfireCache;
	}
	
	/**
	 * Sets the Regions to use (alternative to injecting the GemFire Cache).
	 *
	 * @param regions the Set of Regions (caches) managed by this CacheManager.
	 * @see com.gemstone.gemfire.cache.Region
	 */
	public void setRegions(Set<Region<?,?>> regions) {
		this.regions = regions;
	}

}
