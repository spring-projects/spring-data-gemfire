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

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;

import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.support.AbstractCacheManager;
import org.springframework.util.Assert;

/**
 * Core Spring Framework {@link CacheManager} implementation backed by a Pivotal GemFire cache instance
 * (either a client or peer cache).
 *
 * Automatically discovers available caches (or Pivotal GemFire {@link Region Regions}) when a cache for a given name
 * is missing and dynamic cache lookup/creation is enabled.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.cache.Cache
 * @see org.springframework.cache.CacheManager
 * @see org.springframework.cache.support.AbstractCacheManager
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 */
@SuppressWarnings("unused")
public class GemfireCacheManager extends AbstractCacheManager {

	private final AtomicBoolean dynamic = new AtomicBoolean(true);

	private org.apache.geode.cache.GemFireCache gemfireCache;

	private Set<Region<?, ?>> regions;

	private Set<String> cacheNames;

	@SuppressWarnings("all")
	<T extends GemFireCache> T assertGemFireCacheAvailable(T gemfireCache) {

		Assert.state(gemfireCache != null, "A cache instance is required");
		Assert.state(!gemfireCache.isClosed(), String.format("The cache [%s] has been closed",
			gemfireCache.getName()));

		return gemfireCache;
	}

	@SuppressWarnings("all")
	Region<?, ?> assertGemFireRegionAvailable(Region<?, ?> region, String cacheName) {
		Assert.state(region != null, String.format("No Region for cache name [%s] was found", cacheName));
		Assert.state(!region.isDestroyed(), String.format("Region [%s] has been destroyed", cacheName));

		return region;
	}

	/**
	 * Loads all configured Pivotal GemFire {@link Region Regions} that will be used by this {@link CacheManager}.
	 *
	 * Any Pivotal GemFire {@link Region Regions} configured with the {@link #regions} property will take precedence over
	 * any configured {@link #cacheNames}.  If no Pivotal GemFire {@link Region Regions} were configured, then any
	 * {@link #cacheNames} that were specified will be used to lookup existing Pivotal GemFire {@link Region Regions}
	 * to function as Spring {@link Cache Caches}in Spring's caching infrastructure.
	 *
	 * However, if neither {@link #regions} nor {@link #cacheNames} were specified, then all defined GemFire
	 * {@link Region Regions} declared in the Spring application context, as determined by
	 * {@link GemFireCache#rootRegions()}, will be used as Spring {@link Cache Caches}, and this {@link CacheManager}
	 * will allow any dynamically created Pivotal GemFire {@link Region Regions} at runtime to be found and used as a
	 * Spring {@link Cache} as well.
	 *
	 * @return a {@link Collection} of Pivotal GemFire {@link Region Regions} used by this {@link CacheManager}
	 * to function as {@link Cache Caches} in Spring's caching infrastructure.
	 * @throws IllegalStateException if a Pivotal GemFire cache instance was not provided, the provided Pivotal GemFire cache instance
	 * has been closed, no Pivotal GemFire {@link Region} could be found for a given cache name, or the Pivotal GemFire {@link Region}
	 * for the given cache name has been destroyed.
	 * @see org.springframework.cache.Cache
	 */
	@Override
	protected Collection<Cache> loadCaches() {
		Set<Region<?, ?>> regions = resolveRegions(this.gemfireCache, this.regions, this.cacheNames);

		Collection<Cache> caches = new HashSet<Cache>(regions.size());

		for (Region<?, ?> region : regions) {
			caches.add(newGemfireCache(region));
		}

		return caches;
	}

	Set<Region<?, ?>> resolveRegions(GemFireCache gemfireCache, Set<Region<?, ?>> regions, Set<String> cacheNames) {

		if (isSet(regions)) {
			dynamic.set(false);
			return regions;
		}
		else if (isSet(cacheNames)) {
			dynamic.set(false);

			regions = new HashSet<Region<?, ?>>(cacheNames.size());

			for (String cacheName : cacheNames) {
				regions.add(regionFor(gemfireCache, cacheName));
			}

			return regions;
		}
		else {
			return assertGemFireCacheAvailable(gemfireCache).rootRegions();
		}
	}

	boolean isSet(Iterable<?> collection) {
		return (collection != null && collection.iterator().hasNext());
	}

	/**
	 * Constructs a new instance of {@link GemfireCache} initialized with the given Pivotal GemFire {@link Region}.
	 *
	 * @param region Pivotal GemFire {@link Region} to wrap (adapt).
	 * @return an instance of {@link GemfireCache} initialized with the given Pivotal GemFire {@link Region}.
	 * @see GemfireCache
	 * @see org.apache.geode.cache.Region
	 */
	protected GemfireCache newGemfireCache(Region<?, ?> region) {
		return GemfireCache.wrap(region);
	}

	Region<?, ?> regionFor(GemFireCache gemfireCache, String cacheName) {
		return assertGemFireRegionAvailable(assertGemFireCacheAvailable(gemfireCache).getRegion(cacheName), cacheName);
	}

	/**
	 * Returns a missing Spring {@link Cache} for the given {@code name}.
	 *
	 * To return a missing Spring {@link Cache} for the given {@code name}, dynamic cache lookup/creation must be
	 * enabled, which means that either the {@link #cacheNames} or {@link #regions} properties must not be set.
	 * If either property was specified then dynamic Spring {@link Cache} lookup/creation will be disabled and this
	 * overridden {@link AbstractCacheManager#getMissingCache(String)} method will return {@literal null}.
	 *
	 * @param name name of the missing Spring {@link Cache} to lookup (and potentially create).
	 * @return a Spring {@link Cache} instance for the given {@code name} or {@literal null} if the {@link Cache}
	 * cannot be found (or possibly created).
	 * @see org.springframework.cache.support.AbstractCacheManager#getMissingCache(String)
	 * @see org.springframework.cache.Cache
	 */
	@Override
	protected Cache getMissingCache(String name) {

		Cache cache = super.getMissingCache(name);

		return cache != null ? cache : (isDynamic() ? newGemfireCache(regionFor(this.gemfireCache, name)) : null);
	}

	/**
	 * Determines whether this {@link CacheManager} allows the dynamic creation of a {@link Cache} at runtime.
	 *
	 * @return a boolean value indicating whether dynamic {@link Cache} creation is enabled.
	 */
	protected boolean isDynamic() {
		return dynamic.get();
	}

	/**
	 * Sets the Pivotal GemFire cache instance backing this {@link CacheManager}.
	 *
	 * When set, if neither {@link Region Regions} nor {@code cacheNames} were specified, then this {@link CacheManager}
	 * is capable of creating Spring {@link Cache Caches} backed by existing Pivotal GemFire {@link Region Regions} used by
	 * the application at runtime.  However, in order to dynamically create Spring {@link Cache Caches} a reference to
	 * an open Pivotal GemFire cache instance must be set.
	 *
	 * @param gemfireCache the Pivotal GemFire cache instance used by this {@link CacheManager}
	 * to manage Spring {@link Cache Caches}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	public void setCache(org.apache.geode.cache.GemFireCache gemfireCache) {
		this.gemfireCache = gemfireCache;
	}

	/**
	 * Returns the {@link GemFireCache} instance backing this {@link CacheManager}.
	 *
	 * @return the {@link GemFireCache} instance backing this {@link CacheManager}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	protected org.apache.geode.cache.GemFireCache getCache() {
		return this.gemfireCache;
	}

	/**
	 * Sets the names of all Spring {@link Cache Caches} that will be used in the application.
	 *
	 * When set, this disables the dynamic capability of this {@link CacheManager} to create Spring {@link Cache Caches}
	 * at runtime by dynamically looking up existing {@link Region Regions} from the Pivotal GemFire cache instance.
	 *
	 * @param cacheNames {@link Set} of cache names that will be used in the application.
	 * @see java.util.Set
	 */
	public void setCacheNames(Set<String> cacheNames) {
		this.cacheNames = cacheNames;
	}

	/**
	 * Explicitly sets the Pivotal GemFire {@link Region Regions} to be used as Spring {@link Cache Caches}
	 * in the application.
	 *
	 * When set, this disables the dynamic capability of this {@link CacheManager} to create Spring {@link Cache Caches}
	 * at runtime by dynamically looking up existing {@link Region Regions} from the Pivotal GemFire cache instance.
	 *
	 * @param regions {@link Set} of Pivotal GemFire {@link Region Regions} used by this {@link CacheManager}
	 * as Spring {@link Cache Caches}.
	 * @see org.apache.geode.cache.Region
	 */
	public void setRegions(Set<Region<?, ?>> regions) {
		this.regions = regions;
	}

	/**
	 * Returns the set of Pivotal GemFire {@link Region Regions} used explicitly as Spring {@link Cache Caches}
	 * in Spring's caching infrastructure.
	 *
	 * @return the set of Pivotal GemFire {@link Region Regions} functioning as Spring {@link Cache Caches}
	 * in Spring's caching infrastructure
	 * @see org.apache.geode.cache.Region
	 */
	protected Set<Region<?, ?>> getRegions() {
		return this.regions;
	}
}
