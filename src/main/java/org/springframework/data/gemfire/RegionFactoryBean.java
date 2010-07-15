/*
 * Copyright 2010 the original author or authors.
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

package org.springframework.data.gemfire;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.core.io.Resource;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheWriter;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;

/**
 * FactoryBean for creating generic Gemfire {@link Region}s. Will try to first locate the region (by name)
 * and, in case none if found, proceed to creating one using the given settings.
 * 
 * @author Costin Leau
 */
public class RegionFactoryBean<K, V> implements DisposableBean, FactoryBean<Region<K, V>>, InitializingBean,
		BeanNameAware {

	protected final Log log = LogFactory.getLog(getClass());

	private String beanName;
	private Cache cache;
	private String name;
	private boolean destroy = false;
	private Resource snapshot;

	private CacheListener<K, V> cacheListeners[];
	private CacheLoader<K, V> cacheLoader;
	private CacheWriter<K, V> cacheWriter;
	private RegionAttributes<K, V> attributes;

	private Region<K, V> region;


	public void afterPropertiesSet() throws Exception {
		Assert.notNull(cache, "Cache property must be set");
		name = (!StringUtils.hasText(name) ? beanName : name);
		Assert.hasText(name, "Name (or beanName) property must be set");
		
		// first get cache
		region = cache.getRegion(name);
		if (region != null) {
			log.info("Retrieved region [" + name + "] from cache");
		}
		// fall back to cache creation if one is not found
		else {
			if (attributes != null)
				AttributesFactory.validateAttributes(attributes);

			AttributesFactory<K, V> attrFactory = (attributes != null ? new AttributesFactory<K, V>(attributes)
					: new AttributesFactory<K, V>());
			if (!ObjectUtils.isEmpty(cacheListeners)) {
				for (CacheListener<K, V> listener : cacheListeners) {
					attrFactory.addCacheListener(listener);
				}
			}

			if (cacheLoader != null) {
				attrFactory.setCacheLoader(cacheLoader);
			}

			if (cacheWriter != null) {
				attrFactory.setCacheWriter(cacheWriter);
			}

			region = cache.createRegion(name, attrFactory.create());
			log.info("Created new cache region [" + name + "]");
			if (snapshot != null) {
				region.loadSnapshot(snapshot.getInputStream());
			}
		}

		postProcess(region);
	}

	/**
	 * Post-process the region object for this factory bean during the initialization process.
	 * The object is already initialized and configured by the factory bean before this method
	 * is invoked.
	 * 
	 * @param region
	 */
	protected void postProcess(Region<K, V> region) {
		// do nothing
	}

	public void destroy() throws Exception {
		if (region != null && destroy) {
			region.destroyRegion();
		}
		region = null;
	}

	public Region<K, V> getObject() throws Exception {
		return region;
	}

	public Class<?> getObjectType() {
		return (region != null ? region.getClass() : Region.class);
	}

	public boolean isSingleton() {
		return true;
	}

	public void setBeanName(String beanName) {
		this.beanName = beanName;
	}

	/**
	 * Sets the cache used for creating the region.
	 * 
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 * @param cache the cache to set
	 */
	public void setCache(Cache cache) {
		this.cache = cache;
	}

	/**
	 * Sets the name of the cache region. If no cache is found under
	 * the given name, a new one will be created.
	 * If no name is given, the beanName will be used. 
	 * 
	 * @see com.gemstone.gemfire.cache.Region#getFullPath()
	 * @see #setBeanName(String)
	 * 
	 * @param name the region name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Indicates whether the region referred by this factory bean,
	 * will be destroyed on shutdown (default false).
	 * 
	 * @param destroy the destroy to set
	 */
	public void setDestroy(boolean destroy) {
		this.destroy = destroy;
	}

	/**
	 * Sets the snapshots used for loading a newly <i>created</i> region.
	 * That is, the snapshot will be used <i>only</i> when a new region is created - if the region
	 * already exists, no loading will be performed.
	 * 
	 * @see #setName(String)
	 * @param snapshot the snapshot to set
	 */
	public void setSnapshot(Resource snapshot) {
		this.snapshot = snapshot;
	}

	/**
	 * Sets the cache listeners used for the region used by this factory.
	 * Used only when a new region is created.Overrides the settings
	 * specified through {@link #setAttributes(RegionAttributes)}.
	 * 
	 * @param cacheListeners the cacheListeners to set on a newly created region
	 */
	public void setCacheListeners(CacheListener<K, V>[] cacheListeners) {
		this.cacheListeners = cacheListeners;
	}

	/**
	 * Sets the cache loader used for the region used by this factory.
	 * Used only when a new region is created.Overrides the settings
	 * specified through {@link #setAttributes(RegionAttributes)}.
	 * 
	 * @param cacheLoader the cacheLoader to set on a newly created region
	 */
	public void setCacheLoader(CacheLoader<K, V> cacheLoader) {
		this.cacheLoader = cacheLoader;
	}

	/**
	 * Sets the cache loader used for the region used by this factory.
	 * Used only when a new region is created. Overrides the settings
	 * specified through {@link #setAttributes(RegionAttributes)}.
	 * 
	 * @param cacheWriter the cacheWriter to set on a newly created region
	 */
	public void setCacheWriter(CacheWriter<K, V> cacheWriter) {
		this.cacheWriter = cacheWriter;
	}

	/**
	 * Sets the cache loader used for the region used by this factory.
	 * Used only when a new region is created.
	 * 
	 * @param attributes the attributes to set on a newly created region
	 */
	public void setAttributes(RegionAttributes<K, V> attributes) {
		this.attributes = attributes;
	}
}