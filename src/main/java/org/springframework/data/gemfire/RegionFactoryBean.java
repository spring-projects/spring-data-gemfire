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
import org.springframework.beans.factory.DisposableBean;
import org.springframework.core.io.Resource;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheWriter;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.Scope;

/**
 * FactoryBean for creating generic GemFire {@link Region}s. Will try to first locate the region (by name)
 * and, in case none if found, proceed to creating one using the given settings.
 * 
 * @author Costin Leau
 */
public class RegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V> implements DisposableBean {

	protected final Log log = LogFactory.getLog(getClass());

	private boolean destroy = false;
	private boolean close = true;
	private Resource snapshot;

	private CacheListener<K, V> cacheListeners[];
	private CacheLoader<K, V> cacheLoader;
	private CacheWriter<K, V> cacheWriter;
	private RegionAttributes<K, V> attributes;
	private Scope scope;
	private DataPolicy dataPolicy;


	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();

		postProcess(region);
	}


	@Override
	protected Region<K, V> lookupFallback(Cache cache, String regionName) throws Exception {
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

		if (dataPolicy != null) {
			attrFactory.setDataPolicy(dataPolicy);
		}

		if (scope != null) {
			attrFactory.setScope(scope);
		}

		postProcess(attrFactory);

		Region<K, V> reg = cache.createRegion(regionName, attrFactory.create());
		log.info("Created new cache region [" + regionName + "]");
		if (snapshot != null) {
			reg.loadSnapshot(snapshot.getInputStream());
		}

		return reg;
	}

	/**
	 * Post-process the attribute factory object used for configuring the region of this factory bean during the initialization process.
	 * The object is already initialized and configured by the factory bean before this method
	 * is invoked.
	 * 
	 * @param attrFactory attribute factory
	 */
	protected void postProcess(AttributesFactory<K, V> attrFactory) {
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
		if (region != null) {
			if (close) {
				if (!region.getCache().isClosed()) {
					try {
						region.close();
					} catch (CacheClosedException cce) {
						// nothing to see folks, move on.
					}
				}
			}
			else if (destroy) {
				region.destroyRegion();
			}
		}
		region = null;
	}

	/**
	 * Indicates whether the region referred by this factory bean,
	 * will be destroyed on shutdown (default false).
	 * Note: destroy and close are mutually exclusive. Enabling one will automatically disable the other.
	 * 
	 * @param destroy whether or not to destroy the region
	 * 
	 * @see #setClose(boolean)
	 */
	public void setDestroy(boolean destroy) {
		this.destroy = destroy;

		if (destroy) {
			close = false;
		}
	}

	/**
	 * Indicates whether the region referred by this factory bean,
	 * will be closed on shutdown (default true).
	 * Note: destroy and close are mutually exclusive. Enabling one will automatically disable the other.
	 * 
	 * @param close whether to close or not the region
	 * @see #setDestroy(boolean)
	 */
	public void setClose(boolean close) {
		this.close = close;
		if (close) {
			destroy = false;
		}
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
	 * Sets the cache writer used for the region used by this factory.
	 * Used only when a new region is created. Overrides the settings
	 * specified through {@link #setAttributes(RegionAttributes)}.
	 * 
	 * @param cacheWriter the cacheWriter to set on a newly created region
	 */
	public void setCacheWriter(CacheWriter<K, V> cacheWriter) {
		this.cacheWriter = cacheWriter;
	}

	/**
	 * Sets the data policy. Used only when a new region is created.
	 * Overrides the settings specified through {@link #setAttributes(RegionAttributes)}.
	 * 
	 * @param dataPolicy the region data policy
	 */
	public void setDataPolicy(DataPolicy dataPolicy) {
		this.dataPolicy = dataPolicy;
	}

	/**
	 * Sets the region scope. Used only when a new region is created.
	 * Overrides the settings specified through {@link #setAttributes(RegionAttributes)}.
	 * 
	 * @see Scope
	 * @param scope the region scope
	 */
	public void setScope(Scope scope) {
		this.scope = scope;
	}

	/**
	 * Sets the region attributes used for the region used by this factory.
	 * Allows maximum control in specifying the region settings.
	 * Used only when a new region is created.
	 * 
	 * @param attributes the attributes to set on a newly created region
	 */
	public void setAttributes(RegionAttributes<K, V> attributes) {
		this.attributes = attributes;
	}
}