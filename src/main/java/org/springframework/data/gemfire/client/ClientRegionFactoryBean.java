/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.client;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.DataPolicyConverter;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheWriter;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.internal.cache.GemFireCacheImpl;

/**
 * Client extension for GemFire Regions.
 * <p/>
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
public class ClientRegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V> implements BeanFactoryAware,
		DisposableBean {

	private static final Log log = LogFactory.getLog(ClientRegionFactoryBean.class);

	private boolean close = false;
	private boolean destroy = false;

	private BeanFactory beanFactory;

	private CacheListener<K, V>[] cacheListeners;

	private CacheLoader<K, V> cacheLoader;

	private CacheWriter<K, V> cacheWriter;

	private ClientRegionShortcut shortcut = null;

	private DataPolicy dataPolicy;

	private Interest<K>[] interests;

	private RegionAttributes<K, V> attributes;

	private Resource snapshot;

	private String dataPolicyName;
	private String diskStoreName;
	private String poolName;

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		postProcess(getRegion());
	}

	@Override
	protected Region<K, V> lookupFallback(GemFireCache cache, String regionName) throws Exception {
		Assert.isTrue(cache instanceof ClientCache, String.format("Unable to create regions from %1$s", cache));

		// TODO reference to an internal GemFire class!
		if (cache instanceof GemFireCacheImpl) {
			Assert.isTrue(((GemFireCacheImpl) cache).isClient(), "A client-cache instance is required");
		}

		Assert.isTrue(!(StringUtils.hasText(dataPolicyName) && dataPolicy != null),
			"Only one of 'dataPolicy' or 'dataPolicyName' can be set");

		if (StringUtils.hasText(dataPolicyName)) {
			dataPolicy = new DataPolicyConverter().convert(dataPolicyName);
			Assert.notNull(dataPolicy, "Data policy " + dataPolicyName + " is invalid");
		}

		// first look at shortcut
		ClientRegionShortcut shortcut = this.shortcut;

		if (shortcut == null) {
			if (dataPolicy != null) {
				if (DataPolicy.EMPTY.equals(dataPolicy)) {
					shortcut = ClientRegionShortcut.PROXY;
				}
				else if (DataPolicy.NORMAL.equals(this.dataPolicy)) {
					shortcut = ClientRegionShortcut.CACHING_PROXY;
				}
				else if (DataPolicy.PERSISTENT_REPLICATE.equals(dataPolicy)) {
					shortcut = ClientRegionShortcut.LOCAL_PERSISTENT;
				}
				else {
					shortcut = ClientRegionShortcut.LOCAL;
				}
			}
			else {
				shortcut = ClientRegionShortcut.LOCAL;
			}
		}

		ClientRegionFactory<K, V> factory = ((ClientCache) cache).createClientRegionFactory(shortcut);

		// map the attributes onto the client
		if (attributes != null) {
			factory.setCloningEnabled(attributes.getCloningEnabled());
			factory.setConcurrencyLevel(attributes.getConcurrencyLevel());
			factory.setCustomEntryIdleTimeout(attributes.getCustomEntryIdleTimeout());
			factory.setCustomEntryTimeToLive(attributes.getCustomEntryTimeToLive());
			factory.setDiskStoreName(attributes.getDiskStoreName());
			factory.setDiskSynchronous(attributes.isDiskSynchronous());
			factory.setEntryIdleTimeout(attributes.getEntryIdleTimeout());
			factory.setEntryTimeToLive(attributes.getEntryTimeToLive());
			factory.setEvictionAttributes(attributes.getEvictionAttributes());
			factory.setInitialCapacity(attributes.getInitialCapacity());
			factory.setKeyConstraint(attributes.getKeyConstraint());
			factory.setLoadFactor(attributes.getLoadFactor());
			factory.setPoolName(attributes.getPoolName());
			factory.setRegionIdleTimeout(attributes.getRegionIdleTimeout());
			factory.setRegionTimeToLive(attributes.getRegionTimeToLive());
			factory.setStatisticsEnabled(attributes.getStatisticsEnabled());
			factory.setValueConstraint(attributes.getValueConstraint());
		}

		addCacheListeners(factory);

		if (StringUtils.hasText(poolName)) {
			// try to eagerly initialize the pool name, if defined as a bean
			if (beanFactory.isTypeMatch(poolName, Pool.class)) {
				if (log.isDebugEnabled()) {
					log.debug("Found bean definition for pool '" + poolName + "'. Eagerly initializing it...");
				}
				beanFactory.getBean(poolName, Pool.class);
			}

			factory.setPoolName(poolName);
		}
		else {
			Pool pool = beanFactory.getBean(Pool.class);
			factory.setPoolName(pool.getName());
		}

		if (diskStoreName != null) {
			factory.setDiskStoreName(diskStoreName);
		}

		Region<K, V> clientRegion = factory.create(regionName);
		log.info("Created new cache region [" + regionName + "]");

		if (snapshot != null) {
			clientRegion.loadSnapshot(snapshot.getInputStream());
		}

		return clientRegion;
	}

	private void addCacheListeners(ClientRegionFactory<K, V> factory) {
		if (attributes != null) {
			CacheListener<K, V>[] listeners = attributes.getCacheListeners();

			if (!ObjectUtils.isEmpty(listeners)) {
				for (CacheListener<K, V> listener : listeners) {
					factory.addCacheListener(listener);
				}
			}
		}

		if (!ObjectUtils.isEmpty(cacheListeners)) {
			for (CacheListener<K, V> listener : cacheListeners) {
				factory.addCacheListener(listener);
			}
		}
	}

	protected void postProcess(Region<K, V> region) {
		registerInterests(region);
		setCacheLoader(region);
		setCacheWriter(region);
	}

	private void registerInterests(final Region<K, V> region) {
		if (!ObjectUtils.isEmpty(interests)) {
			for (Interest<K> interest : interests) {
				if (interest instanceof RegexInterest) {
					region.registerInterestRegex((String) interest.getKey(), interest.getPolicy(),
						interest.isDurable(), interest.isReceiveValues());
				}
				else {
					region.registerInterest(interest.getKey(), interest.getPolicy(), interest.isDurable(),
						interest.isReceiveValues());
				}
			}
		}
	}

	private void setCacheLoader(Region<K, V> region) {
		if (cacheLoader != null) {
			region.getAttributesMutator().setCacheLoader(this.cacheLoader);
		}
	}

	private void setCacheWriter(Region<K, V> region) {
		if (cacheWriter != null) {
			region.getAttributesMutator().setCacheWriter(this.cacheWriter);
		}
	}

	@Override
	public void destroy() throws Exception {
		Region<K, V> region = getObject();

		try {
			if (region != null && !ObjectUtils.isEmpty(interests)) {
				for (Interest<K> interest : interests) {
					if (interest instanceof RegexInterest) {
						region.unregisterInterestRegex((String) interest.getKey());
					}
					else {
						region.unregisterInterest(interest.getKey());
					}
				}
			}
		}
		// NOTE AdminRegion, LocalDataSet, Proxy Region and RegionCreation all throw UnsupportedOperationException;
		// however, should not really happen since Interests are validated at start/registration
		catch (UnsupportedOperationException ex) {
			log.warn("Cannot unregister cache interests", ex);
		}

		if (region != null) {
			if (close) {
				if (!region.getRegionService().isClosed()) {
					try {
						region.close();
					}
					catch (CacheClosedException ignore) {
					}
				}
			}
			// TODO I think Region.close and Region.destroyRegion are mutually exclusive; thus, 1 operation (e.g. close)
			// does not cancel the other (i.e. destroy). This should just be if, not else if.
			else if (destroy) {
				region.destroyRegion();
			}
		}
	}

	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/**
	 * Set the interests for this client region. Both key and regex interest are
	 * supported.
	 * 
	 * @param interests the interests to set
	 */
	public void setInterests(Interest<K>[] interests) {
		this.interests = interests;
	}

	/**
	 * @return the interests
	 */
	Interest<K>[] getInterests() {
		return interests;
	}

	/**
	 * Sets the pool name used by this client.
	 * 
	 * @param poolName
	 */
	public void setPoolName(String poolName) {
		Assert.hasText(poolName, "pool name is required");
		this.poolName = poolName;
	}

	/**
	 * Sets the pool used by this client.
	 * 
	 * @param pool
	 */
	public void setPool(Pool pool) {
		Assert.notNull(pool, "pool cannot be null");
		setPoolName(pool.getName());
	}

	/**
	 * Initializes the client using a GemFire {@link ClientRegionShortcut}. The
	 * recommended way for creating clients since it covers all the major
	 * scenarios with minimal configuration.
	 * 
	 * @param shortcut the ClientRegionShortcut to use.
	 */
	public void setShortcut(ClientRegionShortcut shortcut) {
		this.shortcut = shortcut;
	}

	final boolean isDestroy() {
		return destroy;
	}

	/**
	 * Indicates whether the region referred by this factory bean will be
	 * destroyed on shutdown (default false). Note: destroy and close are
	 * mutually exclusive. Enabling one will automatically disable the other.
	 * <p/>
	 * @param destroy whether or not to destroy the region
	 * @see #setClose(boolean)
	 */
	public void setDestroy(boolean destroy) {
		this.destroy = destroy;
		this.close = (this.close && !destroy); // retain previous value iff destroy is false;
	}

	final boolean isClose() {
		return close;
	}

	/**
	 * Indicates whether the region referred by this factory bean, will be
	 * closed on shutdown (default true). Note: destroy and close are mutually
	 * exclusive. Enabling one will automatically disable the other.
	 * <p/>
	 * @param close whether to close or not the region
	 * @see #setDestroy(boolean)
	 */
	public void setClose(boolean close) {
		this.close = close;
		this.destroy = (this.destroy && !close); // retain previous value iff close is false.
	}

	/**
	 * Sets the snapshots used for loading a newly <i>created</i> region. That
	 * is, the snapshot will be used <i>only</i> when a new region is created -
	 * if the region already exists, no loading will be performed.
	 * 
	 * @see #setName(String)
	 * @param snapshot the snapshot to set
	 */
	public void setSnapshot(Resource snapshot) {
		this.snapshot = snapshot;
	}

	/**
	 * Sets the cache listeners used for the region used by this factory. Used
	 * only when a new region is created.Overrides the settings specified
	 * through {@link #setAttributes(RegionAttributes)}.
	 * 
	 * @param cacheListeners the cacheListeners to set on a newly created region
	 */
	public void setCacheListeners(CacheListener<K, V>[] cacheListeners) {
		this.cacheListeners = cacheListeners;
	}

	/**
	 * Sets the CacheLoader used to load data local to the client's Region on cache misses.
	 * <p/>
	 * @param cacheLoader a GemFire CacheLoader used to load data into the client Region.
	 * @see com.gemstone.gemfire.cache.CacheLoader
	 */
	public void setCacheLoader(CacheLoader<K, V> cacheLoader) {
		this.cacheLoader = cacheLoader;
	}

	/**
	 * Sets the CacheWriter used to perform a synchronous write-behind when data is put into the client's Region.
	 * <p/>
	 * @param cacheWriter the GemFire CacheWriter used to perform synchronous write-behinds on put ops.
	 * @see com.gemstone.gemfire.cache.CacheWriter
	 */
	public void setCacheWriter(CacheWriter<K, V> cacheWriter) {
		this.cacheWriter = cacheWriter;
	}

	/**
	 * Sets the data policy. Used only when a new region is created.
	 * 
	 * @param dataPolicy the region data policy
	 */
	public void setDataPolicy(DataPolicy dataPolicy) {
		this.dataPolicy = dataPolicy;
	}
	
	/**
	 * An alternative way to set the data policy as a string. Useful for 
	 * property placeholders, etc.
	 * 
	 * @param dataPolicyName
	 */
	public void setDataPolicyName(String dataPolicyName) {
		this.dataPolicyName = dataPolicyName;
	}

	/**
	 * Sets the name of disk store to use for overflow and persistence
	 * @param diskStoreName
	 */
	public void setDiskStoreName(String diskStoreName) {
		this.diskStoreName = diskStoreName;
	}

	/**
	 * Sets the region attributes used for the region used by this factory.
	 * Allows maximum control in specifying the region settings. Used only when
	 * a new region is created. Note that using this method allows for advanced
	 * customization of the region - while it provides a lot of flexibility,
	 * note that it's quite easy to create misconfigured regions (especially in
	 * a client/server scenario).
	 * 
	 * @param attributes the attributes to set on a newly created region
	 */
	public void setAttributes(RegionAttributes<K, V> attributes) {
		this.attributes = attributes;
	}

}
