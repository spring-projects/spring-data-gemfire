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

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheWriter;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.DataPolicyConverter;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} used to create a GemFire client cache {@link Region}.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.data.gemfire.RegionLookupFactoryBean
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.EvictionAttributes
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionAttributes
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientRegionFactory
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.apache.geode.cache.client.Pool
 */
@SuppressWarnings("unused")
public class ClientRegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V>
		implements BeanFactoryAware, DisposableBean {

	private static final Log log = LogFactory.getLog(ClientRegionFactoryBean.class);

	private boolean close = false;
	private boolean destroy = false;

	private BeanFactory beanFactory;

	private Boolean persistent;

	private CacheListener<K, V>[] cacheListeners;

	private CacheLoader<K, V> cacheLoader;

	private CacheWriter<K, V> cacheWriter;

	private Class<K> keyConstraint;
	private Class<V> valueConstraint;

	private ClientRegionShortcut shortcut = null;

	private DataPolicy dataPolicy;

	private EvictionAttributes evictionAttributes;

	private Interest<K>[] interests;

	private RegionAttributes<K, V> attributes;

	private Resource snapshot;

	private String diskStoreName;
	private String poolName;

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		postProcess(getRegion());
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected Region<K, V> lookupRegion(GemFireCache cache, String regionName) throws Exception {
		Assert.isTrue(GemfireUtils.isClient(cache), "A ClientCache is required to create a client Region");

		ClientRegionFactory<K, V> clientRegionFactory =
			((ClientCache) cache).createClientRegionFactory(resolveClientRegionShortcut());

		setAttributes(clientRegionFactory);
		addCacheListeners(clientRegionFactory);
		setDiskStoreName(clientRegionFactory);
		setEvictionAttributes(clientRegionFactory);
		setPoolName(clientRegionFactory);

		if (keyConstraint != null) {
			clientRegionFactory.setKeyConstraint(keyConstraint);
		}

		if (valueConstraint != null) {
			clientRegionFactory.setValueConstraint(valueConstraint);
		}

		return logCreateRegionEvent(create(clientRegionFactory, regionName));
	}

	/* (non-Javadoc) */
	private Region<K, V> create(ClientRegionFactory<K, V> clientRegionFactory, String regionName) {
		return (getParent() != null ? clientRegionFactory.createSubregion(getParent(), regionName)
			: clientRegionFactory.create(regionName));
	}

	/* (non-Javadoc) */
	private Region<K, V> logCreateRegionEvent(Region<K, V> region) {
		if (log.isInfoEnabled()) {
			if (getParent() != null) {
				log.info(String.format("Created new client cache Sub-Region [%1$s] under parent Region [%2$s].",
					region.getName(), getParent().getName()));
			}
			else {
				log.info(String.format("Created new client cache Region [%s].", region.getName()));
			}
		}

		return region;
	}

	/**
	 * Resolves the {@link ClientRegionShortcut} used to configure the data policy of the client {@link Region}.
	 *
	 * @return a {@link ClientRegionShortcut} used to configure the data policy of the client {@link Region}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 */
	ClientRegionShortcut resolveClientRegionShortcut() {
		ClientRegionShortcut resolvedShortcut = this.shortcut;

		if (resolvedShortcut == null) {
			if (this.dataPolicy != null) {
				assertDataPolicyAndPersistentAttributeAreCompatible(this.dataPolicy);

				if (DataPolicy.EMPTY.equals(this.dataPolicy)) {
					resolvedShortcut = ClientRegionShortcut.PROXY;
				}
				else if (DataPolicy.NORMAL.equals(this.dataPolicy)) {
					resolvedShortcut = ClientRegionShortcut.CACHING_PROXY;
				}
				else if (DataPolicy.PERSISTENT_REPLICATE.equals(this.dataPolicy)) {
					resolvedShortcut = ClientRegionShortcut.LOCAL_PERSISTENT;
				}
				else {
					// NOTE the DataPolicy validation is based on the ClientRegionShortcut initialization logic
					// in org.apache.geode.internal.cache.GemFireCacheImpl.initializeClientRegionShortcuts
					throw new IllegalArgumentException(String.format("Data Policy '%s' is invalid for Client Regions",
						this.dataPolicy));
				}
			}
			else {
				resolvedShortcut = (isPersistent() ? ClientRegionShortcut.LOCAL_PERSISTENT
					: ClientRegionShortcut.LOCAL);
			}
		}

		// NOTE the ClientRegionShortcut and Persistent attribute will be compatible if the shortcut
		// was derived from the Data Policy.
		assertClientRegionShortcutAndPersistentAttributeAreCompatible(resolvedShortcut);

		return resolvedShortcut;
	}

	/**
	 * Validates that the settings for ClientRegionShortcut and the 'persistent' attribute in &lt;gfe:*-region&gt; elements
	 * are compatible.
	 *
	 * @param resolvedShortcut the GemFire ClientRegionShortcut resolved form the Spring GemFire XML namespace
	 * configuration meta-data.
	 * @see #isPersistent()
	 * @see #isNotPersistent()
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 */
	private void assertClientRegionShortcutAndPersistentAttributeAreCompatible(ClientRegionShortcut resolvedShortcut) {
		final boolean persistentNotSpecified = (this.persistent == null);

		if (ClientRegionShortcut.LOCAL_PERSISTENT.equals(resolvedShortcut)
			|| ClientRegionShortcut.LOCAL_PERSISTENT_OVERFLOW.equals(resolvedShortcut)) {
			Assert.isTrue(persistentNotSpecified || isPersistent(), String.format(
				"Client Region Shortcut '%s' is invalid when persistent is false", resolvedShortcut));
		}
		else {
			Assert.isTrue(persistentNotSpecified || isNotPersistent(), String.format(
				"Client Region Shortcut '%s' is invalid when persistent is true", resolvedShortcut));
		}
	}

	/**
	 * Validates that the settings for Data Policy and the 'persistent' attribute in &lt;gfe:*-region&gt; elements
	 * are compatible.
	 *
	 * @param resolvedDataPolicy the GemFire Data Policy resolved form the Spring GemFire XML namespace configuration
	 * meta-data.
	 * @see #isPersistent()
	 * @see #isNotPersistent()
	 * @see org.apache.geode.cache.DataPolicy
	 */
	private void assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy resolvedDataPolicy) {
		if (resolvedDataPolicy.withPersistence()) {
			Assert.isTrue(isPersistentUnspecified() || isPersistent(), String.format(
				"Data Policy '%s' is invalid when persistent is false", resolvedDataPolicy));
		}
		else {
			// NOTE otherwise, the Data Policy is without persistence, so...
			Assert.isTrue(isPersistentUnspecified() || isNotPersistent(), String.format(
				"Data Policy '%s' is invalid when persistent is true", resolvedDataPolicy));
		}
	}

	/* (non-Javadoc) */
	private ClientRegionFactory<K, V> setAttributes(ClientRegionFactory<K, V> clientRegionFactory) {
		RegionAttributes<K, V> localAttributes = this.attributes;

		if (localAttributes != null) {
			clientRegionFactory.setCloningEnabled(localAttributes.getCloningEnabled());
			clientRegionFactory.setCompressor(localAttributes.getCompressor());
			clientRegionFactory.setConcurrencyChecksEnabled(localAttributes.getConcurrencyChecksEnabled());
			clientRegionFactory.setConcurrencyLevel(localAttributes.getConcurrencyLevel());
			clientRegionFactory.setCustomEntryIdleTimeout(localAttributes.getCustomEntryIdleTimeout());
			clientRegionFactory.setCustomEntryTimeToLive(localAttributes.getCustomEntryTimeToLive());
			clientRegionFactory.setDiskStoreName(localAttributes.getDiskStoreName());
			clientRegionFactory.setDiskSynchronous(localAttributes.isDiskSynchronous());
			clientRegionFactory.setEntryIdleTimeout(localAttributes.getEntryIdleTimeout());
			clientRegionFactory.setEntryTimeToLive(localAttributes.getEntryTimeToLive());
			clientRegionFactory.setEvictionAttributes(localAttributes.getEvictionAttributes());
			clientRegionFactory.setInitialCapacity(localAttributes.getInitialCapacity());
			clientRegionFactory.setKeyConstraint(localAttributes.getKeyConstraint());
			clientRegionFactory.setLoadFactor(localAttributes.getLoadFactor());
			clientRegionFactory.setPoolName(localAttributes.getPoolName());
			clientRegionFactory.setRegionIdleTimeout(localAttributes.getRegionIdleTimeout());
			clientRegionFactory.setRegionTimeToLive(localAttributes.getRegionTimeToLive());
			clientRegionFactory.setStatisticsEnabled(localAttributes.getStatisticsEnabled());
			clientRegionFactory.setValueConstraint(localAttributes.getValueConstraint());
		}

		return clientRegionFactory;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	private ClientRegionFactory<K, V> addCacheListeners(ClientRegionFactory<K, V> clientRegionFactory) {
		for (CacheListener<K, V> cacheListener : this.<K, V>attributesCacheListeners()) {
			clientRegionFactory.addCacheListener(cacheListener);
		}

		for (CacheListener<K, V> cacheListener : nullSafeArray(this.cacheListeners, CacheListener.class)) {
			clientRegionFactory.addCacheListener(cacheListener);
		}

		return clientRegionFactory;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	private <K, V> CacheListener<K, V>[] attributesCacheListeners() {
		CacheListener[] cacheListeners = (this.attributes != null ? this.attributes.getCacheListeners() : null);
		return nullSafeArray(cacheListeners, CacheListener.class);
	}

	/* (non-Javadoc) */
	private ClientRegionFactory<K, V> setDiskStoreName(ClientRegionFactory<K, V> clientRegionFactory) {
		if (StringUtils.hasText(this.diskStoreName)) {
			clientRegionFactory.setDiskStoreName(this.diskStoreName);
		}

		return clientRegionFactory;
	}

	/* (non-Javadoc) */
	private ClientRegionFactory<K, V> setEvictionAttributes(ClientRegionFactory<K, V> clientRegionFactory) {
		if (this.evictionAttributes != null) {
			clientRegionFactory.setEvictionAttributes(this.evictionAttributes);
		}

		return clientRegionFactory;
	}

	/* (non-Javadoc) */
	private ClientRegionFactory<K, V> setPoolName(ClientRegionFactory<K, V> clientRegionFactory) {
		String poolName = resolvePoolName();

		if (StringUtils.hasText(poolName)) {
			clientRegionFactory.setPoolName(eagerlyInitializePool(poolName));
		}

		return clientRegionFactory;
	}

	/* (non-Javadoc) */
	private String resolvePoolName() {
		String poolName = this.poolName;

		if (!StringUtils.hasText(poolName)) {
			String defaultPoolName = GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME;
			poolName = (this.beanFactory.containsBean(defaultPoolName) ? defaultPoolName : poolName);
		}

		return poolName;
	}

	/* (non-Javadoc) */
	private String eagerlyInitializePool(String poolName) {
		try {
			if (this.beanFactory.isTypeMatch(poolName, Pool.class)) {
				if (log.isDebugEnabled()) {
					log.debug(String.format("Found bean definition for Pool [%1$s]; Eagerly initializing...", poolName));
				}

				this.beanFactory.getBean(poolName, Pool.class);
			}
		}
		catch (BeansException ignore) {
			log.warn(ignore.getMessage());
		}

		return poolName;
	}

	/* (non-Javadoc) */
	protected void postProcess(Region<K, V> region) throws Exception {
		loadSnapshot(region);
		registerInterests(region);
		setCacheLoader(region);
		setCacheWriter(region);
	}

	/* (non-Javadoc) */
	private Region<K, V> loadSnapshot(Region<K, V> region) throws Exception {
		if (snapshot != null) {
			region.loadSnapshot(snapshot.getInputStream());
		}

		return region;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	private Region<K, V> registerInterests(Region<K, V> region) {
		for (Interest<K> interest : nullSafeArray(interests, Interest.class)) {
			if (interest.isRegexType()) {
				region.registerInterestRegex((String) interest.getKey(), interest.getPolicy(),
					interest.isDurable(), interest.isReceiveValues());
			}
			else {
				region.registerInterest(interest.getKey(), interest.getPolicy(), interest.isDurable(),
					interest.isReceiveValues());
			}
		}

		return region;
	}

	/* (non-Javadoc) */
	private Region<K, V> setCacheLoader(Region<K, V> region) {
		if (cacheLoader != null) {
			region.getAttributesMutator().setCacheLoader(this.cacheLoader);
		}

		return region;
	}

	/* (non-Javadoc) */
	private Region<K, V> setCacheWriter(Region<K, V> region) {
		if (cacheWriter != null) {
			region.getAttributesMutator().setCacheWriter(this.cacheWriter);
		}

		return region;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void destroy() throws Exception {
		Region<K, V> region = getObject();

		if (region != null) {
			if (close) {
				if (!region.getRegionService().isClosed()) {
					try {
						region.close();
					}
					catch (Exception ignore) {
					}
				}
			}

			if (destroy) {
				region.destroyRegion();
			}
		}
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

	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/* (non-Javadoc) */
	final boolean isClose() {
		return close;
	}

	/**
	 * Indicates whether the region referred by this factory bean, will be
	 * closed on shutdown (default true). Note: destroy and close are mutually
	 * exclusive. Enabling one will automatically disable the other.
	 *
	 * @param close whether to close or not the region
	 * @see #setDestroy(boolean)
	 */
	public void setClose(boolean close) {
		this.close = close;
		this.destroy = (this.destroy && !close); // retain previous value iff close is false.
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
	 *
	 * @param cacheLoader a GemFire CacheLoader used to load data into the client Region.
	 * @see org.apache.geode.cache.CacheLoader
	 */
	public void setCacheLoader(CacheLoader<K, V> cacheLoader) {
		this.cacheLoader = cacheLoader;
	}

	/**
	 * Sets the CacheWriter used to perform a synchronous write-behind when data is put into the client's Region.
	 *
	 * @param cacheWriter the GemFire CacheWriter used to perform synchronous write-behinds on put ops.
	 * @see org.apache.geode.cache.CacheWriter
	 */
	public void setCacheWriter(CacheWriter<K, V> cacheWriter) {
		this.cacheWriter = cacheWriter;
	}

	/**
	 * Sets the Data Policy. Used only when a new Region is created.
	 *
	 * @param dataPolicy the client Region's Data Policy.
	 * @see org.apache.geode.cache.DataPolicy
	 */
	public void setDataPolicy(DataPolicy dataPolicy) {
		this.dataPolicy = dataPolicy;
	}

	/**
	 * An alternate way to set the Data Policy, using the String name of the enumerated value.
	 *
	 * @param dataPolicyName the enumerated value String name of the Data Policy.
	 * @see org.apache.geode.cache.DataPolicy
	 * @see #setDataPolicy(org.apache.geode.cache.DataPolicy)
	 * @deprecated use setDataPolicy(:DataPolicy) instead.
	 */
	@Deprecated
	public void setDataPolicyName(String dataPolicyName) {
		DataPolicy resolvedDataPolicy = new DataPolicyConverter().convert(dataPolicyName);
		Assert.notNull(resolvedDataPolicy, String.format("Data Policy '%1$s' is invalid.", dataPolicyName));
		setDataPolicy(resolvedDataPolicy);
	}

	/* (non-Javadoc) */
	final boolean isDestroy() {
		return destroy;
	}

	/**
	 * Indicates whether the region referred by this factory bean will be
	 * destroyed on shutdown (default false). Note: destroy and close are
	 * mutually exclusive. Enabling one will automatically disable the other.
	 *
	 * @param destroy whether or not to destroy the region
	 * @see #setClose(boolean)
	 */
	public void setDestroy(boolean destroy) {
		this.destroy = destroy;
		this.close = (this.close && !destroy); // retain previous value iff destroy is false;
	}

	/**
	 * Sets the name of disk store to use for overflow and persistence
	 *
	 * @param diskStoreName a String specifying the 'name' of the client Region Disk Store.
	 */
	public void setDiskStoreName(String diskStoreName) {
		this.diskStoreName = diskStoreName;
	}

	public void setEvictionAttributes(EvictionAttributes evictionAttributes) {
		this.evictionAttributes = evictionAttributes;
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

	/* (non-Javadoc) */
	Interest<K>[] getInterests() {
		return this.interests;
	}

	public void setKeyConstraint(Class<K> keyConstraint) {
		this.keyConstraint = keyConstraint;
	}

	protected boolean isPersistentUnspecified() {
		return (persistent == null);
	}

	protected boolean isPersistent() {
		return Boolean.TRUE.equals(persistent);
	}

	protected boolean isNotPersistent() {
		return Boolean.FALSE.equals(persistent);
	}

	public void setPersistent(final boolean persistent) {
		this.persistent = persistent;
	}

	/**
	 * Sets the {@link Pool} used by this client {@link Region}.
	 *
	 * @param pool GemFire client {@link Pool}.
	 * @see org.apache.geode.cache.client.Pool
	 */
	public void setPool(Pool pool) {
		Assert.notNull(pool, "Pool cannot be null");
		setPoolName(pool.getName());
	}

	/**
	 * Sets the {@link Pool} name used by this client {@link Region}.
	 *
	 * @param poolName String specifying the name of the GemFire client {@link Pool}.
	 */
	public void setPoolName(String poolName) {
		Assert.hasText(poolName, "Pool name is required");
		this.poolName = poolName;
	}

	/**
	 * Initializes the client {@link Region} using a GemFire {@link ClientRegionShortcut}.
	 *
	 * @param shortcut {@link ClientRegionShortcut} used to initialize this client {@link Region}.
	 */
	public void setShortcut(ClientRegionShortcut shortcut) {
		this.shortcut = shortcut;
	}

	/**
	 * Specifies the data snapshots used for loading a newly <i>created</i> {@link Region}.
	 * The snapshot will be used <i>only</i> when a new {@link Region} is created.
	 * If the {@link Region} already exists, no loading will be performed.
	 *
	 * @param snapshot {@link Resource} referencing the snapshot used to load the {@link Region} with data.
	 * @see org.springframework.core.io.Resource
	 */
	public void setSnapshot(Resource snapshot) {
		this.snapshot = snapshot;
	}

	public void setValueConstraint(Class<V> valueConstraint) {
		this.valueConstraint = valueConstraint;
	}
}
