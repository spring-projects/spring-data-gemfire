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

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.StreamSupport;

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
import org.apache.geode.cache.client.PoolManager;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.DataPolicyConverter;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.config.annotation.RegionConfigurer;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} used to construct, configure and initialize a client {@link Region}.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.EvictionAttributes
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionAttributes
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientRegionFactory
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.data.gemfire.DataPolicyConverter
 * @see org.springframework.data.gemfire.RegionLookupFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
 */
@SuppressWarnings("unused")
public class ClientRegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V> implements DisposableBean {

	public static final String DEFAULT_POOL_NAME = "DEFAULT";
	public static final String GEMFIRE_POOL_NAME = GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME;

	private boolean close = false;
	private boolean destroy = false;

	private Boolean persistent;

	private CacheListener<K, V>[] cacheListeners;

	private CacheLoader<K, V> cacheLoader;

	private CacheWriter<K, V> cacheWriter;

	private Class<K> keyConstraint;
	private Class<V> valueConstraint;

	private ClientRegionShortcut shortcut;

	private DataPolicy dataPolicy;

	private EvictionAttributes evictionAttributes;

	private Interest<K>[] interests;

	private List<RegionConfigurer> regionConfigurers = Collections.emptyList();

	private RegionAttributes<K, V> attributes;

	private RegionConfigurer compositeRegionConfigurer = new RegionConfigurer() {

		@Override
		public void configure(String beanName, ClientRegionFactoryBean<?, ?> bean) {
			nullSafeCollection(regionConfigurers)
				.forEach(regionConfigurer -> regionConfigurer.configure(beanName, bean));
		}
	};

	private String diskStoreName;
	private String poolName;

	/**
	 * Creates a new {@link Region} with the given {@link String name}.
	 *
	 * @param gemfireCache reference to the {@link GemFireCache}.
	 * @param regionName {@link String name} of the new {@link Region}.
	 * @return a new {@link Region} with the given {@link String name}.
	 * @see org.apache.geode.cache.GemFireCache
	 * @see org.apache.geode.cache.Region
	 */
	@Override
	protected Region<K, V> createRegion(GemFireCache gemfireCache, String regionName) throws Exception {

		applyRegionConfigurers(regionName);

		ClientCache cache = resolveCache(gemfireCache);

		ClientRegionFactory<K, V> clientRegionFactory =
			postProcess(configure(createClientRegionFactory(cache, resolveClientRegionShortcut())));

		@SuppressWarnings("all")
		Region<K, V> region = newRegion(clientRegionFactory, getParent(), regionName);

		return region;
	}

	/* (non-Javadoc) */
	private void applyRegionConfigurers(String regionName) {
		applyRegionConfigurers(regionName, getCompositeRegionConfigurer());
	}

	/**
	 * Null-safe operation to apply the given array of {@link RegionConfigurer RegionConfigurers}
	 * to this {@link ClientRegionFactoryBean}.
	 *
	 * @param regionName {@link String} containing the name of the {@link Region}.
	 * @param regionConfigurers array of {@link RegionConfigurer RegionConfigurers} applied
	 * to this {@link ClientRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 * @see #applyRegionConfigurers(String, Iterable)
	 */
	protected void applyRegionConfigurers(String regionName, RegionConfigurer... regionConfigurers) {
		applyRegionConfigurers(regionName, Arrays.asList(nullSafeArray(regionConfigurers, RegionConfigurer.class)));
	}

	/**
	 * Null-safe operation to apply the given {@link Iterable} of {@link RegionConfigurer RegionConfigurers}
	 * to this {@link ClientRegionFactoryBean}.
	 *
	 * @param regionName {@link String} containing the name of the {@link Region}.
	 * @param regionConfigurers {@link Iterable} of {@link RegionConfigurer RegionConfigurers} applied
	 * to this {@link ClientRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 */
	protected void applyRegionConfigurers(String regionName, Iterable<RegionConfigurer> regionConfigurers) {
		StreamSupport.stream(nullSafeIterable(regionConfigurers).spliterator(), false)
			.forEach(regionConfigurer -> regionConfigurer.configure(regionName, this));
	}

	/**
	 * Assert the settings for {@link ClientRegionShortcut} and the {@literal persistent} attribute
	 * in &lt;gfe:*-region&gt; elements are compatible.
	 *
	 * @param resolvedShortcut {@link ClientRegionShortcut} resolved from the SDG XML namespace.
	 * @see org.springframework.data.gemfire.client.ClientRegionShortcutWrapper
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 * @see #isNotPersistent()
	 * @see #isPersistent()
	 */
	private void assertClientRegionShortcutAndPersistentAttributeAreCompatible(ClientRegionShortcut resolvedShortcut) {

		final boolean persistentNotSpecified = (this.persistent == null);

		if (ClientRegionShortcutWrapper.valueOf(resolvedShortcut).isPersistent()) {
			Assert.isTrue(persistentNotSpecified || isPersistent(),
				String.format("Client Region Shortcut [%s] is not valid when persistent is false", resolvedShortcut));
		}
		else {
			Assert.isTrue(persistentNotSpecified || isNotPersistent(),
				String.format("Client Region Shortcut [%s] is not valid when persistent is true", resolvedShortcut));
		}
	}

	/**
	 * Assert the settings for {@link DataPolicy} and the persistent attribute
	 * in &lt;gfe:*-region&gt; elements are compatible.
	 *
	 * @param resolvedDataPolicy {@link DataPolicy} resolved from the SDG XML namespace.
	 * @see org.apache.geode.cache.DataPolicy
	 * @see #isNotPersistent()
	 * @see #isPersistent()
	 */
	private void assertDataPolicyAndPersistentAttributeAreCompatible(DataPolicy resolvedDataPolicy) {

		if (resolvedDataPolicy.withPersistence()) {
			Assert.isTrue(isPersistentUnspecified() || isPersistent(),
				String.format("Data Policy [%s] is not valid when persistent is false", resolvedDataPolicy));
		}
		else {
			Assert.isTrue(isPersistentUnspecified() || isNotPersistent(),
				String.format("Data Policy [%s] is not valid when persistent is true", resolvedDataPolicy));
		}
	}

	/* (non-Javadoc) */
	private Region<K, V> newRegion(ClientRegionFactory<K, V> clientRegionFactory,
			Region<?, ?> parentRegion, String regionName) {

		return Optional.ofNullable(parentRegion)
			.map(parent -> {
				logInfo("Creating client Subregion [%1$s] with parent Region [%2$s]",
					regionName, parent.getName());

				return clientRegionFactory.<K, V>createSubregion(parent, regionName);
			})
			.orElseGet(() -> {
				logInfo("Created client Region [%s]", regionName);

				return clientRegionFactory.create(regionName);
			});
	}

	/* (non-Javadoc) */
	private ClientCache resolveCache(GemFireCache gemfireCache) {

		return Optional.ofNullable(gemfireCache)
			.filter(GemfireUtils::isClient)
			.map(cache -> (ClientCache) cache)
			.orElseThrow(() -> newIllegalArgumentException("ClientCache is required"));
	}

	/**
	 * Resolves the {@link ClientRegionShortcut} used to configure the {@link DataPolicy} of the client {@link Region}.
	 *
	 * @return a {@link ClientRegionShortcut} used to configure the {@link DataPolicy} of the client {@link Region}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 * @see org.apache.geode.cache.DataPolicy
	 */
	ClientRegionShortcut resolveClientRegionShortcut() {

		ClientRegionShortcut resolvedShortcut = this.shortcut;

		if (resolvedShortcut == null) {

			DataPolicy dataPolicy = this.dataPolicy;

			if (dataPolicy != null) {

				assertDataPolicyAndPersistentAttributeAreCompatible(dataPolicy);

				if (DataPolicy.EMPTY.equals(dataPolicy)) {
					resolvedShortcut = ClientRegionShortcut.PROXY;
				}
				else if (DataPolicy.NORMAL.equals(dataPolicy)) {
					resolvedShortcut = ClientRegionShortcut.CACHING_PROXY;
				}
				else if (DataPolicy.PERSISTENT_REPLICATE.equals(dataPolicy)) {
					resolvedShortcut = ClientRegionShortcut.LOCAL_PERSISTENT;
				}
				else {
					// NOTE the DataPolicy validation is based on the ClientRegionShortcut initialization logic
					// in org.apache.geode.internal.cache.GemFireCacheImpl.initializeClientRegionShortcuts
					throw newIllegalArgumentException("Data Policy [%s] is not valid for the client Region", dataPolicy);
				}
			}
			else {
				resolvedShortcut = (isPersistent() ? ClientRegionShortcut.LOCAL_PERSISTENT : ClientRegionShortcut.LOCAL);
			}
		}

		// NOTE the ClientRegionShortcut and Persistent attribute will be compatible
		// if the shortcut was derived from the Data Policy.
		assertClientRegionShortcutAndPersistentAttributeAreCompatible(resolvedShortcut);

		return resolvedShortcut;
	}

	/* (non-Javadoc) */
	private String resolvePoolName() {
		return Optional.of(getPoolName()).filter(this::isPoolResolvable).orElse(null);
	}

	/* (non-Javadoc) */
	private String getPoolName() {
		return Optional.ofNullable(this.poolName).filter(StringUtils::hasText).orElse(GEMFIRE_POOL_NAME);
	}

	/* (non-Javadoc) */
	private boolean isPoolResolvable(String poolName) {
		return (getBeanFactory().containsBean(poolName) || (PoolManager.find(poolName) != null));
	}

	/* (non-Javadoc) */
	private String eagerlyInitializePool(String poolName) {

		try {
			if (getBeanFactory().isTypeMatch(poolName, Pool.class)) {
				logDebug("Found bean definition for Pool [%s]; Eagerly initializing...", poolName);
				getBeanFactory().getBean(poolName, Pool.class);
			}
		}
		catch (BeansException ignore) {
			getLog().warn(ignore.getMessage(), ignore.getCause());
		}

		return poolName;
	}

	/**
	 * Constructs a new instance of {@link ClientRegionFactory} using the given {@link ClientCache}
	 * and {@link ClientRegionShortcut}.
	 *
	 * @param cache reference to the {@link ClientCache}.
	 * @param shortcut {@link ClientRegionShortcut} used to specify the client {@link Region} {@link DataPolicy}.
	 * @return a new instance of {@link ClientRegionFactory}.
	 * @see org.apache.geode.cache.client.ClientCache#createClientRegionFactory(ClientRegionShortcut)
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 * @see org.apache.geode.cache.client.ClientRegionFactory
	 */
	protected ClientRegionFactory<K, V> createClientRegionFactory(ClientCache cache, ClientRegionShortcut shortcut) {
		return cache.createClientRegionFactory(shortcut);
	}

	/**
	 * Configures the given {@link ClientRegionFactoryBean} from the configuration settings
	 * of this {@link ClientRegionFactoryBean}.
	 *
	 * @param clientRegionFactory {@link ClientRegionFactory} to configure.
	 * @return the given {@link ClientRegionFactory}.
	 * @see org.apache.geode.cache.client.ClientRegionFactory
	 */
	protected ClientRegionFactory<K, V> configure(ClientRegionFactory<K, V> clientRegionFactory) {

		Optional.ofNullable(this.attributes).ifPresent(attributes -> {

			stream(nullSafeArray(attributes.getCacheListeners(), CacheListener.class))
				.forEach(clientRegionFactory::addCacheListener);

			clientRegionFactory.setCloningEnabled(attributes.getCloningEnabled());
			clientRegionFactory.setCompressor(attributes.getCompressor());
			clientRegionFactory.setConcurrencyChecksEnabled(attributes.getConcurrencyChecksEnabled());
			clientRegionFactory.setConcurrencyLevel(attributes.getConcurrencyLevel());
			clientRegionFactory.setCustomEntryIdleTimeout(attributes.getCustomEntryIdleTimeout());
			clientRegionFactory.setCustomEntryTimeToLive(attributes.getCustomEntryTimeToLive());
			clientRegionFactory.setDiskStoreName(attributes.getDiskStoreName());
			clientRegionFactory.setDiskSynchronous(attributes.isDiskSynchronous());
			clientRegionFactory.setEntryIdleTimeout(attributes.getEntryIdleTimeout());
			clientRegionFactory.setEntryTimeToLive(attributes.getEntryTimeToLive());
			clientRegionFactory.setEvictionAttributes(attributes.getEvictionAttributes());
			clientRegionFactory.setInitialCapacity(attributes.getInitialCapacity());
			clientRegionFactory.setKeyConstraint(attributes.getKeyConstraint());
			clientRegionFactory.setLoadFactor(attributes.getLoadFactor());
			clientRegionFactory.setPoolName(attributes.getPoolName());
			clientRegionFactory.setRegionIdleTimeout(attributes.getRegionIdleTimeout());
			clientRegionFactory.setRegionTimeToLive(attributes.getRegionTimeToLive());
			clientRegionFactory.setStatisticsEnabled(attributes.getStatisticsEnabled());
			clientRegionFactory.setValueConstraint(attributes.getValueConstraint());
		});

		stream(nullSafeArray(this.cacheListeners, CacheListener.class)).forEach(clientRegionFactory::addCacheListener);

		Optional.ofNullable(this.diskStoreName).filter(StringUtils::hasText)
			.ifPresent(clientRegionFactory::setDiskStoreName);

		Optional.ofNullable(this.evictionAttributes).ifPresent(clientRegionFactory::setEvictionAttributes);

		Optional.ofNullable(this.keyConstraint).ifPresent(clientRegionFactory::setKeyConstraint);

		Optional.ofNullable(resolvePoolName()).filter(StringUtils::hasText)
			.ifPresent(poolName -> clientRegionFactory.setPoolName(eagerlyInitializePool(poolName)));

		Optional.ofNullable(this.valueConstraint).ifPresent(clientRegionFactory::setValueConstraint);

		return clientRegionFactory;
	}

	/**
	 * Post-process the given {@link ClientRegionFactory} setup by this {@link ClientRegionFactoryBean}.
	 *
	 * @param clientRegionFactory {@link ClientRegionFactory} to process.
	 * @return the given {@link ClientRegionFactory}.
	 * @see org.apache.geode.cache.client.ClientRegionFactory
	 */
	protected ClientRegionFactory<K, V> postProcess(ClientRegionFactory<K, V> clientRegionFactory) {
		return clientRegionFactory;
	}

	/**
	 * Post-process the {@link Region} created by this {@link ClientRegionFactoryBean}.
	 *
	 * @param region {@link Region} to process.
	 * @see org.apache.geode.cache.Region
	 */
	@Override
	protected Region<K, V> postProcess(Region<K, V> region) {

		super.postProcess(region);

		registerInterests(region);

		Optional.ofNullable(this.cacheLoader)
			.ifPresent(cacheLoader -> region.getAttributesMutator().setCacheLoader(cacheLoader));

		Optional.ofNullable(this.cacheWriter)
			.ifPresent(cacheWriter -> region.getAttributesMutator().setCacheWriter(cacheWriter));

		return region;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	private Region<K, V> registerInterests(Region<K, V> region) {

		stream(nullSafeArray(this.interests, Interest.class)).forEach(interest -> {

			if (interest.isRegexType()) {
				region.registerInterestRegex((String) interest.getKey(), interest.getPolicy(),
					interest.isDurable(), interest.isReceiveValues());
			}
			else {
				region.registerInterest(((Interest<K>) interest).getKey(), interest.getPolicy(),
					interest.isDurable(), interest.isReceiveValues());
			}
		});

		return region;
	}

	/**
	 * Closes and destroys the {@link Region}.
	 *
	 * @throws Exception if destroy fails.
	 * @see org.springframework.beans.factory.DisposableBean
	 */
	@Override
	public void destroy() throws Exception {

		Optional.ofNullable(getObject()).ifPresent(region -> {

			if (isClose()) {
				if (!region.getRegionService().isClosed()) {
					try {
						region.close();
					}
					catch (Exception ignore) {
					}
				}
			}

			if (isDestroy()) {
				region.destroyRegion();
			}
		});
	}

	/**
	 * Returns a reference to the Composite {@link RegionConfigurer} used to apply additional configuration
	 * to this {@link ClientRegionFactoryBean} on Spring container initialization.
	 *
	 * @return the Composite {@link RegionConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 */
	protected RegionConfigurer getCompositeRegionConfigurer() {
		return this.compositeRegionConfigurer;
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

	/* (non-Javadoc) */
	final boolean isClose() {
		return this.close;
	}

	/**
	 * Indicates whether the region referred by this factory bean will be closed on shutdown (default true).
	 *
	 * Note: destroy and close are mutually exclusive. Enabling one will automatically disable the other.
	 *
	 * @param close whether to close or not the region
	 * @see #setDestroy(boolean)
	 */
	public void setClose(boolean close) {
		this.close = close;
		this.destroy = (this.destroy && !close); // retain previous value iff close is false.
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
		Assert.notNull(resolvedDataPolicy, String.format("Data Policy [%1$s] is not valid", dataPolicyName));
		setDataPolicy(resolvedDataPolicy);
	}

	/* (non-Javadoc) */
	final boolean isDestroy() {
		return this.destroy;
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
		setPoolName(Optional.ofNullable(pool).map(Pool::getName)
			.orElseThrow(() -> newIllegalArgumentException("Pool cannot be null")));
	}

	/**
	 * Sets the {@link Pool} name used by this client {@link Region}.
	 *
	 * @param poolName String specifying the name of the GemFire client {@link Pool}.
	 */
	public void setPoolName(String poolName) {
		this.poolName = Optional.ofNullable(poolName).filter(StringUtils::hasText)
			.orElseThrow(() -> newIllegalArgumentException("Pool name is required"));
	}

	/**
	 * Null-safe operation to set an array of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link ClientRegionFactoryBean} when using Annotation-based configuration.
	 *
	 * @param regionConfigurers array of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link ClientRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 * @see #setRegionConfigurers(List)
	 */
	public void setRegionConfigurers(RegionConfigurer... regionConfigurers) {
		setRegionConfigurers(Arrays.asList(nullSafeArray(regionConfigurers, RegionConfigurer.class)));
	}

	/**
	 * Null-safe operation to set an {@link Iterable} of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link ClientRegionFactoryBean} when using Annotation-based configuration.
	 *
	 * @param regionConfigurers {@link Iterable} of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link ClientRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 */
	public void setRegionConfigurers(List<RegionConfigurer> regionConfigurers) {
		this.regionConfigurers = Optional.ofNullable(regionConfigurers).orElseGet(Collections::emptyList);
	}

	/**
	 * Initializes the client {@link Region} using a GemFire {@link ClientRegionShortcut}.
	 *
	 * @param shortcut {@link ClientRegionShortcut} used to initialize this client {@link Region}.
	 */
	public void setShortcut(ClientRegionShortcut shortcut) {
		this.shortcut = shortcut;
	}

	public void setValueConstraint(Class<V> valueConstraint) {
		this.valueConstraint = valueConstraint;
	}
}
