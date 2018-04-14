/*
 * Copyright 2010-2018 the original author or authors.
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

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.util.Optional;

import org.apache.geode.cache.AttributesFactory;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheWriter;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.PartitionAttributes;
import org.apache.geode.cache.PartitionAttributesFactory;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionFactory;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.Scope;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.compression.Compressor;
import org.apache.geode.internal.cache.UserSpecifiedRegionAttributes;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.util.RegionUtils;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

/**
 * Abstract Spring {@link FactoryBean} base class extended by other SDG {@link FactoryBean FactoryBeans} used to
 * construct, configure and initialize peer {@link Region Regions}.
 *
 * This {@link FactoryBean} allows for very easy and flexible creation of peer {@link Region}.
 * For client {@link Region Regions}, however, see the {@link ClientRegionFactoryBean}.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.CacheListener
 * @see org.apache.geode.cache.CacheLoader
 * @see org.apache.geode.cache.CacheWriter
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.EvictionAttributes
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.PartitionAttributes
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionAttributes
 * @see org.apache.geode.cache.RegionFactory
 * @see org.apache.geode.cache.RegionShortcut
 * @see org.apache.geode.cache.Scope
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.context.SmartLifecycle
 * @see RegionLookupFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
 */
@SuppressWarnings("unused")
// TODO: Rename to PeerRegionFatoryBean in SD Lovelace
public abstract class RegionFactoryBean<K, V> extends ConfigurableRegionFactoryBean<K, V>
		implements DisposableBean, SmartLifecycle {

	private boolean close = true;
	private boolean destroy = false;
	private boolean running;

	private Boolean offHeap;
	private Boolean persistent;

	private AsyncEventQueue[] asyncEventQueues;

	private CacheListener<K, V>[] cacheListeners;

	private CacheLoader<K, V> cacheLoader;

	private CacheWriter<K, V> cacheWriter;

	private Class<K> keyConstraint;
	private Class<V> valueConstraint;

	private Compressor compressor;

	private DataPolicy dataPolicy;

	private EvictionAttributes evictionAttributes;

	private GatewaySender[] gatewaySenders;

	private RegionAttributes<K, V> attributes;

	private RegionShortcut shortcut;

	private Resource snapshot;

	private Scope scope;

	private String diskStoreName;

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

		verifyLockGrantorEligibility(getAttributes(), getScope());

		Cache cache = resolveCache(gemfireCache);

		RegionFactory<K, V> regionFactory = postProcess(configure(createRegionFactory(cache)));

		Region<K, V> region = newRegion(regionFactory, getParent(), regionName);

		return enableAsLockGrantor(region);
	}

	private Region<K, V> enableAsLockGrantor(Region<K, V> region) {

		Optional.ofNullable(region)
			.filter(it -> it.getAttributes().isLockGrantor())
			.ifPresent(Region::becomeLockGrantor);

		return region;
	}

	private Region<K, V> newRegion(RegionFactory<K, V> regionFactory, Region<?, ?> parentRegion, String regionName) {

		return Optional.ofNullable(parentRegion)
			.map(parent -> {
				logInfo("Creating Subregion [%1$s] with parent Region [%2$s]",
					regionName, parent.getName());

				return regionFactory.<K, V>createSubregion(parent, regionName);
			})
			.orElseGet(() -> {
				logInfo("Created Region [%1$s]", regionName);

				return regionFactory.create(regionName);
			});
	}

	private Cache resolveCache(GemFireCache gemfireCache) {

		return Optional.ofNullable(gemfireCache)
			.filter(cache -> cache instanceof Cache)
			.map(cache -> (Cache) cache)
			.orElseThrow(() -> newIllegalArgumentException("Peer Cache is required"));
	}

	private RegionAttributes<K, V> verifyLockGrantorEligibility(RegionAttributes<K, V> regionAttributes, Scope scope) {

		Optional.ofNullable(regionAttributes).ifPresent(attributes ->
			Assert.state(!attributes.isLockGrantor() || verifyScope(scope),
				"Lock Grantor only applies to GLOBAL Scoped Regions"));

		return regionAttributes;
	}

	private boolean verifyScope(Scope scope) {
		return scope == null || Scope.GLOBAL.equals(scope);
	}

	/**
	 * Creates an instance of {@link RegionFactory} with the given {@link Cache} which is then used to construct,
	 * configure and initialize the {@link Region} specified by this {@link RegionFactoryBean}.
	 *
	 * @param cache reference to the {@link Cache}.
	 * @return a {@link RegionFactory} used to construct, configure and initialized the {@link Region} specified by
	 * this {@link RegionFactoryBean}.
	 * @see org.apache.geode.cache.Cache#createRegionFactory(org.apache.geode.cache.RegionShortcut)
	 * @see org.apache.geode.cache.Cache#createRegionFactory(org.apache.geode.cache.RegionAttributes)
	 * @see org.apache.geode.cache.Cache#createRegionFactory()
	 * @see org.apache.geode.cache.RegionFactory
	 */
	protected RegionFactory<K, V> createRegionFactory(Cache cache) {

		if (this.shortcut != null) {

			RegionFactory<K, V> regionFactory =
				mergeRegionAttributes(cache.createRegionFactory(this.shortcut), this.attributes);

			setDataPolicy(getDataPolicy(regionFactory, this.shortcut));

			return regionFactory;
		}
		else if (this.attributes != null) {
			return cache.createRegionFactory(this.attributes);
		}
		else {
			return cache.createRegionFactory();
		}
	}

	/**
	 * Configures the {@link RegionFactory} based on the configuration settings of this {@link RegionFactoryBean}.
	 *
	 * @param regionFactory {@link RegionFactory} to configure
	 * @return the given {@link RegionFactory}.
	 * @see org.apache.geode.cache.RegionFactory
	 */
	protected RegionFactory<K, V> configure(RegionFactory<K, V> regionFactory) {

		stream(nullSafeArray(this.asyncEventQueues, AsyncEventQueue.class))
			.forEach(asyncEventQueue -> regionFactory.addAsyncEventQueueId(asyncEventQueue.getId()));

		stream(nullSafeArray(this.cacheListeners, CacheListener.class)).forEach(regionFactory::addCacheListener);

		Optional.ofNullable(this.cacheLoader).ifPresent(regionFactory::setCacheLoader);

		Optional.ofNullable(this.cacheWriter).ifPresent(regionFactory::setCacheWriter);

		Optional.ofNullable(this.compressor).ifPresent(regionFactory::setCompressor);

		resolveDataPolicy(regionFactory, this.persistent, this.dataPolicy);

		Optional.ofNullable(this.diskStoreName)
			.filter(name -> isDiskStoreConfigurationAllowed())
			.ifPresent(regionFactory::setDiskStoreName);

		Optional.ofNullable(this.evictionAttributes).ifPresent(regionFactory::setEvictionAttributes);

		stream(nullSafeArray(this.gatewaySenders, GatewaySender.class))
			.forEach(gatewaySender -> regionFactory.addGatewaySenderId(gatewaySender.getId()));

		Optional.ofNullable(this.keyConstraint).ifPresent(regionFactory::setKeyConstraint);

		Optional.ofNullable(getScope()).ifPresent(regionFactory::setScope);

		Optional.ofNullable(this.valueConstraint).ifPresent(regionFactory::setValueConstraint);

		return regionFactory;
	}

	/**
	 * Post-process the {@link RegionFactory} used to create the {@link Region} specified by
	 * this {@link RegionFactoryBean} during initialization.
	 *
	 * The {@link RegionFactory} has been already constructed, configured and initialized by
	 * this {@link RegionFactoryBean} before this method gets invoked.
	 *
	 * @param regionFactory {@link RegionFactory} used to create the {@link Region}.
	 * @return the given {@link RegionFactory}.
	 * @see org.apache.geode.cache.RegionFactory
	 */
	protected RegionFactory<K, V> postProcess(RegionFactory<K, V> regionFactory) {

		Optional.ofNullable(this.offHeap).ifPresent(regionFactory::setOffHeap);

		return regionFactory;
	}

	/*
	 * (non-Javadoc)
	 *
	 * This method is not considered part of the RegionFactoryBean API and is strictly used for testing purposes!
	 *
	 * NOTE: Cannot pass RegionAttributes.class as the "targetType" in the second invocation of getFieldValue(..)
	 * since the "regionAttributes" field is naively declared as a instance of the implementation class type
	 * (RegionAttributesImpl) rather than the interface type (RegionAttributes)...
	 * so much for 'programming to interfaces' in GemFire!
	 *
	 * @see org.apache.geode.cache.RegionFactory#attrsFactory
	 * @see org.apache.geode.cache.AttributesFactory#regionAttributes
	 * @see org.apache.geode.cache.RegionAttributes#getDataPolicy
	 * @see org.apache.geode.cache.DataPolicy
	 */
	@SuppressWarnings({ "deprecation", "unchecked" })
	DataPolicy getDataPolicy(RegionFactory regionFactory, RegionShortcut regionShortcut) {

		return getFieldValue(regionFactory, "attrsFactory", AttributesFactory.class)
			.flatMap(attributesFactory -> getFieldValue(attributesFactory,"regionAttributes", null))
			.map(regionAttributes -> ((RegionAttributes<K, V>) regionAttributes).getDataPolicy())
			.orElseGet(() -> RegionShortcutToDataPolicyConverter.INSTANCE.convert(regionShortcut));
	}

	@SuppressWarnings("unchecked")
	private <T> Optional<T> getFieldValue(Object source, String fieldName, Class<T> targetType) {

		return Optional.ofNullable(source)
			.map(Object::getClass)
			.map(type -> ReflectionUtils.findField(type, fieldName, targetType))
			.map(field -> {
				ReflectionUtils.makeAccessible(field);
				return field;
			})
			.map(field -> (T) ReflectionUtils.getField(field, source));
	}

	/**
	 * Intelligently merges the given RegionAttributes with the configuration setting of the RegionFactory. This method
	 * is used to merge the RegionAttributes and PartitionAttributes with the RegionFactory that is created when the
	 * user specified a RegionShortcut.  This method gets called by the createRegionFactory method depending upon
	 * the value passed to the Cache.createRegionFactory() method (i.e. whether there was a RegionShortcut specified
	 * or not).
	 *
	 * @param <K> the Class type fo the Region key.
	 * @param <V> the Class type of the Region value.
	 * @param regionFactory the GemFire RegionFactory used to configure and create the Region that is the product
	 * of this RegionFactoryBean.
	 * @param regionAttributes the RegionAttributes containing the Region configuration settings to merge to the
	 * RegionFactory.
	 * @return the RegionFactory with the configuration settings of the RegionAttributes merged.
	 * @see #isUserSpecifiedEvictionAttributes(org.apache.geode.cache.RegionAttributes)
	 * @see #validateRegionAttributes(org.apache.geode.cache.RegionAttributes)
	 * @see org.apache.geode.cache.RegionAttributes
	 * @see org.apache.geode.cache.RegionFactory
	 */
	@SuppressWarnings("unchecked")
	protected <K, V> RegionFactory<K, V> mergeRegionAttributes(RegionFactory<K, V> regionFactory,
			RegionAttributes<K, V> regionAttributes) {

		if (regionAttributes != null) {

			// NOTE: this validation may not be strictly necessary depending on how the RegionAttributes were "created",
			validateRegionAttributes(regionAttributes);

			regionFactory.setCloningEnabled(regionAttributes.getCloningEnabled());
			regionFactory.setCompressor(regionAttributes.getCompressor());
			regionFactory.setConcurrencyChecksEnabled(regionAttributes.getConcurrencyChecksEnabled());
			regionFactory.setConcurrencyLevel(regionAttributes.getConcurrencyLevel());
			regionFactory.setCustomEntryIdleTimeout(regionAttributes.getCustomEntryIdleTimeout());
			regionFactory.setCustomEntryTimeToLive(regionAttributes.getCustomEntryTimeToLive());
			regionFactory.setDiskSynchronous(regionAttributes.isDiskSynchronous());
			regionFactory.setEnableAsyncConflation(regionAttributes.getEnableAsyncConflation());
			regionFactory.setEnableSubscriptionConflation(regionAttributes.getEnableSubscriptionConflation());
			regionFactory.setEntryIdleTimeout(regionAttributes.getEntryIdleTimeout());
			regionFactory.setEntryTimeToLive(regionAttributes.getEntryTimeToLive());

			// NOTE: EvictionAttributes are created by certain RegionShortcuts; need the null check!
			if (isUserSpecifiedEvictionAttributes(regionAttributes)) {
				regionFactory.setEvictionAttributes(regionAttributes.getEvictionAttributes());
			}

			regionFactory.setIgnoreJTA(regionAttributes.getIgnoreJTA());
			regionFactory.setIndexMaintenanceSynchronous(regionAttributes.getIndexMaintenanceSynchronous());
			regionFactory.setInitialCapacity(regionAttributes.getInitialCapacity());
			regionFactory.setKeyConstraint(regionAttributes.getKeyConstraint());
			regionFactory.setLoadFactor(regionAttributes.getLoadFactor());
			regionFactory.setLockGrantor(regionAttributes.isLockGrantor());
			regionFactory.setMembershipAttributes(regionAttributes.getMembershipAttributes());
			regionFactory.setMulticastEnabled(regionAttributes.getMulticastEnabled());
			regionFactory.setOffHeap(regionAttributes.getOffHeap());

			mergePartitionAttributes(regionFactory, regionAttributes);

			regionFactory.setPoolName(regionAttributes.getPoolName());
			regionFactory.setRegionIdleTimeout(regionAttributes.getRegionIdleTimeout());
			regionFactory.setRegionTimeToLive(regionAttributes.getRegionTimeToLive());
			regionFactory.setStatisticsEnabled(regionAttributes.getStatisticsEnabled());
			regionFactory.setSubscriptionAttributes(regionAttributes.getSubscriptionAttributes());
			regionFactory.setValueConstraint(regionAttributes.getValueConstraint());
		}

		return regionFactory;
	}

	/**
	 *
	 * @param regionFactory
	 * @param regionAttributes
	 */
	protected <K, V> void mergePartitionAttributes(RegionFactory<K, V> regionFactory,
			RegionAttributes<K, V> regionAttributes) {

		// NOTE: PartitionAttributes are created by certain RegionShortcuts; need the null check since RegionAttributes
		// can technically return null!
		// NOTE: Most likely, the PartitionAttributes will never be null since the PartitionRegionFactoryBean always
		// sets a PartitionAttributesFactoryBean BeanBuilder on the RegionAttributesFactoryBean "partitionAttributes"
		// property.
		if (regionAttributes.getPartitionAttributes() != null) {

			PartitionAttributes partitionAttributes = regionAttributes.getPartitionAttributes();

			PartitionAttributesFactory partitionAttributesFactory = new PartitionAttributesFactory(partitionAttributes);

			RegionShortcutWrapper shortcutWrapper = RegionShortcutWrapper.valueOf(shortcut);

			// NOTE: However, since the default value of redundancy is 0, we need to account for 'redundant'
			// RegionShortcut types, which specify a redundancy of 1.
			if (shortcutWrapper.isRedundant() && partitionAttributes.getRedundantCopies() == 0) {
				partitionAttributesFactory.setRedundantCopies(1);
			}

			// NOTE: And, since the default value of localMaxMemory is based on the system memory, we need to
			// account for 'proxy' RegionShortcut types, which specify a local max memory of 0.
			if (shortcutWrapper.isProxy()) {
				partitionAttributesFactory.setLocalMaxMemory(0);
			}

			// NOTE: Internally, RegionFactory.setPartitionAttributes handles merging the PartitionAttributes, hooray!
			regionFactory.setPartitionAttributes(partitionAttributesFactory.create());
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * This method is not part of the RegionFactoryBean API and is strictly used for testing purposes!
	 *
	 * @see org.apache.geode.cache.AttributesFactory#validateAttributes(:RegionAttributes)
	 */
	@SuppressWarnings("deprecation")
	void validateRegionAttributes(RegionAttributes regionAttributes) {
		org.apache.geode.cache.AttributesFactory.validateAttributes(regionAttributes);
	}

	/*
	 * (non-Javadoc)
	 *
	 * This method is not part of the RegionFactoryBean API and is strictly used for testing purposes!
	 *
	 * NOTE unfortunately, must resort to using a GemFire internal class, ugh!
	 *
	 * @see org.apache.geode.internal.cache.UserSpecifiedRegionAttributes#hasEvictionAttributes
	 */
	boolean isUserSpecifiedEvictionAttributes(final RegionAttributes regionAttributes) {
		return (regionAttributes instanceof UserSpecifiedRegionAttributes
			&& ((UserSpecifiedRegionAttributes) regionAttributes).hasEvictionAttributes());
	}

	private boolean isDiskStoreConfigurationAllowed() {

		boolean allow = StringUtils.hasText(this.diskStoreName);

		allow &= (getDataPolicy().withPersistence()
			|| (getAttributes() != null
			&& getAttributes().getEvictionAttributes() != null
			&& EvictionAction.OVERFLOW_TO_DISK.equals(attributes.getEvictionAttributes().getAction())));

		return allow;
	}

	/**
	 * Returns true when the user explicitly specified a value for the persistent attribute and it is true.  If the
	 * persistent attribute was not explicitly specified, then the persistence setting is implicitly undefined
	 * and will be determined by the Data Policy.
	 *
	 * @return true when the user specified an explicit value for the persistent attribute and it is true;
	 * false otherwise.
	 * @see #isNotPersistent()
	 */
	protected boolean isPersistent() {
		return Boolean.TRUE.equals(persistent);
	}

	/**
	 * Returns true when the user explicitly specified a value for the persistent attribute and it is false.  If the
	 * persistent attribute was not explicitly specified, then the persistence setting is implicitly undefined
	 * and will be determined by the Data Policy.
	 *
	 * @return true when the user specified an explicit value for the persistent attribute and it is false;
	 * false otherwise.
	 * @see #isPersistent()
	 */
	protected boolean isNotPersistent() {
		return Boolean.FALSE.equals(persistent);
	}

	/**
=======
>>>>>>> c22ebe6... DATAGEODE-12 - Introduce Spring Configurers to flexibly alter Spring Data GemFire configuration when using Annotation config.
=======
>>>>>>> 1fd41c9... DATAGEODE-100 - Avoid Pool Already Exists Exception on Spring container initialization.
	 * Validates and sets the Data Policy on the RegionFactory used to create and configure the Region from this
	 * FactoryBean.
	 *
	 * @param regionFactory the RegionFactory used by this FactoryBean to create and configure the Region.
	 * @param persistent a boolean value indicating whether the Region should be persistent and persist it's
	 * data to disk.
	 * @param dataPolicy the configured Data Policy for the Region.
	 * @see #resolveDataPolicy(org.apache.geode.cache.RegionFactory, Boolean, String)
	 * @see org.apache.geode.cache.DataPolicy
	 * @see org.apache.geode.cache.RegionFactory
	 */
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, DataPolicy dataPolicy) {

		if (dataPolicy != null) {
			RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(dataPolicy, this.persistent);
			regionFactory.setDataPolicy(dataPolicy);
			setDataPolicy(dataPolicy);
		}
		else {
			resolveDataPolicy(regionFactory, persistent, (String) null);
		}
	}

	/**
	 * Validates the configured Data Policy and may override it, taking into account the 'persistent' attribute
	 * and constraints for the Region type.
	 *
	 * @param regionFactory the GemFire RegionFactory used to create the desired Region.
	 * @param persistent a boolean value indicating whether the Region should persist it's data to disk.
	 * @param dataPolicy requested Data Policy as set by the user in the Spring GemFire configuration meta-data.
	 * @see org.apache.geode.cache.DataPolicy
	 * @see org.apache.geode.cache.RegionFactory
	 */
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, String dataPolicy) {

		if (dataPolicy != null) {

			DataPolicy resolvedDataPolicy = new DataPolicyConverter().convert(dataPolicy);

			Assert.notNull(resolvedDataPolicy, String.format("Data Policy [%s] is invalid", dataPolicy));

			RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(resolvedDataPolicy, this.persistent);

			regionFactory.setDataPolicy(resolvedDataPolicy);
			setDataPolicy(resolvedDataPolicy);
		}
		else {

			DataPolicy regionAttributesDataPolicy = getDataPolicy(getAttributes(), DataPolicy.DEFAULT);

			DataPolicy resolvedDataPolicy = isPersistent() && DataPolicy.DEFAULT.equals(regionAttributesDataPolicy)
				? DataPolicy.PERSISTENT_REPLICATE : regionAttributesDataPolicy;

			RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(resolvedDataPolicy, this.persistent);

			regionFactory.setDataPolicy(resolvedDataPolicy);
			setDataPolicy(resolvedDataPolicy);
		}
	}

	private DataPolicy getDataPolicy(RegionAttributes regionAttributes, DataPolicy defaultDataPolicy) {
		return Optional.ofNullable(regionAttributes).map(RegionAttributes::getDataPolicy).orElse(defaultDataPolicy);
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
			if (this.close) {
				if (!region.getRegionService().isClosed()) {
					try {
						region.close();
					}
					catch (Exception ignore) {
					}
				}

			}

			if (this.destroy) {
				region.destroyRegion();
			}
		});
	}

	/**
	 * The list of AsyncEventQueues to use with this Region.
	 *
	 * @param asyncEventQueues defined as Object for backwards compatibility with Gemfire 6.
	 */
	public void setAsyncEventQueues(AsyncEventQueue[] asyncEventQueues) {
		this.asyncEventQueues = asyncEventQueues;
	}

	/**
	 * Sets the region attributes used for the region used by this factory.
	 * Allows maximum control in specifying the region settings. Used only when
	 * a new region is created.
	 *
	 * @param attributes the attributes to set on a newly created region
	 */
	public void setAttributes(RegionAttributes<K, V> attributes) {
		this.attributes = attributes;
	}

	/**
	 * Returns the attributes used to configure the Region created by this factory as set in the SDG XML namespace
	 * configuration meta-data, or as set with the setAttributes(:Attributes) method.
	 *
	 * @return the RegionAttributes used to configure the Region created by this factory.
	 * @see org.apache.geode.cache.RegionAttributes
	 */
	public RegionAttributes<K, V> getAttributes() {
		return Optional.ofNullable(getRegion()).map(Region::getAttributes).orElse(this.attributes);
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
	 * Sets the cache loader used for the region used by this factory. Used only
	 * when a new region is created.Overrides the settings specified through
	 * {@link #setAttributes(RegionAttributes)}.
	 *
	 * @param cacheLoader the cacheLoader to set on a newly created region
	 */
	public void setCacheLoader(CacheLoader<K, V> cacheLoader) {
		this.cacheLoader = cacheLoader;
	}

	/**
	 * Sets the cache writer used for the region used by this factory. Used only
	 * when a new region is created. Overrides the settings specified through
	 * {@link #setAttributes(RegionAttributes)}.
	 *
	 * @param cacheWriter the cacheWriter to set on a newly created region
	 */
	public void setCacheWriter(CacheWriter<K, V> cacheWriter) {
		this.cacheWriter = cacheWriter;
	}

	/**
	 * Indicates whether the Region referred to by this factory bean will be closed on shutdown (default true).
	 *
	 * @param close a boolean value indicating whether this Region should be closed on member shutdown.
	 * @see #setDestroy(boolean)
	 */
	public void setClose(boolean close) {
		this.close = close;
	}

	/**
	 * Configures the {@link Compressor} used to compress the this {@link Region Region's} data.
	 *
	 * @param compressor {@link Compressor} used to compress the this {@link Region Region's} data.
	 * @see org.apache.geode.compression.Compressor
	 */
	public void setCompressor(Compressor compressor) {
		this.compressor = compressor;
	}

	/**
	 * Indicates whether the Region referred to by this factory bean will be destroyed on shutdown (default false).
	 *
	 * @param destroy a boolean value indicating whether the Region is to be destroy on member shutdown.
	 * @see #setDestroy(boolean)
	 */
	public void setDestroy(boolean destroy) {
		this.destroy = destroy;
	}

	/**
	 * Sets the DataPolicy of the Region.
	 *
	 * @param dataPolicy the GemFire DataPolicy to use when configuring the Region.
	 * @since 1.4.0
	 */
	public void setDataPolicy(DataPolicy dataPolicy) {
		this.dataPolicy = dataPolicy;
	}

	/**
	 * Sets the DataPolicy of the Region as a String.
	 *
	 * @param dataPolicyName the name of the DataPolicy (e.g. REPLICATE, PARTITION)
	 * @see #setDataPolicy(org.apache.geode.cache.DataPolicy)
	 * @deprecated as of 1.4.0, use setDataPolicy(:DataPolicy) instead.
	 */
	@Deprecated
	public void setDataPolicy(String dataPolicyName) {
		setDataPolicy(new DataPolicyConverter().convert(dataPolicyName));
	}

	/**
	 * Gets the "resolved" Data Policy as determined by this RegionFactory when configuring the attributes
	 * of the Region to be created.
	 *
	 * @return the "resolved" Data Policy to be used to create the Region.
	 * @see org.apache.geode.cache.DataPolicy
	 */
	public DataPolicy getDataPolicy() {
		return Optional.ofNullable(this.dataPolicy)
			.orElseThrow(() -> newIllegalStateException("Data Policy has not been properly resolved yet"));
	}

	/**
	 * Sets the name of Disk Store used for either overflow or persistence, or both.
	 *
	 * @param diskStoreName the name of the Disk Store bean in context used for overflow/persistence.
	 */
	public void setDiskStoreName(String diskStoreName) {
		this.diskStoreName = diskStoreName;
	}

	public void setEvictionAttributes(EvictionAttributes evictionAttributes) {
		this.evictionAttributes = evictionAttributes;
	}

	/**
	 *
	 * @param gatewaySenders defined as Object for backward compatibility with
	 * Gemfire 6
	 */
	public void setGatewaySenders(GatewaySender[] gatewaySenders) {
		this.gatewaySenders = gatewaySenders;
	}

	/**
	 * Sets whether to enable this {@link Region} to store it's data in off-heap memory.
	 *
	 * @param offHeap Boolean value indicating whether to enable off-heap memory for this Region.
	 * @see org.apache.geode.cache.RegionFactory#setOffHeap(boolean)
	 */
	public void setOffHeap(Boolean offHeap) {
		this.offHeap = offHeap;
	}

	/**
	 * Returns a {@link Boolean} value indicating whether off-heap memory was enabled for this {@link Region}.
	 * Off-heap will be enabled if this method returns a non-{@literal null} {@link Boolean} value that evaluates
	 * to {@literal true}.
	 *
	 * @return a {@link Boolean} value indicating whether off-heap is enabled for this {@link Region}.
	 */
	public Boolean getOffHeap() {
		return this.offHeap;
	}

	/**
	 * Returns a boolean value indicating whether off-heap has been enabled for this {@link Region}.
	 *
	 * @return a {@literal boolean} value indicating whether off-heap has been enabled for this {@link Region}.
	 * @see #getOffHeap()
	 */
	public boolean isOffHeap() {
		return Boolean.TRUE.equals(getOffHeap());
	}

	public void setKeyConstraint(Class<K> keyConstraint) {
		this.keyConstraint = keyConstraint;
	}

	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
	}

	public Scope getScope() {
		return this.scope;
	}

	/**
	 * Sets the region scope. Used only when a new region is created. Overrides
	 * the settings specified through {@link #setAttributes(RegionAttributes)}.
	 *
	 * @see Scope
	 * @param scope the region scope
	 */
	public void setScope(Scope scope) {
		this.scope = scope;
	}

	/**
	 * Configures the Region with a RegionShortcut.
	 *
	 * @param shortcut the RegionShortcut used to configure pre-defined default for the Region created
	 * by this FactoryBean.
	 * @see org.apache.geode.cache.RegionShortcut
	 */
	public void setShortcut(RegionShortcut shortcut) {
		this.shortcut = shortcut;
	}

	protected final RegionShortcut getShortcut() {
		return shortcut;
	}

	public void setValueConstraint(Class<V> valueConstraint) {
		this.valueConstraint = valueConstraint;
	}

	@Override
	@SuppressWarnings("all")
	public void start() {

		if (!ObjectUtils.isEmpty(this.gatewaySenders)) {
			synchronized (this.gatewaySenders) {
				for (Object obj : this.gatewaySenders) {
					GatewaySender gatewaySender = (GatewaySender) obj;
					if (!(gatewaySender.isManualStart() || gatewaySender.isRunning())) {
						gatewaySender.start();
					}
				}
			}
		}

		this.running = true;
	}

	@Override
	public void stop(Runnable callback) {
		stop();
		callback.run();
	}

	@Override
	@SuppressWarnings("all")
	public void stop() {

		if (!ObjectUtils.isEmpty(this.gatewaySenders)) {
			synchronized (this.gatewaySenders) {
				for (GatewaySender gatewaySender : this.gatewaySenders) {
					gatewaySender.stop();
				}
			}
		}

		this.running = false;
	}

	@Override
	public boolean isRunning() {
		return this.running;
	}

	@Override
	public int getPhase() {
		return Integer.MAX_VALUE;
	}

	@Override
	public boolean isAutoStartup() {
		return true;
	}
}
