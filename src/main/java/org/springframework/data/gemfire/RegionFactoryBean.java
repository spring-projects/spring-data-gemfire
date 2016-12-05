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

package org.springframework.data.gemfire;

import java.lang.reflect.Field;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheWriter;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.PartitionAttributes;
import com.gemstone.gemfire.cache.PartitionAttributesFactory;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.RegionShortcut;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.internal.cache.UserSpecifiedRegionAttributes;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.support.RegionShortcutWrapper;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;

/**
 * Base class for FactoryBeans used to create GemFire {@link Region}s. Will try
 * to first locate the region (by name) and, in case none if found, proceed to
 * creating one using the given settings.
 *
 * Note that this factory bean allows for very flexible creation of GemFire
 * {@link Region}. For "client" regions however, see
 * {@link ClientRegionFactoryBean} which offers easier configuration and
 * defaults.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
@SuppressWarnings("unused")
public abstract class RegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V> implements DisposableBean, SmartLifecycle {

	protected final Log log = LogFactory.getLog(getClass());

	private boolean close = true;
	private boolean destroy = false;
	private boolean running;

	private Boolean enableGateway;
	private Boolean persistent;

	private AsyncEventQueue[] asyncEventQueues;

	private CacheListener<K, V>[] cacheListeners;

	private CacheLoader<K, V> cacheLoader;

	private CacheWriter<K, V> cacheWriter;

	private DataPolicy dataPolicy;

	private EvictionAttributes evictionAttributes;

	private GatewaySender[] gatewaySenders;

	private RegionAttributes<K, V> attributes;

	private RegionShortcut shortcut;

	private Resource snapshot;

	private Scope scope;

	private String diskStoreName;
	private String hubId;

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
	@SuppressWarnings({ "unchecked" })
	protected Region<K, V> lookupFallback(GemFireCache gemfireCache, String regionName) throws Exception {
		Assert.isTrue(gemfireCache instanceof Cache, String.format("Unable to create Regions from '%1$s'.",
			gemfireCache));

		Cache cache = (Cache) gemfireCache;

		RegionFactory<K, V> regionFactory = createRegionFactory(cache);

		if (hubId != null) {
			enableGateway = (enableGateway == null || enableGateway);
			Assert.isTrue(enableGateway, "The 'hubId' requires the 'enableGateway' property to be true.");
			regionFactory.setGatewayHubId(hubId);
		}

		if (enableGateway != null) {
			if (enableGateway) {
				Assert.notNull(hubId, "The 'enableGateway' property requires the 'hubId' property to be set.");
			}
			regionFactory.setEnableGateway(enableGateway);
		}

		if (!ObjectUtils.isEmpty(gatewaySenders)) {
			Assert.isTrue(hubId == null, "It is invalid to configure a region with both a hubId and gatewaySenders."
				+ " Note that the enableGateway and hubId properties are deprecated since Gemfire 7.0");

			for (Object gatewaySender : gatewaySenders) {
				regionFactory.addGatewaySenderId(((GatewaySender) gatewaySender).getId());
			}
		}

		for (AsyncEventQueue asyncEventQueue : ArrayUtils.nullSafeArray(asyncEventQueues, AsyncEventQueue.class)) {
			regionFactory.addAsyncEventQueueId(asyncEventQueue.getId());
		}

		for (CacheListener<K, V> listener : ArrayUtils.nullSafeArray(cacheListeners, CacheListener.class)) {
			regionFactory.addCacheListener(listener);
		}

		if (cacheLoader != null) {
			regionFactory.setCacheLoader(cacheLoader);
		}

		if (cacheWriter != null) {
			regionFactory.setCacheWriter(cacheWriter);
		}

		resolveDataPolicy(regionFactory, persistent, dataPolicy);

		if (isDiskStoreConfigurationAllowed()) {
			regionFactory.setDiskStoreName(diskStoreName);
		}

		if (evictionAttributes != null) {
			regionFactory.setEvictionAttributes(evictionAttributes);
		}

		if (scope != null) {
			regionFactory.setScope(scope);
		}

		if (attributes != null) {
			Assert.state(!attributes.isLockGrantor() || (scope == null) || scope.isGlobal(),
				"Lock Grantor only applies to a 'GLOBAL' scoped Region.");
		}

		postProcess(regionFactory);

		Region<K, V> region = (getParent() != null ? regionFactory.createSubregion(getParent(), regionName)
			: regionFactory.create(regionName));

		if (log.isInfoEnabled()) {
			if (getParent() != null) {
				log.info(String.format("Created new Cache sub-Region [%1$s] under parent Region [%2$s].",
					regionName, getParent().getName()));
			}
			else {
				log.info(String.format("Created new Cache Region [%1$s].", regionName));
			}
		}

		if (snapshot != null) {
			region.loadSnapshot(snapshot.getInputStream());
		}

		if (attributes != null && attributes.isLockGrantor()) {
			region.becomeLockGrantor();
		}

		return region;
	}

	/**
	 * Creates an instance of RegionFactory using the given Cache instance used to configure and construct the Region
	 * created by this FactoryBean.
	 *
	 * @param cache the GemFire Cache instance.
	 * @return a RegionFactory used to configure and construct the Region created by this FactoryBean.
	 * @see com.gemstone.gemfire.cache.Cache#createRegionFactory()
	 * @see com.gemstone.gemfire.cache.Cache#createRegionFactory(com.gemstone.gemfire.cache.RegionAttributes)
	 * @see com.gemstone.gemfire.cache.Cache#createRegionFactory(com.gemstone.gemfire.cache.RegionShortcut)
	 * @see com.gemstone.gemfire.cache.RegionFactory
	 */
	protected RegionFactory<K, V> createRegionFactory(Cache cache) {
		if (shortcut != null) {
			RegionFactory<K, V> regionFactory = mergeRegionAttributes(
				cache.<K, V>createRegionFactory(shortcut), attributes);
			setDataPolicy(getDataPolicy(regionFactory));
			return regionFactory;
		}
		else if (attributes != null) {
			return cache.createRegionFactory(attributes);
		}
		else {
			return cache.createRegionFactory();
		}
	}

	/*
	 * (non-Javadoc) - This method should not be considered part of the RegionFactoryBean API
	 * and is strictly for testing purposes!
	 *
	 * NOTE cannot pass RegionAttributes.class as the "targetType" in the second invocation of getFieldValue(..)
	 * since the "regionAttributes" field is naively declared as a instance of the implementation class type
	 * (RegionAttributesImpl) rather than the interface type (RegionAttributes)...
	 * so much for 'programming to interfaces' in GemFire!
	 *
	 * @see com.gemstone.gemfire.cache.RegionFactory#attrsFactory
	 * @see com.gemstone.gemfire.cache.AttributesFactory#regionAttributes
	 * @see com.gemstone.gemfire.cache.RegionAttributes#getDataPolicy
	 * @see com.gemstone.gemfire.cache.DataPolicy
	 */
	@SuppressWarnings({ "deprecation", "unchecked" })
	DataPolicy getDataPolicy(RegionFactory regionFactory) {
		return ((RegionAttributes) getFieldValue(getFieldValue(regionFactory, "attrsFactory", AttributesFactory.class),
			"regionAttributes", null)).getDataPolicy();
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	private <T> T getFieldValue(Object source, String fieldName, Class<T> targetType) {
		Field field = ReflectionUtils.findField(source.getClass(), fieldName, targetType);
		ReflectionUtils.makeAccessible(field);
		return (T) ReflectionUtils.getField(field, source);
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
	 * @see #isUserSpecifiedEvictionAttributes(com.gemstone.gemfire.cache.RegionAttributes)
	 * @see #validateRegionAttributes(com.gemstone.gemfire.cache.RegionAttributes)
	 * @see com.gemstone.gemfire.cache.RegionAttributes
	 * @see com.gemstone.gemfire.cache.RegionFactory
	 */
	@SuppressWarnings("unchecked")
	protected <K, V> RegionFactory<K, V> mergeRegionAttributes(RegionFactory<K, V> regionFactory,
			RegionAttributes<K, V> regionAttributes) {

		if (regionAttributes != null) {
			// NOTE this validation may not be strictly required depending on how the RegionAttributes were "created",
			// but...
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

			// NOTE EvictionAttributes are created by certain RegionShortcuts; need the null check!
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

	/* (non-Javadoc) */
	protected <K, V> void mergePartitionAttributes(final RegionFactory<K, V> regionFactory,
			final RegionAttributes<K, V> regionAttributes) {

		// NOTE PartitionAttributes are created by certain RegionShortcuts; need the null check since RegionAttributes
		// can technically return null!
		// NOTE most likely, the PartitionAttributes will never be null since the PartitionRegionFactoryBean always
		// sets a PartitionAttributesFactoryBean BeanBuilder on the RegionAttributesFactoryBean "partitionAttributes"
		// property.
		if (regionAttributes.getPartitionAttributes() != null) {
			PartitionAttributes partitionAttributes = regionAttributes.getPartitionAttributes();
			PartitionAttributesFactory partitionAttributesFactory = new PartitionAttributesFactory(partitionAttributes);
			RegionShortcutWrapper shortcutWrapper = RegionShortcutWrapper.valueOf(shortcut);

			// NOTE however, since the default value of redundancy is 0, we need to account for 'redundant'
			// RegionShortcut types, which specify a redundancy of 1.
			if (shortcutWrapper.isRedundant() && partitionAttributes.getRedundantCopies() == 0) {
				partitionAttributesFactory.setRedundantCopies(1);
			}

			// NOTE and, since the default value of localMaxMemory is based on the system memory, we need to account for
			// 'proxy' RegionShortcut types, which specify a local max memory of 0.
			if (shortcutWrapper.isProxy()) {
				partitionAttributesFactory.setLocalMaxMemory(0);
			}

			// NOTE internally, RegionFactory.setPartitionAttributes handles merging the PartitionAttributes, hooray!
			regionFactory.setPartitionAttributes(partitionAttributesFactory.create());
		}
	}

	/*
	 * (non-Javadoc) - This method should not be considered part of the RegionFactoryBean API
	 * and is strictly for testing purposes!
	 *
	 * @see com.gemstone.gemfire.cache.AttributesFactory#validateAttributes(:RegionAttributes)
	 */
	@SuppressWarnings("deprecation")
	void validateRegionAttributes(RegionAttributes regionAttributes) {
		com.gemstone.gemfire.cache.AttributesFactory.validateAttributes(regionAttributes);
	}

	/*
	 * (non-Javadoc) - This method should not be considered part of the RegionFactoryBean API
	 * and is strictly for testing purposes!
	 *
	 * NOTE unfortunately, must resort to using a GemFire internal class, ugh!
	 * @see com.gemstone.gemfire.internal.cache.UserSpecifiedRegionAttributes#hasEvictionAttributes
	 */
	boolean isUserSpecifiedEvictionAttributes(final RegionAttributes regionAttributes) {
		return (regionAttributes instanceof UserSpecifiedRegionAttributes
			&& ((UserSpecifiedRegionAttributes) regionAttributes).hasEvictionAttributes());
	}

	/* (non-Javadoc) */
	private boolean isDiskStoreConfigurationAllowed() {
		boolean allow = (diskStoreName != null);

		allow &= (getDataPolicy().withPersistence() || (getAttributes() != null
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
	 * @see #isPersistentUnspecified()
	 */
	protected boolean isPersistent() {
		return Boolean.TRUE.equals(persistent);
	}

	/**
	 * Determines whether the user explicitly set the 'persistent' attribute or not.
	 *
	 * @return a boolean value indicating whether the user explicitly set the 'persistent' attribute to true or false.
	 * @see #isPersistent()
	 * @see #isNotPersistent()
	 */
	protected boolean isPersistentUnspecified() {
		return (persistent == null);
	}

	/**
	 * Returns true when the user explicitly specified a value for the persistent attribute and it is false.  If the
	 * persistent attribute was not explicitly specified, then the persistence setting is implicitly undefined
	 * and will be determined by the Data Policy.
	 *
	 * @return true when the user specified an explicit value for the persistent attribute and it is false;
	 * false otherwise.
	 * @see #isPersistent()
	 * @see #isPersistentUnspecified()
	 */
	protected boolean isNotPersistent() {
		return Boolean.FALSE.equals(persistent);
	}

	/**
	 * Validates that the settings for Data Policy and the 'persistent' attribute in &lt;gfe:*-region&gt; elements
	 * are compatible.
	 *
	 * @param resolvedDataPolicy the GemFire Data Policy resolved form the Spring GemFire XML namespace configuration
	 * meta-data.
	 * @see #isPersistent()
	 * @see #isNotPersistent()
	 * @see com.gemstone.gemfire.cache.DataPolicy
	 */
	protected void assertDataPolicyAndPersistentAttributesAreCompatible(DataPolicy resolvedDataPolicy) {
		if (resolvedDataPolicy.withPersistence()) {
			Assert.isTrue(isPersistentUnspecified() || isPersistent(), String.format(
				"Data Policy '%1$s' is invalid when persistent is false.", resolvedDataPolicy));
		}
		else {
			// NOTE otherwise, the Data Policy is not persistent, so...
			Assert.isTrue(isPersistentUnspecified() || isNotPersistent(), String.format(
				"Data Policy '%1$s' is invalid when persistent is true.", resolvedDataPolicy));
		}
	}

	/**
	 * Post-process the RegionFactory used to create the GemFire Region for this factory bean during the initialization
	 * process.  The RegionFactory is already configured and initialized by the factory bean before this method
	 * is invoked.
	 *
	 * @param regionFactory the GemFire RegionFactory used to create the Region for post-processing.
	 * @see com.gemstone.gemfire.cache.RegionFactory
	 */
	protected RegionFactory<K, V> postProcess(RegionFactory<K, V> regionFactory) {
		return regionFactory;
	}

	/**
	 * Post-process the Region for this factory bean during the initialization process. The Region is
	 * already configured and initialized by the factory bean before this method is invoked.
	 *
	 * @param region the GemFire Region to post-process.
	 * @see com.gemstone.gemfire.cache.Region
	 */
	protected Region<K, V> postProcess(Region<K, V> region) {
		return region;
	}

	/**
	 * Validates and sets the Data Policy on the RegionFactory used to create and configure the Region from this
	 * FactoryBean.
	 *
	 * @param regionFactory the RegionFactory used by this FactoryBean to create and configure the Region.
	 * @param persistent a boolean value indicating whether the Region should be persistent and persist it's
	 * data to disk.
	 * @param dataPolicy the configured Data Policy for the Region.
	 * @see #resolveDataPolicy(com.gemstone.gemfire.cache.RegionFactory, Boolean, String)
	 * @see com.gemstone.gemfire.cache.DataPolicy
	 * @see com.gemstone.gemfire.cache.RegionFactory
	 */
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, DataPolicy dataPolicy) {
		if (dataPolicy != null) {
			assertDataPolicyAndPersistentAttributesAreCompatible(dataPolicy);
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
	 * @see com.gemstone.gemfire.cache.DataPolicy
	 * @see com.gemstone.gemfire.cache.RegionFactory
	 */
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, String dataPolicy) {
		if (dataPolicy != null) {
			DataPolicy resolvedDataPolicy = new DataPolicyConverter().convert(dataPolicy);

			Assert.notNull(resolvedDataPolicy, String.format("Data Policy '%1$s' is invalid.", dataPolicy));
			assertDataPolicyAndPersistentAttributesAreCompatible(resolvedDataPolicy);

			regionFactory.setDataPolicy(resolvedDataPolicy);
			setDataPolicy(resolvedDataPolicy);
		}
		else {
			DataPolicy regionAttributesDataPolicy = getDataPolicy(getAttributes(), DataPolicy.DEFAULT);
			DataPolicy resolvedDataPolicy = (isPersistent() && DataPolicy.DEFAULT.equals(regionAttributesDataPolicy)
				? DataPolicy.PERSISTENT_REPLICATE : regionAttributesDataPolicy);

			assertDataPolicyAndPersistentAttributesAreCompatible(resolvedDataPolicy);

			regionFactory.setDataPolicy(resolvedDataPolicy);
			setDataPolicy(resolvedDataPolicy);
		}
	}

	/* (non-Javadoc) */
	private DataPolicy getDataPolicy(final RegionAttributes regionAttributes, final DataPolicy defaultDataPolicy) {
		return (regionAttributes != null ? regionAttributes.getDataPolicy() : defaultDataPolicy);
	}

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
	 * @see com.gemstone.gemfire.cache.RegionAttributes
	 */
	public RegionAttributes getAttributes() {
		Region<K, V> region = getRegion();
		return (region != null ? region.getAttributes() : attributes);
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
	 * @see #setDataPolicy(com.gemstone.gemfire.cache.DataPolicy)
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
	 * @see com.gemstone.gemfire.cache.DataPolicy
	 */
	public DataPolicy getDataPolicy() {
		Assert.state(dataPolicy != null, "The Data Policy has not been properly resolved yet!");
		return dataPolicy;
	}

	/**
	 * Sets the name of Disk Store used for either overflow or persistence, or both.
	 *
	 * @param diskStoreName the name of the Disk Store bean in context used for overflow/persistence.
	 */
	public void setDiskStoreName(String diskStoreName) {
		this.diskStoreName = diskStoreName;
	}

	public void setEnableGateway(boolean enableGateway) {
		this.enableGateway = enableGateway;
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

	public void setHubId(String hubId) {
		this.hubId = hubId;
	}

	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
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
	 * @see com.gemstone.gemfire.cache.RegionShortcut
	 */
	public void setShortcut(RegionShortcut shortcut) {
		this.shortcut = shortcut;
	}

	/* (non-Javadoc) */
	protected final RegionShortcut getShortcut() {
		return shortcut;
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
	 * @inheritDoc
	 */
	@Override
	@SuppressWarnings("all")
	public void start() {
		if (!ObjectUtils.isEmpty(gatewaySenders)) {
			synchronized (gatewaySenders) {
				for (Object obj : gatewaySenders) {
					GatewaySender gatewaySender = (GatewaySender) obj;
					if (!(gatewaySender.isManualStart() || gatewaySender.isRunning())) {
						gatewaySender.start();
					}
				}
			}
		}

		this.running = true;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void stop(Runnable callback) {
		stop();
		callback.run();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void stop() {
		if (!ObjectUtils.isEmpty(gatewaySenders)) {
			synchronized (gatewaySenders) {
				for (Object gatewaySender : gatewaySenders) {
					((GatewaySender) gatewaySender).stop();
				}
			}
		}

		this.running = false;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean isRunning() {
		return this.running;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public int getPhase() {
		return Integer.MAX_VALUE;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean isAutoStartup() {
		return true;
	}
}
