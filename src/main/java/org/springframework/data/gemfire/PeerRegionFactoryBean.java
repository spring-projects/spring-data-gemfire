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
package org.springframework.data.gemfire;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.geode.cache.AttributesFactory;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheWriter;
import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.ExpirationAttributes;
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

import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.eviction.EvictingRegionFactoryBean;
import org.springframework.data.gemfire.expiration.ExpiringRegionFactoryBean;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.data.gemfire.util.RegionUtils;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} and abstract base class extended by other SDG {@link FactoryBean FactoryBeans}
 * used to construct, configure and initialize {@literal peer} {@link Region Regions}.
 *
 * This {@link FactoryBean} allows for very easy and flexible creation of {@literal peer} {@link Region Regions}.
 * For {@literal client} {@link Region Regions}, see the {@link ClientRegionFactoryBean}.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.CacheListener
 * @see org.apache.geode.cache.CacheLoader
 * @see org.apache.geode.cache.CacheWriter
 * @see org.apache.geode.cache.CustomExpiry
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.DiskStore
 * @see org.apache.geode.cache.EvictionAttributes
 * @see org.apache.geode.cache.ExpirationAttributes
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.PartitionAttributes
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionAttributes
 * @see org.apache.geode.cache.RegionFactory
 * @see org.apache.geode.cache.RegionShortcut
 * @see org.apache.geode.cache.Scope
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.apache.geode.compression.Compressor
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.context.SmartLifecycle
 * @see org.springframework.data.gemfire.ConfigurableRegionFactoryBean
 * @see org.springframework.data.gemfire.ResolvableRegionFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.eviction.EvictingRegionFactoryBean
 * @see org.springframework.data.gemfire.expiration.ExpiringRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
 */
@SuppressWarnings("unused")
public abstract class PeerRegionFactoryBean<K, V> extends ConfigurableRegionFactoryBean<K, V>
		implements DisposableBean, EvictingRegionFactoryBean, ExpiringRegionFactoryBean<K, V>, SmartLifecycle {

	private boolean close = false;
	private boolean destroy = false;
	private boolean running = false;

	private Boolean offHeap;
	private Boolean persistent;
	private Boolean statisticsEnabled;

	private CacheListener<K, V>[] cacheListeners;

	private CacheLoader<K, V> cacheLoader;

	private CacheWriter<K, V> cacheWriter;

	private Class<K> keyConstraint;
	private Class<V> valueConstraint;

	private Compressor compressor;

	private CustomExpiry<K, V> customEntryIdleTimeout;
	private CustomExpiry<K, V> customEntryTimeToLive;

	private DataPolicy dataPolicy;

	private EvictionAttributes evictionAttributes;

	private ExpirationAttributes entryIdleTimeout;
	private ExpirationAttributes entryTimeToLive;
	private ExpirationAttributes regionIdleTimeout;
	private ExpirationAttributes regionTimeToLive;

	private List<AsyncEventQueue> asyncEventQueues = new ArrayList<>();
	private List<GatewaySender> gatewaySenders = new ArrayList<>();
	private List<String> asyncEventQueueIds = new ArrayList<>();
	private List<String> gatewaySenderIds = new ArrayList<>();

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
	protected Region<K, V> createRegion(GemFireCache gemfireCache, String regionName) {

		applyRegionConfigurers(regionName);

		verifyLockGrantorEligibility(getAttributes(), getScope());

		Cache cache = resolveCache(gemfireCache);

		RegionFactory<K, V> regionFactory = postProcess(configure(createRegionFactory(cache)));

		Region<K, V> region = newRegion(regionFactory, getParent(), regionName);

		region = becomeLockGrantor(region);

		return region;
	}

	private Region<K, V> becomeLockGrantor(Region<K, V> region) {

		if (isLockGrantor(region)) {
			region.becomeLockGrantor();
		}

		return region;
	}

	private boolean isLockGrantor(@Nullable Region<K, V> region) {
		return region != null && region.getAttributes() != null && region.getAttributes().isLockGrantor();
	}

	private Region<K, V> newRegion(RegionFactory<K, V> regionFactory, Region<?, ?> parentRegion, String regionName) {

		if (parentRegion != null) {

			logInfo("Creating Subregion [%1$s] with parent Region [%2$s]", regionName, parentRegion.getName());

			return regionFactory.createSubregion(parentRegion, regionName);
		}
		else {

			logInfo("Created Region [%s]", regionName);

			return regionFactory.create(regionName);
		}
	}

	private Cache resolveCache(GemFireCache gemfireCache) {

		return Optional.ofNullable(gemfireCache)
			.filter(Cache.class::isInstance)
			.map(Cache.class::cast)
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
	 * configure and initialize the {@link Region} specified by this {@link PeerRegionFactoryBean}.
	 *
	 * @param cache reference to the {@link Cache}.
	 * @return a {@link RegionFactory} used to construct, configure and initialized the {@link Region} specified by
	 * this {@link PeerRegionFactoryBean}.
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
	 * Configures the {@link RegionFactory} based on the configuration settings of this {@link PeerRegionFactoryBean}.
	 *
	 * @param regionFactory {@link RegionFactory} to configure
	 * @return the given {@link RegionFactory}.
	 * @see org.apache.geode.cache.RegionFactory
	 */
	protected RegionFactory<K, V> configure(RegionFactory<K, V> regionFactory) {

		regionFactory.setStatisticsEnabled(resolveStatisticsEnabled());

		getConfiguredAsyncEventQueueIds().forEach(regionFactory::addAsyncEventQueueId);

		Arrays.stream(ArrayUtils.nullSafeArray(this.cacheListeners, CacheListener.class))
			.forEach(regionFactory::addCacheListener);

		Optional.ofNullable(this.cacheLoader).ifPresent(regionFactory::setCacheLoader);

		Optional.ofNullable(this.cacheWriter).ifPresent(regionFactory::setCacheWriter);

		Optional.ofNullable(this.compressor).ifPresent(regionFactory::setCompressor);

		Optional.ofNullable(this.customEntryIdleTimeout).ifPresent(regionFactory::setCustomEntryIdleTimeout);

		Optional.ofNullable(this.customEntryTimeToLive).ifPresent(regionFactory::setCustomEntryTimeToLive);

		resolveDataPolicy(regionFactory, this.persistent, this.dataPolicy);

		Optional.ofNullable(this.diskStoreName)
			.filter(name -> isDiskStoreConfigurationAllowed())
			.ifPresent(regionFactory::setDiskStoreName);

		Optional.ofNullable(this.entryIdleTimeout).ifPresent(regionFactory::setEntryIdleTimeout);

		Optional.ofNullable(this.entryTimeToLive).ifPresent(regionFactory::setEntryTimeToLive);

		Optional.ofNullable(this.evictionAttributes).ifPresent(regionFactory::setEvictionAttributes);

		getConfiguredGatewaySenderIds().forEach(regionFactory::addGatewaySenderId);

		Optional.ofNullable(this.keyConstraint).ifPresent(regionFactory::setKeyConstraint);

		Optional.ofNullable(this.offHeap).ifPresent(regionFactory::setOffHeap);

		Optional.ofNullable(this.regionIdleTimeout).ifPresent(regionFactory::setRegionIdleTimeout);

		Optional.ofNullable(this.regionTimeToLive).ifPresent(regionFactory::setRegionTimeToLive);

		Optional.ofNullable(getScope()).ifPresent(regionFactory::setScope);

		Optional.ofNullable(this.valueConstraint).ifPresent(regionFactory::setValueConstraint);

		return regionFactory;
	}

	/**
	 * Post-process the {@link RegionFactory} used to create the {@link Region} specified by
	 * this {@link PeerRegionFactoryBean} during initialization.
	 *
	 * The {@link RegionFactory} has been already constructed, configured and initialized by
	 * this {@link PeerRegionFactoryBean} before this method gets invoked.
	 *
	 * @param regionFactory {@link RegionFactory} used to create the {@link Region}.
	 * @return the given {@link RegionFactory}.
	 * @see org.apache.geode.cache.RegionFactory
	 */
	protected RegionFactory<K, V> postProcess(RegionFactory<K, V> regionFactory) {
		return regionFactory;
	}

	private Set<String> getConfiguredAsyncEventQueueIds() {

		Set<String> asyncEventQueueIds = new HashSet<>();

		CollectionUtils.nullSafeList(this.asyncEventQueues).stream()
			.filter(Objects::nonNull)
			.map(AsyncEventQueue::getId)
			.collect(Collectors.toCollection(() -> asyncEventQueueIds));

		CollectionUtils.nullSafeList(this.asyncEventQueueIds).stream()
			.filter(StringUtils::hasText)
			.map(String::trim)
			.collect(Collectors.toCollection(() -> asyncEventQueueIds));

		return asyncEventQueueIds;
	}

	private Set<String> getConfiguredGatewaySenderIds() {

		Set<String> gatewaySenderIds = new HashSet<>();

		CollectionUtils.nullSafeList(this.gatewaySenders).stream()
			.filter(Objects::nonNull)
			.map(GatewaySender::getId)
			.collect(Collectors.toCollection(() -> gatewaySenderIds));

		CollectionUtils.nullSafeList(this.gatewaySenderIds).stream()
			.filter(StringUtils::hasText)
			.map(String::trim)
			.collect(Collectors.toCollection(() -> gatewaySenderIds));

		return gatewaySenderIds;
	}

	/*
	 * This method is not considered part of the PeerRegionFactoryBean API and is strictly used for testing purposes!
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
	@SuppressWarnings({ "deprecation", "rawtypes", "unchecked" })
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
	 * of this PeerRegionFactoryBean.
	 * @param regionAttributes the RegionAttributes containing the Region configuration settings to merge to the
	 * RegionFactory.
	 * @return the RegionFactory with the configuration settings of the RegionAttributes merged.
	 * @see #isUserSpecifiedEvictionAttributes(org.apache.geode.cache.RegionAttributes)
	 * @see #validateRegionAttributes(org.apache.geode.cache.RegionAttributes)
	 * @see org.apache.geode.cache.RegionAttributes
	 * @see org.apache.geode.cache.RegionFactory
	 */
	protected <K, V> RegionFactory<K, V> mergeRegionAttributes(RegionFactory<K, V> regionFactory,
			RegionAttributes<K, V> regionAttributes) {

		if (regionAttributes != null) {

			// NOTE: this validation may not be strictly necessary depending on how the RegionAttributes were "created".
			validateRegionAttributes(regionAttributes);

			CollectionUtils.nullSafeSet(regionAttributes.getAsyncEventQueueIds()).stream()
				.filter(StringUtils::hasText)
				.forEach(regionFactory::addAsyncEventQueueId);

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

			// NOTE: EvictionAttributes are created by certain RegionShortcuts; null check needed!
			if (isUserSpecifiedEvictionAttributes(regionAttributes)) {
				regionFactory.setEvictionAttributes(regionAttributes.getEvictionAttributes());
			}

			CollectionUtils.nullSafeSet(regionAttributes.getGatewaySenderIds()).stream()
				.filter(StringUtils::hasText)
				.forEach(regionFactory::addGatewaySenderId);

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
	 * Merges the {@link RegionAttributes} into the {@link RegionFactory}.
	 *
	 * @param regionFactory {@link RegionFactory} to configure.
	 * @param regionAttributes {@link RegionAttributes} used to configure the {@link RegionFactory}
	 * if not {@literal null}.
	 * @see org.apache.geode.cache.RegionAttributes
	 * @see org.apache.geode.cache.RegionFactory
	 */
	@SuppressWarnings("rawtypes")
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

	private boolean isDiskStoreConfigurationAllowed() {

		boolean allow = StringUtils.hasText(this.diskStoreName);

		allow &= getDataPolicy().withPersistence()
			|| (getAttributes() != null && getAttributes().getEvictionAttributes() != null
				&& EvictionAction.OVERFLOW_TO_DISK.equals(this.attributes.getEvictionAttributes().getAction()));

		return allow;
	}

	/*
	 * This method is not part of the PeerRegionFactoryBean API and is strictly used for testing purposes!
	 *
<<<<<<< HEAD
	 * NOTE unfortunately, must resort to using a Pivotal GemFire internal class, ugh!
	 *
=======
>>>>>>> f7643fe4a... DATAGEODE-368 - Add API to attach additional AsyncEventQueues and GatewaySenders to peer Regions.
	 * @see org.apache.geode.internal.cache.UserSpecifiedRegionAttributes#hasEvictionAttributes
	 */
	boolean isUserSpecifiedEvictionAttributes(RegionAttributes<?, ?> regionAttributes) {

		SpringUtils.ValueReturningThrowableOperation<Boolean> hasEvictionAttributes = () ->
			Optional.ofNullable(regionAttributes)
				.map(Object::getClass)
				.map(type -> ReflectionUtils.findMethod(type, "hasEvictionAttributes"))
				.map(method -> ReflectionUtils.invokeMethod(method, regionAttributes))
				.map(Boolean.TRUE::equals)
				.orElse(false);

		return SpringUtils.safeGetValue(hasEvictionAttributes, false);
	}

	/*
	 * This method is not part of the PeerRegionFactoryBean API and is strictly used for testing purposes!
	 *
	 * @see org.apache.geode.cache.AttributesFactory#validateAttributes(:RegionAttributes)
	 */
	@SuppressWarnings({ "deprecation", "rawtypes" })
	void validateRegionAttributes(RegionAttributes regionAttributes) {
		org.apache.geode.cache.AttributesFactory.validateAttributes(regionAttributes);
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
		return Boolean.FALSE.equals(this.persistent);
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
		return Boolean.TRUE.equals(this.persistent);
	}

	/**
	 * Validates and sets the {@link DataPolicy} on the {@link RegionFactory} used to create and configure
	 * the {@link Region} from this {@link FactoryBean}.
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
	 * @param regionFactory the Pivotal GemFire RegionFactory used to create the desired Region.
	 * @param persistent a boolean value indicating whether the Region should persist it's data to disk.
	 * @param dataPolicy requested Data Policy as set by the user in the Spring Pivotal GemFire configuration meta-data.
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
				? DataPolicy.PERSISTENT_REPLICATE
				: regionAttributesDataPolicy;

			RegionUtils.assertDataPolicyAndPersistentAttributeAreCompatible(resolvedDataPolicy, this.persistent);

			regionFactory.setDataPolicy(resolvedDataPolicy);
			setDataPolicy(resolvedDataPolicy);
		}
	}

	@SuppressWarnings("rawtypes")
	private DataPolicy getDataPolicy(RegionAttributes regionAttributes, DataPolicy defaultDataPolicy) {

		return Optional.ofNullable(regionAttributes)
			.map(RegionAttributes::getDataPolicy)
			.orElse(defaultDataPolicy);
	}

	/**
	 * Closes and destroys this {@link Region}.
	 *
	 * @throws Exception if {@code destroy()} fails.
	 * @see org.springframework.beans.factory.DisposableBean
	 * @see org.apache.geode.cache.Region#close()
	 * @see org.apache.geode.cache.Region#destroyRegion()
	 */
	@Override
	public void destroy() throws Exception {

		Optional.ofNullable(getObject()).ifPresent(region -> {

			if (this.close && RegionUtils.isCloseable(region)) {
				RegionUtils.close(region);
			}

			if (this.destroy) {
				region.destroyRegion();
			}
		});
	}

	/**
	 * Configures an array of {@link AsyncEventQueue AsyncEventQueues} for this {@link Region}, which are used
	 * to perform asynchronous data access operations, e.g. {@literal asynchronous, write-behind operations}.
	 *
	 * This method clears any existing, registered {@link AsyncEventQueue AsyncEventQueues} (AEQ) already associated
	 * with this {@link Region}. Use {@link #addAsyncEventQueues(AsyncEventQueue[])}
	 * or {@link #addAsyncEventQueueIds(String[])} to append to the existing AEQs already registered instead.
	 *
	 * @param asyncEventQueues array of {@link AsyncEventQueue AsyncEventQueues} registered with and used by
	 * this {@link Region} to perform asynchronous data access operations.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
	 * @see #addAsyncEventQueues(AsyncEventQueue[])
	 * @see #addAsyncEventQueueIds(String[])
	 * @see #setAsyncEventQueueIds(String[])
	 */
	public void setAsyncEventQueues(@NonNull AsyncEventQueue[] asyncEventQueues) {
		this.asyncEventQueues.clear();
		addAsyncEventQueues(asyncEventQueues);
	}

	/**
	 * Configures an array of {@link AsyncEventQueue AsyncEventQueues} (AEQ) for this {@link Region}
	 * by {@link String AEQ ID}.
	 *
	 * This method clears any existing, registered {@link AsyncEventQueue AsyncEventQueues} (AEQ) already associated
	 * with this {@link Region} by {@literal AEQ ID}. Use {@link #addAsyncEventQueues(AsyncEventQueue[])}
	 * or {@link #addAsyncEventQueueIds(String[])} to append to the existing AEQs already registered instead.
	 *
	 * or {@link #addAsyncEventQueueIds(String[])} to append to the existing AEQs already registered instead.
	 * @param asyncEventQueueIds array of {@link String Strings} specifying {@link String AEQ IDs} to be registered
	 * with this {@link Region}.
	 * @see #addAsyncEventQueues(AsyncEventQueue[])
	 * @see #setAsyncEventQueues(AsyncEventQueue[])
	 * @see #setAsyncEventQueueIds(String[])
	 */
	public void setAsyncEventQueueIds(@NonNull String[] asyncEventQueueIds) {
		this.asyncEventQueueIds.clear();
		addAsyncEventQueueIds(asyncEventQueueIds);
	}

	/**
	 * Registers the array of {@link AsyncEventQueue AsyncEventQueues} (AEQ) with this {@link Region} by appending to
	 * the already existing, registered AEQs for this {@link Region}.
	 *
	 * @param asyncEventQueues array of {@link AsyncEventQueue AsyncEventQueues} to register with this {@link Region}.
	 * @see #addAsyncEventQueueIds(String[])
	 * @see #setAsyncEventQueues(AsyncEventQueue[])
	 * @see #setAsyncEventQueueIds(String[])
	 */
	public void addAsyncEventQueues(@NonNull AsyncEventQueue[] asyncEventQueues) {

		Arrays.stream(ArrayUtils.nullSafeArray(asyncEventQueues, AsyncEventQueue.class))
			.filter(Objects::nonNull)
			.forEach(this.asyncEventQueues::add);
	}

	/**
	 * Registers the array of {@link AsyncEventQueue AsyncEventQueues} (AEQ) with this {@link Region}
	 * by {@link String ID} by appending to the already existing, registered AEQs for this {@link Region}.
	 *
	 * @param asyncEventQueueIds array of {@link AsyncEventQueue AsyncEventQueue} {@link String IDs} to register with
	 * this {@link Region}.
	 * @see #addAsyncEventQueues(AsyncEventQueue[])
	 * @see #setAsyncEventQueues(AsyncEventQueue[])
	 * @see #setAsyncEventQueueIds(String[])
	 */
	public void addAsyncEventQueueIds(@NonNull String[] asyncEventQueueIds) {

		Arrays.stream(ArrayUtils.nullSafeArray(asyncEventQueueIds, String.class))
			.filter(StringUtils::hasText)
			.forEach(this.asyncEventQueueIds::add);
	}

	/**
	 * Sets the {@link RegionAttributes} used to configure this {@link Region}.
	 *
	 * Specifying {@link RegionAttributes} allows full control in configuring various {@link Region} settings.
	 * Used only when the {@link Region} is created and not when the {@link Region} is looked up.
	 *
	 * @param attributes {@link RegionAttributes} used to configure this {@link Region}.
	 * @see org.apache.geode.cache.RegionAttributes
	 */
	public void setAttributes(RegionAttributes<K, V> attributes) {
		this.attributes = attributes;
	}

	/**
	 * Returns the {@link RegionAttributes} used to configure this {@link Region}.
	 *
	 * @return the {@link RegionAttributes} used to configure this {@link Region}.
	 * @see org.apache.geode.cache.RegionAttributes
	 */
	public RegionAttributes<K, V> getAttributes() {

		return Optional.ofNullable(getRegion())
			.map(Region::getAttributes)
			.orElse(this.attributes);
	}

	/**
	 * Configures {@link CacheListener CacheListeners} used to listen for entry events on this {@link Region}.
	 *
	 * Used only when a new {@link Region} is created and not {@link #isLookupEnabled() looked up}.
	 *
	 * Overrides the {@link Region} settings specified in {@link RegionAttributes}
	 * set with {@link #setAttributes(RegionAttributes)}.
	 *
	 * @param cacheListeners array {@link CacheListener CacheListeners} to register with this {@link Region}.
	 * @see org.apache.geode.cache.CacheListener
	 */
	public void setCacheListeners(CacheListener<K, V>[] cacheListeners) {
		this.cacheListeners = cacheListeners;
	}

	/**
	 * Configures the {@link CacheLoader} used by this {@link Region} to perform {@literal synchronous read-through}
	 * data access operations to an underlying, external data source.
	 *
	 * Used only when a new {@link Region} is created and not {@link #isLookupEnabled() looked up}.
	 *
	 * Overrides the {@link Region} settings specified in {@link RegionAttributes}
	 * set with {@link #setAttributes(RegionAttributes)}.
	 *
	 * @param cacheLoader {@link CacheLoader} to register for this {@link Region}.
	 * @see org.apache.geode.cache.CacheLoader
	 */
	public void setCacheLoader(CacheLoader<K, V> cacheLoader) {
		this.cacheLoader = cacheLoader;
	}

	/**
	 * Configures the {@link CacheWriter} used by this {@link Region} to perform {@literal synchronous write-through}
	 * data access operations to an underlying, external data source.
	 *
	 * Used only when a new {@link Region} is created and not {@link #isLookupEnabled() looked up}.
	 *
	 * Overrides the {@link Region} settings specified in {@link RegionAttributes}
	 * set with {@link #setAttributes(RegionAttributes)}.
	 *
	 * @param cacheWriter {@link CacheWriter} to register for this {@link Region}.
	 * @see org.apache.geode.cache.CacheWriter
	 */
	public void setCacheWriter(CacheWriter<K, V> cacheWriter) {
		this.cacheWriter = cacheWriter;
	}

	/**
	 * Configure whether to close this {@literal Region} during shutdown.
	 *
	 * Defaults to {@literal true}.
	 *
	 * @param close boolean value indicating whether this {@link Region} should be closed during shutdown.
	 * @see #setDestroy(boolean)
	 */
	public void setClose(boolean close) {
		this.close = close;
	}

	/**
	 * Configures the {@link Compressor} used to compress this {@link Region Region's} data.
	 *
	 * @param compressor {@link Compressor} used to compress this {@link Region Region's} data.
	 * @see org.apache.geode.compression.Compressor
	 */
	public void setCompressor(Compressor compressor) {
		this.compressor = compressor;
	}

	public void setCustomEntryIdleTimeout(CustomExpiry<K, V> customEntryIdleTimeout) {
		this.customEntryIdleTimeout = customEntryIdleTimeout;
	}

	public void setCustomEntryTimeToLive(CustomExpiry<K, V> customEntryTimeToLive) {
		this.customEntryTimeToLive = customEntryTimeToLive;
	}

	/**
	 * Configure whether to destroy this {@link Region} during shutdown.
	 *
	 * Defaults to {@literal false}.
	 *
	 * @param destroy value indicating whether this {@link Region} should be destroyed during shutdown.
	 * @see #setClose(boolean)
	 */
	public void setDestroy(boolean destroy) {
		this.destroy = destroy;
	}

	/**
	 * Configure the {@link DataPolicy} for this {@link Region}.
	 *
	 * @param dataPolicy {@link DataPolicy} used when configuring this {@link Region}.
	 * @see org.apache.geode.cache.DataPolicy
	 * @since 1.4.0
	 */
	public void setDataPolicy(DataPolicy dataPolicy) {
		this.dataPolicy = dataPolicy;
	}

	/**
	 * Returns resolved {@link DataPolicy} as configured with the {@link RegionFactory}
	 * when creating this {@link Region}.
	 *
	 * @return the configured, resolved {@link DataPolicy} used by this {@link Region}.
	 * @throws IllegalStateException if the {@link DataPolicy} has not been configured
	 * or is not resolvable.
	 * @see org.apache.geode.cache.DataPolicy
	 */
	public DataPolicy getDataPolicy() {

		Assert.state(this.dataPolicy != null, "Data Policy has not been properly resolved yet");

		return this.dataPolicy;
	}

	/**
	 * Configures the {@link String name} of the {@link DiskStore} used by this {@link Region}
	 * for overflow and/or persistence.
	 *
	 * @param diskStoreName {@link String} containing the name of the {@link DiskStore} bean
	 * configured for this {@link Region}.
	 */
	public void setDiskStoreName(String diskStoreName) {
		this.diskStoreName = diskStoreName;
	}

	public void setEntryIdleTimeout(ExpirationAttributes entryIdleTimeout) {
		this.entryIdleTimeout = entryIdleTimeout;
	}

	public void setEntryTimeToLive(ExpirationAttributes entryTimeToLive) {
		this.entryTimeToLive = entryTimeToLive;
	}

	public void setEvictionAttributes(EvictionAttributes evictionAttributes) {
		this.evictionAttributes = evictionAttributes;
	}

	/**
	 * Configures the {@link GatewaySender GatewaySenders} used to send data and events from this {@link Region}
	 * to a matching {@link Region} in a remote cluster.
	 *
	 * This method clears all existing, registered {@link GatewaySender GatewaySenders} already associated with
	 * this {@link Region}. Use {@link #addGatewaySenders(GatewaySender[])}
	 * or {@link #addGatewaySendersIds(String[])} to append to the existing, registered
	 * {@link GatewaySender GatewaySenders} for this {@link Region}.
	 *
	 * @param gatewaySenders {@link GatewaySender GatewaySenders} used to send data and events from this {@link Region}
	 * to a matching {@link Region} in a remote cluster.
	 * @see org.apache.geode.cache.wan.GatewaySender
	 * @see #addGatewaySenders(GatewaySender[])
	 * @see #addGatewaySendersIds(String[])
	 * @see #setGatewaySenderIds(String[])
	 */
	public void setGatewaySenders(@NonNull GatewaySender[] gatewaySenders) {
		this.gatewaySenders.clear();
		addGatewaySenders(gatewaySenders);
	}

	/**
	 * Configures the {@link GatewaySender GatewaySenders} by {@link String ID} used to send data and events from
	 * this {@link Region} to a matching {@link Region} in a remote cluster.
	 *
	 * This method clears all existing, registered {@link GatewaySender GatewaySenders} already associated with
	 * this {@link Region}. Use {@link #addGatewaySenders(GatewaySender[])}
	 * or {@link #addGatewaySendersIds(String[])} to append to the existing, registered
	 * {@link GatewaySender GatewaySenders} for this {@link Region}.
	 *
	 * @param gatewaySenderIds {@link String} array containing {@link GatewaySender} {@link String IDs} to register
	 * with this {@link Region}.
	 * @see #addGatewaySenders(GatewaySender[])
	 * @see #addGatewaySendersIds(String[])
	 * @see #setGatewaySenders(GatewaySender[])
	 */
	public void setGatewaySenderIds(@NonNull String[] gatewaySenderIds) {
		this.gatewaySenderIds.clear();
		addGatewaySendersIds(gatewaySenderIds);
	}

	/**
	 * Registers the array of {@link GatewaySender GatewaySenders} with this {@link Region} by appending to the already
	 * existing, registered {@link GatewaySender GatewaySenders} for this {@link Region}.
	 *
	 * @param gatewaySenders array of {@link GatewaySender GatewaySenders} to register with this {@link Region}.
	 * @see org.apache.geode.cache.wan.GatewaySender
	 * @see #addGatewaySendersIds(String[])
	 * @see #setGatewaySenders(GatewaySender[])
	 * @see #setGatewaySenderIds(String[])
	 */
	public void addGatewaySenders(@NonNull GatewaySender[] gatewaySenders) {

		Arrays.stream(ArrayUtils.nullSafeArray(gatewaySenders, GatewaySender.class))
			.filter(Objects::nonNull)
			.forEach(this.gatewaySenders::add);
	}

	/**
	 * Registers the array of {@link GatewaySender} {@link String IDs} with this {@link Region} by appending to
	 * the already existing, registered {@link GatewaySender GatewaySenders} for this {@link Region}.
	 *
	 * @param gatewaySenderIds array of {@link GatewaySender} {@link String IDs} to register with this {@link Region}.
	 * @see org.apache.geode.cache.wan.GatewaySender
	 * @see #addGatewaySenders(GatewaySender[])
	 * @see #setGatewaySenders(GatewaySender[])
	 * @see #setGatewaySenderIds(String[])
	 */
	public void addGatewaySendersIds(@NonNull String[] gatewaySenderIds) {

		Arrays.stream(ArrayUtils.nullSafeArray(gatewaySenderIds, String.class))
			.filter(StringUtils::hasText)
			.forEach(this.gatewaySenderIds::add);
	}

	/**
	 * Configures this {@link Region} with the capability to store data in {@literal off-heap memory}.
	 *
	 * @param offHeap {@link Boolean} value indicating whether to enable the use of {@literal off-heap memory}
	 * for this {@link Region}.
	 * @see org.apache.geode.cache.RegionFactory#setOffHeap(boolean)
	 */
	public void setOffHeap(Boolean offHeap) {
		this.offHeap = offHeap;
	}

	/**
	 * Returns a {@link Boolean} value indicating whether {@literal off-heap memory} was enabled for this {@link Region}.
	 *
	 * {@literal Off-heap memory} will be enabled for this {@link Region} if this method returns a {@literal non-null},
	 * {@link Boolean} value evaluating to {@literal true}.
	 *
	 * @return a {@link Boolean} value indicating whether {@literal off-heap memory} use is enabled for
	 * this {@link Region}.
	 */
	public Boolean getOffHeap() {
		return this.offHeap;
	}

	/**
	 * Returns a boolean value indicating whether {@literal off-heap memory} use was enabled for this {@link Region}.
	 *
	 * @return a {@literal boolean} value indicating whether {@literal off-heap memory} use was enabled for
	 * this {@link Region}.
	 * @see #getOffHeap()
	 */
	public boolean isOffHeap() {
		return Boolean.TRUE.equals(getOffHeap());
	}

	/**
	 * Configures the {@link Class key constraint} used to enforce key {@link Class types} for this {@link Region}.
	 *
	 * @param keyConstraint {@link Class} specifying the key type constraint for this {@link Region}.
	 * @see org.apache.geode.cache.RegionFactory#setKeyConstraint(Class)
	 * @see org.apache.geode.cache.RegionAttributes#getKeyConstraint()
	 * @see java.lang.Class
	 */
	public void setKeyConstraint(Class<K> keyConstraint) {
		this.keyConstraint = keyConstraint;
	}

	/**
	 * Configures whether to enable {@literal persistence} for this {@link Region}.
	 *
	 * When {@literal persistence} is enable, then data in the {@link Region} is persisted to disk
	 * using the configured, specified {@link DiskStore}, or the {@literal DEFAULT} {@link DiskStore}
	 * if a {@link DiskStore} was not explicitly configured.
	 *
	 * @param persistent {@link Boolean} value indicating whether to enaable {@literal persistence}
	 * for this {@link Region}.
	 */
	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
	}

	public void setRegionIdleTimeout(ExpirationAttributes regionIdleTimeout) {
		this.regionIdleTimeout = regionIdleTimeout;
	}

	public void setRegionTimeToLive(ExpirationAttributes regionTimeToLive) {
		this.regionTimeToLive = regionTimeToLive;
	}

	/**
	 * Configures the {@link Region Region's} {@link Scope}, which affects data distribution
	 * and acknowledgement strategy (useful in consistency) for the {@link Region}.
	 *
	 * @param scope {@link Scope} used to configure the {@link Region Region's} data distribution
	 * and acknowledgement strategy.
	 * @see org.apache.geode.cache.Scope
	 */
	public void setScope(Scope scope) {
		this.scope = scope;
	}

	/**
	 * Returns the configured {@link Scope} of the {@link Region} affecting data distribution
	 * and acknowledgement strategy (useful in consistency) for the {@link Region}.
	 *
	 * @return the configured {@link Scope} of the {@link Region}.
	 * @see org.apache.geode.cache.Scope
	 */
	public Scope getScope() {
		return this.scope;
	}

	/**
	 * Configures the {@link Region} with the given {@link RegionShortcut}.
	 *
	 * @param shortcut {@link RegionShortcut} used to configure pre-defined defaults for the {@link Region}.
	 * @see org.apache.geode.cache.RegionShortcut
	 */
	public void setShortcut(RegionShortcut shortcut) {
		this.shortcut = shortcut;
	}

	/**
	 * Returns the configured {@link RegionShortcut}.
	 *
	 * @return the configured {@link RegionShortcut}.
	 * @see org.apache.geode.cache.RegionShortcut
	 */
	public RegionShortcut getShortcut() {
		return this.shortcut;
	}

	public void setStatisticsEnabled(Boolean statisticsEnabled) {
		this.statisticsEnabled = statisticsEnabled;
	}

	public Boolean getStatisticsEnabled() {
		return this.statisticsEnabled;
	}

	public boolean isStatisticsEnabled() {
		return Boolean.TRUE.equals(getStatisticsEnabled());
	}

	protected boolean resolveStatisticsEnabled() {

		return isStatisticsEnabled()
			|| this.customEntryIdleTimeout != null
			|| this.customEntryTimeToLive != null
			|| this.entryIdleTimeout != null
			|| this.entryTimeToLive != null
			|| this.regionIdleTimeout != null
			|| this.regionTimeToLive != null
			|| Optional.ofNullable(getAttributes())
				.map(RegionAttributes::getStatisticsEnabled)
				.orElse(false);
	}

	/**
	 * Configures the {@link Class value constraint} used to enforce value {@link Class types} for this {@link Region}.
	 *
	 * @param valueConstraint {@link Class} specifying the value type constraint for this {@link Region}.
	 * @see org.apache.geode.cache.RegionFactory#setValueConstraint(Class)
	 * @see org.apache.geode.cache.RegionAttributes#getValueConstraint()
	 * @see java.lang.Class
	 */
	public void setValueConstraint(Class<V> valueConstraint) {
		this.valueConstraint = valueConstraint;
	}

	@Override
	@SuppressWarnings("all")
	public void start() {

		if (!this.gatewaySenders.isEmpty()) {
			synchronized(this.gatewaySenders) {
				this.gatewaySenders.stream()
					.filter(Objects::nonNull)
					.filter(gatewaySender -> !gatewaySender.isManualStart())
					.filter(gatewaySender -> !gatewaySender.isRunning())
					.forEach(GatewaySender::start);
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

		if (!this.gatewaySenders.isEmpty()) {
			synchronized (this.gatewaySenders) {
				this.gatewaySenders.forEach(GatewaySender::stop);
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
