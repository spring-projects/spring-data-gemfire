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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheWriter;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.wan.GatewaySender;

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
public class RegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V> implements DisposableBean, SmartLifecycle {

	protected final Log log = LogFactory.getLog(getClass());

	private boolean autoStartup = true;
	private boolean close = true;
	private boolean destroy = false;
	private boolean running;

	private Boolean enableGateway;
	private Boolean persistent;

	private CacheListener<K, V>[] cacheListeners;

	private CacheLoader<K, V> cacheLoader;

	private CacheWriter<K, V> cacheWriter;

	private Object[] asyncEventQueues;
	private Object[] gatewaySenders;

	private RegionAttributes<K, V> attributes;

	private Resource snapshot;

	private Scope scope;

	private String dataPolicy;
	private String diskStoreName;
	private String hubId;

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		postProcess(getRegion());
	}

	@Override
	protected Region<K, V> lookupFallback(GemFireCache gemfireCache, String regionName) throws Exception {
		Assert.isTrue(gemfireCache instanceof Cache, "Unable to create Regions from " + gemfireCache);

		Cache cache = (Cache) gemfireCache;

		if (attributes != null) {
			AttributesFactory.validateAttributes(attributes);
		}

		final RegionFactory<K, V> regionFactory = (attributes != null ? cache.createRegionFactory(attributes) :
			cache.<K, V> createRegionFactory());

		if (hubId != null) {
			enableGateway = enableGateway == null ? true : enableGateway;
			Assert.isTrue(enableGateway, "hubId requires the enableGateway property to be true");

			regionFactory.setGatewayHubId(hubId);
		}

		if (enableGateway != null) {
			if (enableGateway) {
				Assert.notNull(hubId, "enableGateway requires the hubId property to be true");
			}
			regionFactory.setEnableGateway(enableGateway);
		}

		if (!ObjectUtils.isEmpty(cacheListeners)) {
			for (CacheListener<K, V> listener : cacheListeners) {
				regionFactory.addCacheListener(listener);
			}
		}

		if (!ObjectUtils.isEmpty(gatewaySenders)) {
			Assert.isTrue(hubId == null, "It is invalid to configure a region with both a hubId and gatewaySenders."
				+ " Note that the enableGateway and hubId properties are deprecated since Gemfire 7.0");
			for (Object gatewaySender : gatewaySenders) {
				regionFactory.addGatewaySenderId(((GatewaySender) gatewaySender).getId());
			}
		}

		if (!ObjectUtils.isEmpty(asyncEventQueues)) {
			for (Object asyncEventQueue : asyncEventQueues) {
				regionFactory.addAsyncEventQueueId(((AsyncEventQueue) asyncEventQueue).getId());
			}
		}

		if (cacheLoader != null) {
			regionFactory.setCacheLoader(cacheLoader);
		}

		if (cacheWriter != null) {
			regionFactory.setCacheWriter(cacheWriter);
		}

		if (diskStoreName != null) {
			regionFactory.setDiskStoreName(diskStoreName);
		}

		resolveDataPolicy(regionFactory, persistent, dataPolicy);

		if (scope != null) {
			regionFactory.setScope(scope);
		}

		if (attributes != null) {
			Assert.state(!attributes.isLockGrantor() || scope.isGlobal(),
				"Lock grantor only applies to a global scoped region");
		}

		// get underlying AttributesFactory
		postProcess(findAttrFactory(regionFactory));

		Region<K, V> region = regionFactory.create(regionName);
		log.info("Created new cache region [" + regionName + "]");

		if (snapshot != null) {
			region.loadSnapshot(snapshot.getInputStream());
		}

		if (attributes != null && attributes.isLockGrantor()) {
			region.becomeLockGrantor();
		}

		return region;
	}

	/**
	 * Validates the configured Data Policy and may override it, taking into account the 'persistent' attribute
	 * and constraints for the Region type.
	 * <p/>
	 * @param regionFactory the GemFire RegionFactory used to created the Local Region.
	 * @param persistent a boolean value indicating whether the Local Region should persist it's data.
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
		}
		else {
			regionFactory.setDataPolicy(isPersistent() ? DataPolicy.PERSISTENT_REPLICATE : DataPolicy.DEFAULT);
		}
	}

	@SuppressWarnings("unchecked")
	private AttributesFactory<K, V> findAttrFactory(RegionFactory<K, V> regionFactory) {
		Field attrsFactoryField = ReflectionUtils.findField(RegionFactory.class, "attrsFactory",
			AttributesFactory.class);
		ReflectionUtils.makeAccessible(attrsFactoryField);
		return (AttributesFactory<K, V>) ReflectionUtils.getField(attrsFactoryField, regionFactory);
	}

	/**
	 * Post-process the attribute factory object used for configuring the region
	 * of this factory bean during the initialization process. The object is
	 * already initialized and configured by the factory bean before this method
	 * is invoked.
	 *
	 * @param attrFactory attribute factory
	 * @deprecated as of GemFire 6.5, the use of {@link AttributesFactory} has
	 * been deprecated
	 */
	@Deprecated
	protected void postProcess(AttributesFactory<K, V> attrFactory) {
	}

	/**
	 * Post-process the region object for this factory bean during the
	 * initialization process. The object is already initialized and configured
	 * by the factory bean before this method is invoked.
	 *
	 * @param region
	 */
	protected void postProcess(Region<K, V> region) {

	}

	@Override
	public void destroy() throws Exception {
		if (getRegion() != null) {
			if (close) {
				if (!getRegion().getRegionService().isClosed()) {
					try {
						getRegion().close();
					} catch (Exception cce) {
						// nothing to see folks, move on.
					}
				}

			} if (destroy) {
				getRegion().destroyRegion();
			}
		}
	}

	/**
	 * Indicates whether the region referred by this factory bean, will be
	 * closed on shutdown (default true).
	 */
	public void setClose(boolean close) {
		this.close = close;
	}

	/**
	 * Indicates whether the region referred by this factory bean, will be
	 * destroyed on shutdown (default false).
	 */
	public void setDestroy(boolean destroy) {
		this.destroy = destroy;
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
	 * Sets the region scope. Used only when a new region is created. Overrides
	 * the settings specified through {@link #setAttributes(RegionAttributes)}.
	 *
	 * @see Scope
	 * @param scope the region scope
	 */
	public void setScope(Scope scope) {
		this.scope = scope;
	}

	public void setPersistent(boolean persistent) {
		this.persistent = persistent;
	}

	/**
	 * Sets the dataPolicy as a String. Required to support property
	 * placeholders
	 * @param dataPolicyName the dataPolicy name (NORMAL, PRELOADED, etc)
	 */
	public void setDataPolicy(String dataPolicyName) {
		this.dataPolicy = dataPolicyName;
	}

	/**
	 * Sets the name of disk store to use for overflow and persistence
	 * @param diskStoreName
	 */
	public void setDiskStoreName(String diskStoreName) {
		this.diskStoreName = diskStoreName;
	}

	/**
	 *
	 * @param gatewaySenders defined as Object for backward compatibility with
	 * Gemfire 6
	 */
	public void setGatewaySenders(Object[] gatewaySenders) {
		this.gatewaySenders = gatewaySenders;
	}

	/**
	 *
	 * @param asyncEventQueues defined as Object for backward compatibility with
	 * Gemfire 6
	 */
	public void setAsyncEventQueues(Object[] asyncEventQueues) {
		this.asyncEventQueues = asyncEventQueues;
	}

	public void setEnableGateway(boolean enableGateway) {
		this.enableGateway = enableGateway;
	}

	public void setHubId(String hubId) {
		this.hubId = hubId;
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
	 * Validates that the settings for Data Policy and the 'persistent' attribute in <gfe:*-region/> elements
	 * are compatible.
	 * <p/>
	 * @param resolvedDataPolicy the GemFire Data Policy resolved form the Spring GemFire XML namespace configuration
	 * meta-data.
	 * @see #isPersistent()
	 * @see #isNotPersistent()
	 * @see com.gemstone.gemfire.cache.DataPolicy
	 */
	protected void assertDataPolicyAndPersistentAttributesAreCompatible(final DataPolicy resolvedDataPolicy) {
		final boolean persistentNotSpecified = (this.persistent == null);

		if (resolvedDataPolicy.withPersistence()) {
			Assert.isTrue(persistentNotSpecified || isPersistent(), String.format(
				"Data Policy '%1$s' is invalid when persistent is false.", resolvedDataPolicy));
		}
		else {
			// NOTE otherwise, the Data Policy is with persistence, so...
			Assert.isTrue(persistentNotSpecified || isNotPersistent(), String.format(
				"Data Policy '%1$s' is invalid when persistent is true.", resolvedDataPolicy));
		}
	}

	/**
	 * Returns true when the user explicitly specified a value for the persistent attribute and it is true.  If the
	 * persistent attribute was not explicitly specified, then the persistence setting is implicitly undefined
	 * and will be determined by the Data Policy.
	 * <p/>
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
	 * <p/>
	 * @return true when the user specified an explicit value for the persistent attribute and it is false;
	 * false otherwise.
	 * @see #isPersistent()
	 */
	protected boolean isNotPersistent() {
		return Boolean.FALSE.equals(persistent);
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.Lifecycle#start()
	 */
	@Override
	public void start() {
		if (!ObjectUtils.isEmpty(gatewaySenders)) {
			synchronized (gatewaySenders) {
				for (Object obj : gatewaySenders) {
					GatewaySender gws = (GatewaySender) obj;
					if (!(gws.isManualStart() || gws.isRunning())) {
						gws.start();
					}
				}
			}
		}
		this.running = true;
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.Lifecycle#stop()
	 */
	@Override
	public void stop() {
		if (!ObjectUtils.isEmpty(gatewaySenders)) {
			synchronized (gatewaySenders) {
				for (Object obj : gatewaySenders) {
					((GatewaySender) obj).stop();
				}
			}
		}
		this.running = false;
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.Lifecycle#isRunning()
	 */
	@Override
	public boolean isRunning() {
		return this.running;
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.Phased#getPhase()
	 */
	@Override
	public int getPhase() {
		return Integer.MAX_VALUE;
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.SmartLifecycle#isAutoStartup()
	 */
	@Override
	public boolean isAutoStartup() {
		return this.autoStartup;
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.SmartLifecycle#stop(java.lang.Runnable)
	 */
	@Override
	public void stop(Runnable callback) {
		stop();
		callback.run();
	}

}
