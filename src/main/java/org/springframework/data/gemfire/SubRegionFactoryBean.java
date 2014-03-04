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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.wan.GatewaySender;

/**
 * FactoryBean for creating a Gemfire sub-Regions.
 * <p/>
 * @author David Turanski
 * @author John Blum
 * @param <K> Region Key Type
 * @param <V> Region Value Type
 * @deprecated as Spring Data GemFire 1.4.0.  Use Region type specific FactoryBeans
 * (e.g. ReplicatedRegionFactoryBean) instead.
 */
@Deprecated
@SuppressWarnings("deprecation")
public class SubRegionFactoryBean<K, V> extends AttributesFactory<K, V> implements FactoryBean<Region<K, V>>,
		InitializingBean {

	protected final Log log = LogFactory.getLog(getClass());

	private boolean lookupOnly;

	private CacheListener<K, V>[] cacheListeners;

	private Object[] asyncEventQueues;
	private Object[] gatewaySenders;

	private Region<?, ?> parentRegion;
	private Region<K, V> subRegion;

	@SuppressWarnings("unused")
	private String name;
	private String regionName;

	@Override
	public Region<K, V> getObject() throws Exception {
		return this.subRegion;
	}

	@Override
	public Class<?> getObjectType() {
		// TODO perhaps this should be 'return (subRegion != null ? subRegion.getClass() : Region.class);' for consistency.
		return Region.class;
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		Assert.notNull(parentRegion, "The parent Region cannot be null.");

		this.subRegion = parentRegion.getSubregion(regionName);

		if (this.subRegion == null) {
			if (lookupOnly) {
				throw new BeanInitializationException(String.format("Cannot find Region [%1$s] in Cache %2$s",
					regionName, parentRegion.getRegionService()));
			}
			else {
				log.debug(String.format("Creating sub-Region of [%1$s] with name [%2$s]...",
					(parentRegion.getFullPath() != null ? parentRegion.getFullPath() : parentRegion.getName()),
						regionName));

				if (!ObjectUtils.isEmpty(asyncEventQueues)) {
					for (Object asyncEventQueue : asyncEventQueues) {
						addAsyncEventQueueId(((AsyncEventQueue) asyncEventQueue).getId());
					}
				}

				if (!ObjectUtils.isEmpty(cacheListeners)) {
					for (CacheListener<K, V> listener : cacheListeners) {
						addCacheListener(listener);
					}
				}

				if (!ObjectUtils.isEmpty(gatewaySenders)) {
					for (Object gatewaySender : gatewaySenders) {
						addGatewaySenderId(((GatewaySender) gatewaySender).getId());
					}
				}

				this.subRegion = this.parentRegion.createSubregion(regionName, create());
			}
		}
	}

	/**
	 *
	 * @param asyncEventQueues defined as Object for backward compatibility with Gemfire 6.
	 */
	public void setAsyncEventQueues(Object[] asyncEventQueues) {
		this.asyncEventQueues = asyncEventQueues;
	}

	/**
	 * Sets the cache listeners used for the region used by this factory. Used
	 * only when a new region is created.Overrides the settings specified
	 * through {@link #setAttributes(com.gemstone.gemfire.cache.RegionAttributes)}.
	 *
	 * @param cacheListeners the cacheListeners to set on a newly created region
	 */
	public void setCacheListeners(CacheListener<K, V>[] cacheListeners) {
		this.cacheListeners = cacheListeners;
	}

	/**
	 *
	 * @param gatewaySenders
	 */
	public void setGatewaySenders(Object[] gatewaySenders) {
		this.gatewaySenders = gatewaySenders;
	}

	/**
	 * Set to true if the subregion should already exist, e.g., specified by
	 * &lt;lookup-region&gt;
	 * @param lookupOnly
	 */
	public void setLookupOnly(boolean lookupOnly) {
		this.lookupOnly = lookupOnly;
	}

	/**
	 * Set the bean name - the same as the subregion full path
	 * @param name
	 */
	public void setName(String name) {
        this.name = name;
	}

	/**
	 * Set the simple name of the region
	 * @param regionName
	 */
	public void setRegionName(String regionName) {
		this.regionName = regionName;
	}

	/**
	 * Set the parent Region
	 * @param parent
	 */
	public void setParent(Region<?, ?> parent) {
		this.parentRegion = parent;
	}

}
