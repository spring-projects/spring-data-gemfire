/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import org.apache.geode.cache.AttributesMutator;
import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheWriter;
import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.EvictionAttributesMutator;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewaySender;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * The LookupRegionFactoryBean class is a concrete implementation of RegionLookupFactoryBean for handling
 * &gt;gfe:lookup-region/&lt; SDG XML namespace (XSD) elements.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.RegionLookupFactoryBean
 * @see org.apache.geode.cache.AttributesMutator
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class LookupRegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V> {

	private Boolean cloningEnabled;
	private Boolean enableStatistics;

	private AsyncEventQueue[] asyncEventQueues;

	private CacheListener<K, V>[] cacheListeners;

	private CacheLoader<K, V> cacheLoader;

	private CacheWriter<K, V> cacheWriter;

	private CustomExpiry<K, V> customEntryIdleTimeout;
	private CustomExpiry<K, V> customEntryTimeToLive;

	private ExpirationAttributes entryIdleTimeout;
	private ExpirationAttributes entryTimeToLive;
	private ExpirationAttributes regionIdleTimeout;
	private ExpirationAttributes regionTimeToLive;

	private GatewaySender[] gatewaySenders;

	private Integer evictionMaximum;

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();

		AttributesMutator<K, V> attributesMutator = getRegion().getAttributesMutator();

		if (!ObjectUtils.isEmpty(asyncEventQueues)) {
			for (AsyncEventQueue asyncEventQueue : asyncEventQueues) {
				attributesMutator.addAsyncEventQueueId(asyncEventQueue.getId());
			}
		}

		if (!ObjectUtils.isEmpty(cacheListeners)) {
			for (CacheListener<K, V> cacheListener : cacheListeners) {
				attributesMutator.addCacheListener(cacheListener);
			}
		}

		if (cacheLoader != null) {
			attributesMutator.setCacheLoader(cacheLoader);
		}

		if (cacheWriter != null) {
			attributesMutator.setCacheWriter(cacheWriter);
		}

		if (cloningEnabled != null) {
			attributesMutator.setCloningEnabled(cloningEnabled);
		}

		if (isStatisticsEnabled()) {
			assertStatisticsEnabled();

			if (customEntryIdleTimeout != null) {
				attributesMutator.setCustomEntryIdleTimeout(customEntryIdleTimeout);
			}

			if (customEntryTimeToLive != null) {
				attributesMutator.setCustomEntryTimeToLive(customEntryTimeToLive);
			}

			if (entryIdleTimeout != null) {
				attributesMutator.setEntryIdleTimeout(entryIdleTimeout);
			}

			if (entryTimeToLive != null) {
				attributesMutator.setEntryTimeToLive(entryTimeToLive);
			}

			if (regionIdleTimeout != null) {
				attributesMutator.setRegionIdleTimeout(regionIdleTimeout);
			}

			if (regionTimeToLive != null) {
				attributesMutator.setRegionTimeToLive(regionTimeToLive);
			}
		}

		if (evictionMaximum != null) {
			EvictionAttributesMutator evictionAttributesMutator = attributesMutator.getEvictionAttributesMutator();
			evictionAttributesMutator.setMaximum(evictionMaximum);
		}

		if (!ObjectUtils.isEmpty(gatewaySenders)) {
			for (GatewaySender gatewaySender : gatewaySenders) {
				attributesMutator.addGatewaySenderId(gatewaySender.getId());
			}
		}
	}

	@Override
	final boolean isLookupEnabled() {
		return true;
	}

	/* (non-Javadoc) */
	public void setAsyncEventQueues(AsyncEventQueue[] asyncEventQueues) {
		this.asyncEventQueues = asyncEventQueues;
	}

	/* (non-Javadoc) */
	public void setCacheListeners(CacheListener<K, V>[] cacheListeners) {
		this.cacheListeners = cacheListeners;
	}

	/* (non-Javadoc) */
	public void setCacheLoader(CacheLoader<K, V> cacheLoader) {
		this.cacheLoader = cacheLoader;
	}

	/* (non-Javadoc) */
	public void setCacheWriter(CacheWriter<K, V> cacheWriter) {
		this.cacheWriter = cacheWriter;
	}

	/* (non-Javadoc) */
	public void setCloningEnabled(Boolean cloningEnabled) {
		this.cloningEnabled = cloningEnabled;
	}

	/* (non-Javadoc) */
	public void setCustomEntryIdleTimeout(CustomExpiry<K, V> customEntryIdleTimeout) {
		setStatisticsEnabled(customEntryIdleTimeout != null);
		this.customEntryIdleTimeout = customEntryIdleTimeout;
	}

	/* (non-Javadoc) */
	public void setCustomEntryTimeToLive(CustomExpiry<K, V> customEntryTimeToLive) {
		setStatisticsEnabled(customEntryTimeToLive != null);
		this.customEntryTimeToLive = customEntryTimeToLive;
	}

	/* (non-Javadoc) */
	public void setEntryIdleTimeout(ExpirationAttributes entryIdleTimeout) {
		setStatisticsEnabled(entryIdleTimeout != null);
		this.entryIdleTimeout = entryIdleTimeout;
	}

	/* (non-Javadoc) */
	public void setEntryTimeToLive(ExpirationAttributes entryTimeToLive) {
		setStatisticsEnabled(entryTimeToLive != null);
		this.entryTimeToLive = entryTimeToLive;
	}

	/* (non-Javadoc) */
	public void setEvictionMaximum(final Integer evictionMaximum) {
		this.evictionMaximum = evictionMaximum;
	}

	/* (non-Javadoc) */
	public void setGatewaySenders(GatewaySender[] gatewaySenders) {
		this.gatewaySenders = gatewaySenders;
	}

	/* (non-Javadoc) */
	public void setRegionIdleTimeout(ExpirationAttributes regionIdleTimeout) {
		setStatisticsEnabled(regionIdleTimeout != null);
		this.regionIdleTimeout = regionIdleTimeout;
	}

	/* (non-Javadoc) */
	public void setRegionTimeToLive(ExpirationAttributes regionTimeToLive) {
		setStatisticsEnabled(regionTimeToLive != null);
		this.regionTimeToLive = regionTimeToLive;
	}

	/* (non-Javadoc) */
	public void setStatisticsEnabled(Boolean enableStatistics) {
		this.enableStatistics = enableStatistics;
	}

	/* (non-Javadoc) */
	protected boolean isStatisticsEnabled() {
		return Boolean.TRUE.equals(this.enableStatistics);
	}

	/* (non-Javadoc) */
	private void assertStatisticsEnabled() {
		Region localRegion = getRegion();
		Assert.state(localRegion.getAttributes().getStatisticsEnabled(), String.format(
			"Statistics for Region '%1$s' must be enabled to change Entry & Region TTL/TTI Expiration settings",
				localRegion.getFullPath()));
	}

}
