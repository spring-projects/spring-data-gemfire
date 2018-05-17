/*
 * Copyright 2010-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.util.Arrays;
import java.util.Optional;

import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheWriter;
import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewaySender;
import org.springframework.util.Assert;

/**
 * The LookupRegionFactoryBean class is a concrete implementation of ResolvableRegionFactoryBean for handling
 * &gt;gfe:lookup-region/&lt; SDG XML namespace (XSD) elements.
 *
 * @author John Blum
 * @see org.apache.geode.cache.AttributesMutator
 * @see org.springframework.data.gemfire.ResolvableRegionFactoryBean
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class LookupRegionFactoryBean<K, V> extends ResolvableRegionFactoryBean<K, V> {

	private AsyncEventQueue[] asyncEventQueues;

	private Boolean cloningEnabled;
	private Boolean enableStatistics;

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

		Optional.ofNullable(getRegion().getAttributesMutator()).ifPresent(attributesMutator -> {

			Arrays.stream(nullSafeArray(this.asyncEventQueues, AsyncEventQueue.class))
				.map(AsyncEventQueue::getId)
				.forEach(attributesMutator::addAsyncEventQueueId);

			Arrays.stream(nullSafeArray(this.cacheListeners, CacheListener.class))
				.forEach(attributesMutator::addCacheListener);

			Optional.ofNullable(this.cacheLoader).ifPresent(attributesMutator::setCacheLoader);
			Optional.ofNullable(this.cacheWriter).ifPresent(attributesMutator::setCacheWriter);
			Optional.ofNullable(this.cloningEnabled).ifPresent(attributesMutator::setCloningEnabled);

			// Eviction
			Optional.ofNullable(attributesMutator.getEvictionAttributesMutator())
				.ifPresent(evictionAttributesMutator -> Optional.ofNullable(this.evictionMaximum)
					.ifPresent(evictionAttributesMutator::setMaximum));

			// Expiration
			if (isStatisticsEnabled()) {

				assertStatisticsEnabled();

				Optional.ofNullable(this.customEntryIdleTimeout).ifPresent(attributesMutator::setCustomEntryIdleTimeout);
				Optional.ofNullable(this.customEntryTimeToLive).ifPresent(attributesMutator::setCustomEntryTimeToLive);
				Optional.ofNullable(this.entryIdleTimeout).ifPresent(attributesMutator::setEntryIdleTimeout);
				Optional.ofNullable(this.entryTimeToLive).ifPresent(attributesMutator::setEntryTimeToLive);
				Optional.ofNullable(this.regionIdleTimeout).ifPresent(attributesMutator::setRegionIdleTimeout);
				Optional.ofNullable(this.regionTimeToLive).ifPresent(attributesMutator::setRegionTimeToLive);
			}

			Arrays.stream(nullSafeArray(this.gatewaySenders, GatewaySender.class))
				.map(GatewaySender::getId)
				.forEach(attributesMutator::addGatewaySenderId);
		});
	}

	@Override
	public final boolean isLookupEnabled() {
		return true;
	}

	public void setAsyncEventQueues(AsyncEventQueue[] asyncEventQueues) {
		this.asyncEventQueues = asyncEventQueues;
	}

	public void setCacheListeners(CacheListener<K, V>[] cacheListeners) {
		this.cacheListeners = cacheListeners;
	}

	public void setCacheLoader(CacheLoader<K, V> cacheLoader) {
		this.cacheLoader = cacheLoader;
	}

	public void setCacheWriter(CacheWriter<K, V> cacheWriter) {
		this.cacheWriter = cacheWriter;
	}

	public void setCloningEnabled(Boolean cloningEnabled) {
		this.cloningEnabled = cloningEnabled;
	}

	public void setCustomEntryIdleTimeout(CustomExpiry<K, V> customEntryIdleTimeout) {
		setStatisticsEnabled(customEntryIdleTimeout != null);
		this.customEntryIdleTimeout = customEntryIdleTimeout;
	}

	public void setCustomEntryTimeToLive(CustomExpiry<K, V> customEntryTimeToLive) {
		setStatisticsEnabled(customEntryTimeToLive != null);
		this.customEntryTimeToLive = customEntryTimeToLive;
	}

	public void setEntryIdleTimeout(ExpirationAttributes entryIdleTimeout) {
		setStatisticsEnabled(entryIdleTimeout != null);
		this.entryIdleTimeout = entryIdleTimeout;
	}

	public void setEntryTimeToLive(ExpirationAttributes entryTimeToLive) {
		setStatisticsEnabled(entryTimeToLive != null);
		this.entryTimeToLive = entryTimeToLive;
	}

	public void setEvictionMaximum(final Integer evictionMaximum) {
		this.evictionMaximum = evictionMaximum;
	}

	public void setGatewaySenders(GatewaySender[] gatewaySenders) {
		this.gatewaySenders = gatewaySenders;
	}

	public void setRegionIdleTimeout(ExpirationAttributes regionIdleTimeout) {
		setStatisticsEnabled(regionIdleTimeout != null);
		this.regionIdleTimeout = regionIdleTimeout;
	}

	public void setRegionTimeToLive(ExpirationAttributes regionTimeToLive) {
		setStatisticsEnabled(regionTimeToLive != null);
		this.regionTimeToLive = regionTimeToLive;
	}

	public void setStatisticsEnabled(Boolean enableStatistics) {
		this.enableStatistics = enableStatistics;
	}

	protected boolean isStatisticsEnabled() {
		return Boolean.TRUE.equals(this.enableStatistics);
	}

	private void assertStatisticsEnabled() {

		Region localRegion = getRegion();

		Assert.state(localRegion.getAttributes().getStatisticsEnabled(),
			String.format("Statistics for Region [%s] must be enabled to change Entry & Region TTL/TTI Expiration settings",
				localRegion.getFullPath()));
	}
}
