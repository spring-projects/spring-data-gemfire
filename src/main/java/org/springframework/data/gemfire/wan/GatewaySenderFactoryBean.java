/*
 * Copyright 2010-2019 the original author or authors.
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
package org.springframework.data.gemfire.wan;

import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.cache.wan.GatewaySenderFactory;
import org.apache.geode.cache.wan.GatewayTransportFilter;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} for creating a parallel or serial Pivotal GemFire {@link GatewaySender}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.wan.AbstractWANComponentFactoryBean
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.util.Gateway
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.apache.geode.cache.wan.GatewaySenderFactory
 * @since 1.2.2
 */
@SuppressWarnings("unused")
public class GatewaySenderFactoryBean extends AbstractWANComponentFactoryBean<GatewaySender> {

	private boolean manualStart = false;

	private int remoteDistributedSystemId;

	private Boolean diskSynchronous;
	private Boolean batchConflationEnabled;
	private Boolean parallel;
	private Boolean persistent;

	private GatewayEventSubstitutionFilter eventSubstitutionFilter;

	private GatewaySender gatewaySender;

	private GatewaySender.OrderPolicy orderPolicy;

	private Integer alertThreshold;
	private Integer batchSize;
	private Integer batchTimeInterval;
	private Integer dispatcherThreads;
	private Integer maximumQueueMemory;
	private Integer socketBufferSize;
	private Integer socketReadTimeout;

	private List<GatewayEventFilter> eventFilters;

	private List<GatewayTransportFilter> transportFilters;

	private String diskStoreReference;

	/**
	 * Constructs an instance of the {@link GatewaySenderFactoryBean} class initialized with a reference to
	 * the Pivotal GemFire {@link Cache} used to configured and initialized a Pivotal GemFire {@link GatewaySender}.
	 *
	 * @param cache reference to the Pivotal GemFire {@link Cache} used to create the Pivotal GemFire {@link GatewaySender}.
	 * @see org.apache.geode.cache.Cache
	 */
	public GatewaySenderFactoryBean(Cache cache) {
		super(cache);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doInit() {

		GatewaySenderFactory gatewaySenderFactory = resolveGatewaySenderFactory();

		Optional.ofNullable(this.alertThreshold).ifPresent(gatewaySenderFactory::setAlertThreshold);
		Optional.ofNullable(this.batchConflationEnabled).ifPresent(gatewaySenderFactory::setBatchConflationEnabled);
		Optional.ofNullable(this.batchSize).ifPresent(gatewaySenderFactory::setBatchSize);
		Optional.ofNullable(this.batchTimeInterval).ifPresent(gatewaySenderFactory::setBatchTimeInterval);

		Optional.ofNullable(this.diskStoreReference)
			.filter(StringUtils::hasText)
			.ifPresent(gatewaySenderFactory::setDiskStoreName);

		Optional.ofNullable(this.diskSynchronous).ifPresent(gatewaySenderFactory::setDiskSynchronous);
		Optional.ofNullable(this.dispatcherThreads).ifPresent(gatewaySenderFactory::setDispatcherThreads);

		CollectionUtils.nullSafeList(this.eventFilters).forEach(gatewaySenderFactory::addGatewayEventFilter);

		Optional.ofNullable(this.eventSubstitutionFilter)
			.ifPresent(gatewaySenderFactory::setGatewayEventSubstitutionFilter);

		gatewaySenderFactory.setManualStart(this.manualStart);

		Optional.ofNullable(this.maximumQueueMemory).ifPresent(gatewaySenderFactory::setMaximumQueueMemory);
		Optional.ofNullable(this.orderPolicy).ifPresent(gatewaySenderFactory::setOrderPolicy);

		gatewaySenderFactory.setParallel(isParallelGatewaySender());
		gatewaySenderFactory.setPersistenceEnabled(isPersistent());

		Optional.ofNullable(this.socketBufferSize).ifPresent(gatewaySenderFactory::setSocketBufferSize);
		Optional.ofNullable(this.socketReadTimeout).ifPresent(gatewaySenderFactory::setSocketReadTimeout);

		CollectionUtils.nullSafeList(this.transportFilters).forEach(gatewaySenderFactory::addGatewayTransportFilter);

		GatewaySenderWrapper wrapper =
			new GatewaySenderWrapper(gatewaySenderFactory.create(getName(), this.remoteDistributedSystemId));

        wrapper.setManualStart(this.manualStart);
        this.gatewaySender = wrapper;
	}

	private GatewaySenderFactory resolveGatewaySenderFactory() {

		return this.factory != null
			? (GatewaySenderFactory) this.factory
			: this.cache.createGatewaySenderFactory();
	}

	@Override
	public GatewaySender getObject() throws Exception {
		return this.gatewaySender;
	}

	@Override
	public Class<?> getObjectType() {

		return this.gatewaySender != null
			? this.gatewaySender.getClass()
			: GatewaySender.class;
	}

	public void setAlertThreshold(Integer alertThreshold) {
		this.alertThreshold = alertThreshold;
	}

	public void setBatchConflationEnabled(Boolean batchConflationEnabled) {
		this.batchConflationEnabled = batchConflationEnabled;
	}

	/**
	 * Boolean value that determines whether Pivotal GemFire should conflate messages.
	 *
	 * @param enableBatchConflation a boolean value indicating whether Pivotal GemFire should conflate messages in the Queue.
	 * @see #setBatchConflationEnabled(Boolean)
	 * @deprecated use setBatchConflationEnabled(Boolean)
	 */
	@Deprecated
	public void setEnableBatchConflation(Boolean enableBatchConflation) {
		this.batchConflationEnabled = enableBatchConflation;
	}

	public void setBatchSize(Integer batchSize) {
		this.batchSize = batchSize;
	}

	public void setBatchTimeInterval(Integer batchTimeInterval) {
		this.batchTimeInterval = batchTimeInterval;
	}

	public void setDiskStoreRef(String diskStoreRef) {
		this.diskStoreReference = diskStoreRef;
	}

	public void setDiskSynchronous(Boolean diskSynchronous) {
		this.diskSynchronous = diskSynchronous;
	}

	public void setDispatcherThreads(Integer dispatcherThreads) {
		this.dispatcherThreads = dispatcherThreads;
	}

	public void setEventFilters(List<GatewayEventFilter> gatewayEventFilters) {
		this.eventFilters = gatewayEventFilters;
	}

	public void setEventSubstitutionFilter(GatewayEventSubstitutionFilter eventSubstitutionFilter) {
		this.eventSubstitutionFilter = eventSubstitutionFilter;
	}

	public void setManualStart(Boolean manualStart) {
		this.manualStart = Boolean.TRUE.equals(manualStart);
	}

	public void setMaximumQueueMemory(Integer maximumQueueMemory) {
		this.maximumQueueMemory = maximumQueueMemory;
	}

	public void setOrderPolicy(GatewaySender.OrderPolicy orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

	public void setParallel(Boolean parallel) {
		this.parallel = parallel;
	}

	public boolean isSerialGatewaySender() {
		return !isParallelGatewaySender();
	}

	public boolean isParallelGatewaySender() {
		return Boolean.TRUE.equals(this.parallel);
	}

	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
	}

	public boolean isNotPersistent() {
		return !isPersistent();
	}

	public boolean isPersistent() {
		return Boolean.TRUE.equals(this.persistent);
	}

	public void setRemoteDistributedSystemId(int remoteDistributedSystemId) {
		this.remoteDistributedSystemId = remoteDistributedSystemId;
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public void setSocketReadTimeout(Integer socketReadTimeout) {
		this.socketReadTimeout = socketReadTimeout;
	}

	public void setTransportFilters(List<GatewayTransportFilter> gatewayTransportFilters) {
		this.transportFilters = gatewayTransportFilters;
	}
}
