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
package org.springframework.data.gemfire.wan;

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.cache.wan.GatewaySenderFactory;
import org.apache.geode.cache.wan.GatewayTransportFilter;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.config.annotation.GatewaySenderConfigurer;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} for creating a parallel or serial Pivotal GemFire {@link GatewaySender}.
 *
 * @author David Turanski
 * @author John Blum
 * @author Udo Kohlmeyer
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.apache.geode.cache.wan.GatewaySenderFactory
 * @see org.springframework.data.gemfire.wan.AbstractWANComponentFactoryBean
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

	private List<GatewaySenderConfigurer> gatewaySenderConfigurers = Collections.emptyList();

	private List<GatewayTransportFilter> transportFilters;

	// TODO: Come of with better association and remove.
	private List<String> regions = new ArrayList<>();

	private String diskStoreReference;

	public GatewaySenderFactoryBean() { }

	/**
	 * Constructs an instance of the {@link GatewaySenderFactoryBean} class initialized with a reference to
	 * the Pivotal GemFire {@link Cache} used to configured and initialized a Pivotal GemFire {@link GatewaySender}.
	 *
	 * @param cache reference to the Pivotal GemFire {@link Cache} used to create the Pivotal GemFire {@link GatewaySender}.
	 * @see org.apache.geode.cache.Cache
	 */
	public GatewaySenderFactoryBean(GemFireCache cache) {
		super(cache);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doInit() {

		GatewaySenderFactory gatewaySenderFactory = resolveGatewaySenderFactory();

		stream(nullSafeIterable(this.gatewaySenderConfigurers).spliterator(), false)
			.forEach(it -> it.configure(getName(), this));

		Optional.ofNullable(getAlertThreshold()).ifPresent(gatewaySenderFactory::setAlertThreshold);
		Optional.ofNullable(getBatchConflationEnabled()).ifPresent(gatewaySenderFactory::setBatchConflationEnabled);
		Optional.ofNullable(getBatchSize()).ifPresent(gatewaySenderFactory::setBatchSize);
		Optional.ofNullable(getBatchTimeInterval()).ifPresent(gatewaySenderFactory::setBatchTimeInterval);

		Optional.ofNullable(getDiskStoreReference())
			.filter(StringUtils::hasText)
			.ifPresent(gatewaySenderFactory::setDiskStoreName);

		Optional.ofNullable(getDiskSynchronous()).ifPresent(gatewaySenderFactory::setDiskSynchronous);
		Optional.ofNullable(getDispatcherThreads()).ifPresent(gatewaySenderFactory::setDispatcherThreads);

		CollectionUtils.nullSafeList(getEventFilters()).forEach(gatewaySenderFactory::addGatewayEventFilter);

		Optional.ofNullable(getEventSubstitutionFilter())
			.ifPresent(gatewaySenderFactory::setGatewayEventSubstitutionFilter);

		gatewaySenderFactory.setManualStart(isManualStart());

		Optional.ofNullable(getMaximumQueueMemory()).ifPresent(gatewaySenderFactory::setMaximumQueueMemory);
		Optional.ofNullable(getOrderPolicy()).ifPresent(gatewaySenderFactory::setOrderPolicy);

		gatewaySenderFactory.setParallel(isParallelGatewaySender());
		gatewaySenderFactory.setPersistenceEnabled(isPersistent());

		Optional.ofNullable(getSocketBufferSize()).ifPresent(gatewaySenderFactory::setSocketBufferSize);
		Optional.ofNullable(getSocketReadTimeout()).ifPresent(gatewaySenderFactory::setSocketReadTimeout);

		CollectionUtils.nullSafeList(getTransportFilters()).forEach(gatewaySenderFactory::addGatewayTransportFilter);

		GatewaySenderWrapper wrapper =
			new GatewaySenderWrapper(gatewaySenderFactory.create(getName(), getRemoteDistributedSystemId()));

        wrapper.setManualStart(isManualStart());
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

	public void setGatewaySender(GatewaySender gatewaySender) {
		this.gatewaySender = gatewaySender;
	}

	public GatewaySender getGatewaySender() {
		return this.gatewaySender;
	}

	public void setGatewaySenderConfigurers(List<GatewaySenderConfigurer> gatewaySenderConfigurers) {
		this.gatewaySenderConfigurers = gatewaySenderConfigurers;
	}

	public void setAlertThreshold(Integer alertThreshold) {
		this.alertThreshold = alertThreshold;
	}

	public Integer getAlertThreshold() {
		return this.alertThreshold;
	}

	public void setBatchConflationEnabled(Boolean batchConflationEnabled) {
		this.batchConflationEnabled = batchConflationEnabled;
	}

	public Boolean getBatchConflationEnabled() {
		return this.batchConflationEnabled;
	}

	public void setBatchSize(Integer batchSize) {
		this.batchSize = batchSize;
	}

	public Integer getBatchSize() {
		return this.batchSize;
	}

	public void setBatchTimeInterval(Integer batchTimeInterval) {
		this.batchTimeInterval = batchTimeInterval;
	}

	public Integer getBatchTimeInterval() {
		return this.batchTimeInterval;
	}

	public void setDiskStoreRef(String diskStoreRef) {
		setDiskStoreReference(diskStoreRef);
	}

	public void setDiskStoreReference(String diskStoreReference) {
		this.diskStoreReference = diskStoreReference;
	}

	public String getDiskStoreReference() {
		return this.diskStoreReference;
	}

	public void setDiskSynchronous(Boolean diskSynchronous) {
		this.diskSynchronous = diskSynchronous;
	}

	public Boolean getDiskSynchronous() {
		return this.diskSynchronous;
	}

	public void setDispatcherThreads(Integer dispatcherThreads) {
		this.dispatcherThreads = dispatcherThreads;
	}

	public Integer getDispatcherThreads() {
		return this.dispatcherThreads;
	}

	public void setEventFilters(List<GatewayEventFilter> eventFilters) {
		this.eventFilters = eventFilters;
	}

	public List<GatewayEventFilter> getEventFilters() {
		return this.eventFilters;
	}

	public void setEventSubstitutionFilter(GatewayEventSubstitutionFilter eventSubstitutionFilter) {
		this.eventSubstitutionFilter = eventSubstitutionFilter;
	}

	public GatewayEventSubstitutionFilter getEventSubstitutionFilter() {
		return this.eventSubstitutionFilter;
	}

	public void setManualStart(boolean manualStart) {
		this.manualStart = manualStart;
	}

	public void setManualStart(Boolean manualStart) {
		setManualStart(Boolean.TRUE.equals(manualStart));
	}

	public boolean isManualStart() {
		return this.manualStart;
	}

	public void setMaximumQueueMemory(Integer maximumQueueMemory) {
		this.maximumQueueMemory = maximumQueueMemory;
	}

	public Integer getMaximumQueueMemory() {
		return this.maximumQueueMemory;
	}

	public void setOrderPolicy(GatewaySender.OrderPolicy orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

	public void setOrderPolicy(OrderPolicyType orderPolicy) {
		setOrderPolicy(orderPolicy != null ? orderPolicy.getOrderPolicy() : null);
	}

	public GatewaySender.OrderPolicy getOrderPolicy() {
		return this.orderPolicy;
	}

	public void setParallel(Boolean parallel) {
		this.parallel = parallel;
	}

	public boolean isParallelGatewaySender() {
		return Boolean.TRUE.equals(this.parallel);
	}

	public boolean isSerialGatewaySender() {
		return !isParallelGatewaySender();
	}

	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
	}

	public boolean isPersistent() {
		return Boolean.TRUE.equals(this.persistent);
	}

	public boolean isNotPersistent() {
		return !isPersistent();
	}

	public void setRemoteDistributedSystemId(int remoteDistributedSystemId) {
		this.remoteDistributedSystemId = remoteDistributedSystemId;
	}

	public void setRegions(String[] regions) {
		setRegions(Arrays.asList(ArrayUtils.nullSafeArray(regions, String.class)));
	}

	public void setRegions(List<String> regions) {
		this.regions.addAll(CollectionUtils.nullSafeList(regions));
	}

	public List<String> getRegions() {
		return Collections.unmodifiableList(this.regions);
	}

	public int getRemoteDistributedSystemId() {
		return this.remoteDistributedSystemId;
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public Integer getSocketBufferSize() {
		return this.socketBufferSize;
	}

	public void setSocketReadTimeout(Integer socketReadTimeout) {
		this.socketReadTimeout = socketReadTimeout;
	}

	public Integer getSocketReadTimeout() {
		return this.socketReadTimeout;
	}

	public void setTransportFilters(List<GatewayTransportFilter> transportFilters) {
		this.transportFilters = transportFilters;
	}

	public List<GatewayTransportFilter> getTransportFilters() {
		return this.transportFilters;
	}
}
