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

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;

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

import org.apache.shiro.util.StringUtils;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.config.annotation.GatewaySenderConfigurer;
import org.springframework.data.gemfire.util.CollectionUtils;

/**
 * Spring {@link FactoryBean} for creating a parallel or serial Pivotal GemFire {@link GatewaySender}.
 *
 * @author David Turanski
 * @author John Blum
 * @author Udo Kohlmeyer
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

	private List<GatewaySenderConfigurer> gatewaySenderConfigurers = Collections.emptyList();

	private List<String> regions;

	/**
	 * Constructs an instance of the {@link GatewaySenderFactoryBean} class initialized with a reference to
	 * the Pivotal GemFire {@link Cache} used to configured and initialized a Pivotal GemFire {@link GatewaySender}.
	 *
	 * @param cache reference to the Pivotal GemFire {@link Cache} used to create the Pivotal GemFire {@link GatewaySender}.
	 * @see org.apache.geode.cache.Cache
	 */
	public GatewaySenderFactoryBean(GemFireCache cache) {
		super(cache);
		this.regions = Arrays.asList(new String[] {});
	}

	public GatewaySenderFactoryBean() {
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doInit() {

		GatewaySenderFactory gatewaySenderFactory = resolveGatewaySenderFactory();

		stream(nullSafeIterable(gatewaySenderConfigurers).spliterator(), false)
			.forEach(gatewaySenderConfigurer1 -> gatewaySenderConfigurer1.configure(getName(), this));

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

	public boolean isManualStart() {
		return manualStart;
	}

	public void setManualStart(boolean manualStart) {
		this.manualStart = manualStart;
	}

	public int getRemoteDistributedSystemId() {
		return remoteDistributedSystemId;
	}

	public void setRemoteDistributedSystemId(int remoteDistributedSystemId) {
		this.remoteDistributedSystemId = remoteDistributedSystemId;
	}

	public GatewaySender getGatewaySender() {
		return gatewaySender;
	}

	public void setGatewaySender(GatewaySender gatewaySender) {
		this.gatewaySender = gatewaySender;
	}

	public List<GatewayEventFilter> getEventFilters() {
		return eventFilters;
	}

	public void setEventFilters(List<GatewayEventFilter> eventFilters) {
		this.eventFilters = eventFilters;
	}

	public List<GatewayTransportFilter> getTransportFilters() {
		return transportFilters;
	}

	public void setTransportFilters(List<GatewayTransportFilter> transportFilters) {
		this.transportFilters = transportFilters;
	}

	public Boolean getDiskSynchronous() {
		return diskSynchronous;
	}

	public void setDiskSynchronous(Boolean diskSynchronous) {
		this.diskSynchronous = diskSynchronous;
	}

	public void setManualStart(Boolean manualStart) {
		this.manualStart = Boolean.TRUE.equals(manualStart);
	}

	public void setBatchConflationEnabled(Boolean batchConflationEnabled) {
		this.batchConflationEnabled = batchConflationEnabled;
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

	public boolean isNotPersistent() {
		return !isPersistent();
	}

	public boolean isPersistent() {
		return Boolean.TRUE.equals(this.persistent);
	}

	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
	}

	public GatewaySender.OrderPolicy getOrderPolicy() {
		return orderPolicy;
	}

	public void setOrderPolicy(GatewaySender.OrderPolicy orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

	public GatewayEventSubstitutionFilter getEventSubstitutionFilter() {
		return eventSubstitutionFilter;
	}

	public void setEventSubstitutionFilter(GatewayEventSubstitutionFilter eventSubstitutionFilter) {
		this.eventSubstitutionFilter = eventSubstitutionFilter;
	}

	public Integer getAlertThreshold() {
		return alertThreshold;
	}

	public void setAlertThreshold(Integer alertThreshold) {
		this.alertThreshold = alertThreshold;
	}

	public Integer getBatchSize() {
		return batchSize;
	}

	public void setBatchSize(Integer batchSize) {
		this.batchSize = batchSize;
	}

	public Integer getBatchTimeInterval() {
		return batchTimeInterval;
	}

	public void setBatchTimeInterval(Integer batchTimeInterval) {
		this.batchTimeInterval = batchTimeInterval;
	}

	public Integer getDispatcherThreads() {
		return dispatcherThreads;
	}

	public void setDispatcherThreads(Integer dispatcherThreads) {
		this.dispatcherThreads = dispatcherThreads;
	}

	public Integer getMaximumQueueMemory() {
		return maximumQueueMemory;
	}

	public void setMaximumQueueMemory(Integer maximumQueueMemory) {
		this.maximumQueueMemory = maximumQueueMemory;
	}

	public Integer getSocketBufferSize() {
		return socketBufferSize;
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public Integer getSocketReadTimeout() {
		return socketReadTimeout;
	}

	public void setSocketReadTimeout(Integer socketReadTimeout) {
		this.socketReadTimeout = socketReadTimeout;
	}

	public String getDiskStoreReference() {
		return diskStoreReference;
	}

	public void setDiskStoreReference(String diskStoreReference) {
		this.diskStoreReference = diskStoreReference;
	}

	public void setGatewaySenderConfigurers(List<GatewaySenderConfigurer> gatewaySenderConfigurers) {
		this.gatewaySenderConfigurers = gatewaySenderConfigurers;
	}

	public void setDiskStoreRef(String diskStoreRef) {
		this.diskStoreReference = diskStoreRef;
	}

	private List<String> getRegions() {
		return regions;
	}

	public void setRegions(List<String> regions) {
		this.regions = regions;
	}

	public void setRegions(String[] regions) {
		this.regions = Arrays.asList(regions);
	}
}
