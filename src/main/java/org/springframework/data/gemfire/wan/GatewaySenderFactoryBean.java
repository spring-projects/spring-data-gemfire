/*
 * Copyright 2010-2013 the original author or authors.
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

import org.springframework.context.SmartLifecycle;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.wan.GatewayEventFilter;
import com.gemstone.gemfire.cache.wan.GatewayEventSubstitutionFilter;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewaySenderFactory;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;

/**
 * FactoryBean for creating a parallel or serial GemFire {@link GatewaySender}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.context.SmartLifecycle
 * @see org.springframework.data.gemfire.wan.AbstractWANComponentFactoryBean
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.util.Gateway
 * @see com.gemstone.gemfire.cache.wan.GatewaySender
 * @see com.gemstone.gemfire.cache.wan.GatewaySenderFactory
 * @since 1.2.2
 */
@SuppressWarnings("unused")
public class GatewaySenderFactoryBean extends AbstractWANComponentFactoryBean<GatewaySender>
		implements SmartLifecycle {

	private boolean manualStart = false;

	private int remoteDistributedSystemId;

	private GatewaySender gatewaySender;

	private List<GatewayEventFilter> eventFilters;

	private List<GatewayTransportFilter> transportFilters;

	private Boolean diskSynchronous;
	private Boolean batchConflationEnabled;
	private Boolean parallel;
	private Boolean persistent;

	private GatewayEventSubstitutionFilter eventSubstitutionFilter;

	private Integer alertThreshold;
	private Integer batchSize;
	private Integer batchTimeInterval;
	private Integer dispatcherThreads;
	private Integer maximumQueueMemory;
	private Integer socketBufferSize;
	private Integer socketReadTimeout;

	private String diskStoreReference;
	private String orderPolicy;

	/**
	 * Constructs an instance of the GatewaySenderFactoryBean class initialized with a reference to the GemFire cache.
	 *
	 * @param cache the Gemfire cache reference.
	 * @see com.gemstone.gemfire.cache.Cache
	 */
	public GatewaySenderFactoryBean(final Cache cache) {
		super(cache);
	}

	@Override
	public GatewaySender getObject() throws Exception {
		return gatewaySender;
	}

	@Override
	public Class<?> getObjectType() {
		return (gatewaySender != null ? gatewaySender.getClass() : GatewaySender.class);
	}

	@Override
	protected void doInit() {
		GatewaySenderFactory gatewaySenderFactory = (this.factory != null ? (GatewaySenderFactory) factory
			: cache.createGatewaySenderFactory());

		if (alertThreshold != null) {
			gatewaySenderFactory.setAlertThreshold(alertThreshold);
		}

		if (batchSize != null) {
			gatewaySenderFactory.setBatchSize(batchSize);
		}

		if (batchTimeInterval != null) {
			gatewaySenderFactory.setBatchTimeInterval(batchTimeInterval);
		}

		if (diskStoreReference != null) {
			gatewaySenderFactory.setDiskStoreName(diskStoreReference);
		}

		if (diskSynchronous != null) {
			gatewaySenderFactory.setDiskSynchronous(diskSynchronous);
		}

		if (dispatcherThreads != null) {
			gatewaySenderFactory.setDispatcherThreads(dispatcherThreads);
		}

		if (batchConflationEnabled != null) {
			gatewaySenderFactory.setBatchConflationEnabled(batchConflationEnabled);
		}

		if (!CollectionUtils.isEmpty(eventFilters)) {
			for (GatewayEventFilter eventFilter : eventFilters) {
				gatewaySenderFactory.addGatewayEventFilter(eventFilter);
			}
		}

		if (eventSubstitutionFilter != null) {
			gatewaySenderFactory.setGatewayEventSubstitutionFilter(eventSubstitutionFilter);
		}

		gatewaySenderFactory.setManualStart(true);

		if (maximumQueueMemory != null) {
			gatewaySenderFactory.setMaximumQueueMemory(maximumQueueMemory);
		}

		if (orderPolicy != null) {
			Assert.isTrue(isSerialGatewaySender(), "Order Policy cannot be used with a Parallel Gateway Sender Queue.");

			Assert.isTrue(VALID_ORDER_POLICIES.contains(orderPolicy.toUpperCase()),
				String.format("The value for Order Policy '%1$s' is invalid.", orderPolicy));

			gatewaySenderFactory.setOrderPolicy(Gateway.OrderPolicy.valueOf(orderPolicy.toUpperCase()));
		}

		gatewaySenderFactory.setParallel(isParallelGatewaySender());
		gatewaySenderFactory.setPersistenceEnabled(isPersistent());

		if (socketBufferSize != null) {
			gatewaySenderFactory.setSocketBufferSize(socketBufferSize);
		}

		if (socketReadTimeout != null) {
			gatewaySenderFactory.setSocketReadTimeout(socketReadTimeout);
		}

		if (!CollectionUtils.isEmpty(transportFilters)) {
			for (GatewayTransportFilter transportFilter : transportFilters) {
				gatewaySenderFactory.addGatewayTransportFilter(transportFilter);
			}
		}

		GatewaySenderWrapper wrapper = new GatewaySenderWrapper(gatewaySenderFactory.create(getName(),
			remoteDistributedSystemId));

        wrapper.setManualStart(manualStart);
        gatewaySender = wrapper;
	}

	public void setAlertThreshold(Integer alertThreshold) {
		this.alertThreshold = alertThreshold;
	}

	public void setBatchConflationEnabled(Boolean batchConflationEnabled) {
		this.batchConflationEnabled = batchConflationEnabled;
	}

	/**
	 * Boolean value that determines whether GemFire should conflate messages.
	 *
	 * @param enableBatchConflation a boolean value indicating whether GemFire should conflate messages in the Queue.
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

	public void setEventSubstitutionFilter(final GatewayEventSubstitutionFilter eventSubstitutionFilter) {
		this.eventSubstitutionFilter = eventSubstitutionFilter;
	}

	public void setManualStart(Boolean manualStart) {
		this.manualStart = Boolean.TRUE.equals(manualStart);
	}

	public void setMaximumQueueMemory(Integer maximumQueueMemory) {
		this.maximumQueueMemory = maximumQueueMemory;
	}

	public void setOrderPolicy(String orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

	public void setParallel(Boolean parallel) {
		this.parallel = parallel;
	}

	public boolean isSerialGatewaySender() {
		return !isParallelGatewaySender();
	}

	public boolean isParallelGatewaySender() {
		return Boolean.TRUE.equals(parallel);
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

	/* (non-Javadoc)
	 * @see org.springframework.context.SmartLifecycle#isAutoStartup()
	 */
	@Override
	public boolean isAutoStartup() {
		return !manualStart;
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.Phased#getPhase()
	 */
	@Override
	public int getPhase() {
		return Integer.MAX_VALUE;
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.Lifecycle#isRunning()
	 */
	@Override
	public boolean isRunning() {
		return gatewaySender.isRunning();
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.Lifecycle#start()
	 */
	@Override
	public synchronized void start() {
		Assert.notNull(gatewaySender, "The GatewaySender was not properly configured and initialized!");

		if (!isRunning()){
			gatewaySender.start();
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.Lifecycle#stop()
	 */
	@Override
	public void stop() {
		gatewaySender.stop();
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
