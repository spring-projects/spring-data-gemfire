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
package org.springframework.data.gemfire.wan;

import java.util.Arrays;
import java.util.List;

import org.springframework.context.SmartLifecycle;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.wan.GatewayEventFilter;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewaySenderFactory;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;

/**
 * FactoryBean for creating a parallel or serial GemFire {@link GatewaySender}.
 * <p/>
 * @author David Turanski
 * @author John Blum
 */
public class GatewaySenderFactoryBean extends AbstractWANComponentFactoryBean<GatewaySender>
		implements SmartLifecycle {

	private static final List<String> VALID_ORDER_POLICIES = Arrays.asList("KEY", "PARTITION", "THREAD");

	private boolean manualStart = false;

	private int remoteDistributedSystemId;

	private GatewaySender gatewaySender;

	private List<GatewayEventFilter> eventFilters;

	private List<GatewayTransportFilter> transportFilters;

	private Boolean diskSynchronous;
	private Boolean enableBatchConflation;
	private Boolean parallel;
	private Boolean persistent;

	private Integer alertThreshold;
	private Integer batchSize;
	private Integer batchTimeInterval;
	private Integer dispatcherThreads;
	private Integer maximumQueueMemory;
	private Integer socketBufferSize;
	private Integer socketReadTimeout;

	private String diskStoreRef;
	private String orderPolicy;

	/**
	 * Constructs an instance of the GatewaySenderFactoryBean class initialized with a reference to the GemFire cache.
	 * <p/>
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
		return GatewaySender.class;
	}

	@Override
	protected void doInit() {
		GatewaySenderFactory gatewaySenderFactory = (this.factory != null ? (GatewaySenderFactory) factory :
			cache.createGatewaySenderFactory());

		if (diskStoreRef != null) {
			persistent = (persistent == null || persistent);
			Assert.isTrue(persistent, "Specifying a disk store requires the persistent property to be true.");
			gatewaySenderFactory.setDiskStoreName(diskStoreRef);
		}

		if (diskSynchronous != null) {
			persistent = (persistent == null || persistent);
			Assert.isTrue(persistent, "Specifying disk synchronous requires the persistent property to be true.");
			gatewaySenderFactory.setDiskSynchronous(diskSynchronous);
		}

		if (persistent != null) {
			gatewaySenderFactory.setPersistenceEnabled(persistent);
		}

		parallel = Boolean.TRUE.equals(parallel);
		gatewaySenderFactory.setParallel(parallel);

		if (orderPolicy != null) {
			Assert.isTrue(isSerialGatewaySender(), "Order Policy cannot be used with a Parallel Gateway Sender Queue.");

			Assert.isTrue(VALID_ORDER_POLICIES.contains(orderPolicy.toUpperCase()),
				String.format("The value for Order Policy '%1$s' is invalid.", orderPolicy));

			gatewaySenderFactory.setOrderPolicy(Gateway.OrderPolicy.valueOf(orderPolicy.toUpperCase()));
		}

		if (!CollectionUtils.isEmpty(eventFilters)) {
			for (GatewayEventFilter eventFilter : eventFilters) {
				gatewaySenderFactory.addGatewayEventFilter(eventFilter);
			}
		}

		if (!CollectionUtils.isEmpty(transportFilters)) {
			for (GatewayTransportFilter transportFilter : transportFilters) {
				gatewaySenderFactory.addGatewayTransportFilter(transportFilter);
			}
		}

		if (alertThreshold != null) {
			gatewaySenderFactory.setAlertThreshold(alertThreshold);
		}
		if (enableBatchConflation != null) {
			gatewaySenderFactory.setBatchConflationEnabled(enableBatchConflation);
		}
		if (batchSize != null) {
			gatewaySenderFactory.setBatchSize(batchSize);
		}
		if (batchTimeInterval != null) {
			gatewaySenderFactory.setBatchTimeInterval(batchTimeInterval);
		}
		if (dispatcherThreads != null) {
			Assert.isTrue(isSerialGatewaySender(),
				"The number of Dispatcher Threads cannot be specified with a Parallel Gateway Sender Queue.");
			gatewaySenderFactory.setDispatcherThreads(dispatcherThreads);
		}

		gatewaySenderFactory.setManualStart(true);

		if (maximumQueueMemory != null) {
			gatewaySenderFactory.setMaximumQueueMemory(maximumQueueMemory);
		}
		if (socketBufferSize != null) {
			gatewaySenderFactory.setSocketBufferSize(socketBufferSize);
		}
		if (socketReadTimeout != null) {
			gatewaySenderFactory.setSocketReadTimeout(socketReadTimeout);
		}

		GatewaySenderWrapper wrapper = new GatewaySenderWrapper(gatewaySenderFactory.create(getName(),
			remoteDistributedSystemId));
        wrapper.setManualStart(manualStart);
        gatewaySender = wrapper;
	}

	public void setRemoteDistributedSystemId(int remoteDistributedSystemId) {
		this.remoteDistributedSystemId = remoteDistributedSystemId;
	}

	public void setEventFilters(List<GatewayEventFilter> gatewayEventFilters) {
		this.eventFilters = gatewayEventFilters;
	}

	public void setTransportFilters(List<GatewayTransportFilter> gatewayTransportFilters) {
		this.transportFilters = gatewayTransportFilters;
	}

	public void setAlertThreshold(Integer alertThreshold) {
		this.alertThreshold = alertThreshold;
	}

	public void setEnableBatchConflation(Boolean enableBatchConflation) {
		this.enableBatchConflation = enableBatchConflation;
	}

	public void setBatchSize(Integer batchSize) {
		this.batchSize = batchSize;
	}

	public void setBatchTimeInterval(Integer batchTimeInterval) {
		this.batchTimeInterval = batchTimeInterval;
	}

	public void setDiskStoreRef(String diskStoreRef) {
		this.diskStoreRef = diskStoreRef;
	}

	public void setDiskSynchronous(Boolean diskSynchronous) {
		this.diskSynchronous = diskSynchronous;
	}

	public void setDispatcherThreads(Integer dispatcherThreads) {
		this.dispatcherThreads = dispatcherThreads;
	}

	public void setManualStart(Boolean manualStart) {
		this.manualStart = manualStart;
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

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public void setSocketReadTimeout(Integer socketReadTimeout) {
		this.socketReadTimeout = socketReadTimeout;
	}

	/* (non-Javadoc)
	 * @see org.springframework.context.Lifecycle#start()
	 */
	@Override
	public synchronized void start() {
		if (!gatewaySender.isRunning()){
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
	 * @see org.springframework.context.Lifecycle#isRunning()
	 */
	@Override
	public boolean isRunning() {
		return gatewaySender.isRunning();
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
		return !manualStart;
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
