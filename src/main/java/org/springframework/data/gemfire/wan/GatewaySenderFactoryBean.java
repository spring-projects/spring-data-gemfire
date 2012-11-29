/*
 * Copyright 2010-2012 the original author or authors.
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

import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.wan.GatewayEventFilter;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewaySenderFactory;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;

/**
 * FactoryBean for creating a GemFire {@link GatewaySender}.
 * @author David Turanski
 * 
 */
public class GatewaySenderFactoryBean extends AbstractWANComponentFactoryBean<GatewaySender>  {
	private static List<String> validOrderPolicyValues = Arrays.asList("KEY", "PARTITION", "THREAD");

	private GatewaySender gatewaySender;

	private int remoteDistributedSystemId;

	private List<GatewayEventFilter> eventFilters;

	private List<GatewayTransportFilter> transportFilters;

	private Integer alertThreshold;

	private Boolean enableBatchConflation;

	private Integer batchSize;

	private Integer batchTimeInterval;

	private String diskStoreRef;

	private Boolean diskSynchronous;

	private Integer dispatcherThreads;

	private boolean manualStart = false;

	private Integer maximumQueueMemory;

	private String orderPolicy;

	private Boolean parallel;

	private Boolean persistent;

	private Integer socketBufferSize;

	private Integer socketReadTimeout;

	/**
	 * 
	 * @param cache the Gemfire cache
	 */
	public GatewaySenderFactoryBean(Cache cache) {
		super(cache);
	}

	@Override
	public GatewaySender getObject() throws Exception {
		return new SmartLifecycleGatewaySender(gatewaySender, !manualStart);
	}

	@Override
	public Class<?> getObjectType() {
		return SmartLifecycleGatewaySender.class;
	}

	@Override
	protected void doInit() {
		GatewaySenderFactory gatewaySenderFactory = null;
		if (this.factory == null) {
			gatewaySenderFactory = cache.createGatewaySenderFactory();
		} else {
			gatewaySenderFactory = (GatewaySenderFactory)factory;
		}
		if (diskStoreRef != null) {
			persistent = (persistent == null) ? Boolean.TRUE : persistent;
			Assert.isTrue(persistent, "specifying a disk store requires persistent property to be true");
			gatewaySenderFactory.setDiskStoreName(diskStoreRef);
		}

		if (diskSynchronous != null) {
			persistent = (persistent == null) ? Boolean.TRUE : persistent;
			Assert.isTrue(persistent, "specifying a disk synchronous requires persistent property to be true");
			gatewaySenderFactory.setDiskSynchronous(diskSynchronous);
		}

		if (persistent != null) {
			gatewaySenderFactory.setPersistenceEnabled(persistent);
		}

		parallel = (parallel == null) ? Boolean.FALSE : parallel;

		gatewaySenderFactory.setParallel(parallel);

		if (orderPolicy != null) {
			Assert.isTrue(parallel, "specifying an order policy requires the parallel property to be true");

			Assert.isTrue(validOrderPolicyValues.contains(orderPolicy.toUpperCase()), "The value of order policy:'"
					+ orderPolicy + "' is invalid");
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
		gatewaySender = gatewaySenderFactory.create(getName(), remoteDistributedSystemId);
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

	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public void setSocketReadTimeout(Integer socketReadTimeout) {
		this.socketReadTimeout = socketReadTimeout;
	}
}
