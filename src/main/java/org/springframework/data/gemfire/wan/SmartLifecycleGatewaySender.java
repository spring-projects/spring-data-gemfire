/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.wan;

import java.util.List;

import org.springframework.context.SmartLifecycle;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.util.Gateway.OrderPolicy;
import com.gemstone.gemfire.cache.wan.GatewayEventFilter;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;

/**
 * A {@link GatewaySender} controlled by {@link SmartLifecycle}
 * @author David Turanski
 *
 */
public class SmartLifecycleGatewaySender implements GatewaySender, SmartLifecycle {
	
	private final GatewaySender delegate;
	private final boolean autoStartup;

	public SmartLifecycleGatewaySender(GatewaySender delegate, boolean autoStartup) {
		Assert.notNull(delegate);
		this.delegate = delegate;
		this.autoStartup = autoStartup; 
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#addGatewayEventFilter(com.gemstone.gemfire.cache.wan.GatewayEventFilter)
	 */
	@Override
	public void addGatewayEventFilter(GatewayEventFilter eventFilter) {
		this.delegate.addGatewayEventFilter(eventFilter);
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getAlertThreshold()
	 */
	@Override
	public int getAlertThreshold() {
		return this.delegate.getAlertThreshold();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getBatchSize()
	 */
	@Override
	public int getBatchSize() {
		return this.delegate.getBatchSize();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getBatchTimeInterval()
	 */
	@Override
	public int getBatchTimeInterval() {
		return this.delegate.getBatchTimeInterval();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getDiskStoreName()
	 */
	@Override
	public String getDiskStoreName() {
		return this.delegate.getDiskStoreName();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getDispatcherThreads()
	 */
	@Override
	public int getDispatcherThreads() {
		return this.delegate.getDispatcherThreads();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getGatewayEventFilters()
	 */
	@Override
	public List<GatewayEventFilter> getGatewayEventFilters() {
		return this.delegate.getGatewayEventFilters();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getGatewayTransportFilters()
	 */
	@Override
	public List<GatewayTransportFilter> getGatewayTransportFilters() {
		return this.delegate.getGatewayTransportFilters();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getId()
	 */
	@Override
	public String getId() {
		return this.delegate.getId();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getMaximumQueueMemory()
	 */
	@Override
	public int getMaximumQueueMemory() {
		return this.delegate.getMaximumQueueMemory();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getOrderPolicy()
	 */
	@Override
	public OrderPolicy getOrderPolicy() {
		return this.delegate.getOrderPolicy();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getRemoteDSId()
	 */
	@Override
	public int getRemoteDSId() {
		return this.delegate.getRemoteDSId();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getSocketBufferSize()
	 */
	@Override
	public int getSocketBufferSize() {
		return this.delegate.getSocketBufferSize();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#getSocketReadTimeout()
	 */
	@Override
	public int getSocketReadTimeout() {
		return this.delegate.getSocketReadTimeout();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#isBatchConflationEnabled()
	 */
	@Override
	public boolean isBatchConflationEnabled() {
		return this.delegate.isBatchConflationEnabled();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#isDiskSynchronous()
	 */
	@Override
	public boolean isDiskSynchronous() {
		return this.delegate.isDiskSynchronous();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#isManualStart()
	 */
	@Override
	public boolean isManualStart() {
		return true;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#isParallel()
	 */
	@Override
	public boolean isParallel() {
		return this.delegate.isParallel();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#isPaused()
	 */
	@Override
	public boolean isPaused() {
		return this.delegate.isPaused();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#isPersistenceEnabled()
	 */
	@Override
	public boolean isPersistenceEnabled() {
		return this.delegate.isPersistenceEnabled();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#isRunning()
	 */
	@Override
	public boolean isRunning() {
		return this.delegate.isRunning();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#pause()
	 */
	@Override
	public void pause() {
		this.delegate.pause();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#removeGatewayEventFilter(com.gemstone.gemfire.cache.wan.GatewayEventFilter)
	 */
	@Override
	public void removeGatewayEventFilter(GatewayEventFilter eventFilter) {
		this.delegate.removeGatewayEventFilter(eventFilter);	
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#resume()
	 */
	@Override
	public void resume() {
		this.delegate.resume();
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#start()
	 */
	@Override
	public void start() {
		this.delegate.start();
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender#stop()
	 */
	@Override
	public void stop() {
		this.delegate.stop();
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
		return this.autoStartup ;
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
