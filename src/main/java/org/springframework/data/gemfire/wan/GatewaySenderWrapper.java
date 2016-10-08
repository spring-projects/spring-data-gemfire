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

import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.cache.wan.GatewayTransportFilter;
import org.springframework.util.Assert;

/**
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.util.Gateway
 * @see org.apache.geode.cache.wan.GatewaySender
 */
public class GatewaySenderWrapper implements GatewaySender {

    private boolean manualStart;

    private final GatewaySender delegate;

	public GatewaySenderWrapper(final GatewaySender gatewaySender) {
		Assert.notNull(gatewaySender, "The target Gateway Sender must not be null.");
		this.delegate = gatewaySender;
	}

    @Override
    public void start() {
        delegate.start();
    }

    @Override
    public void stop() {
        delegate.stop();
    }

    @Override
    public void pause() {
        delegate.pause();
    }

    @Override
    public void resume() {
        delegate.resume();
    }

	@Override
	public void rebalance() {
		delegate.rebalance();
	}

	@Override
    public boolean isRunning() {
        return delegate.isRunning();
    }

    @Override
    public boolean isPaused() {
        return delegate.isPaused();
    }

    @Override
    public void addGatewayEventFilter(GatewayEventFilter filter) {
         delegate.addGatewayEventFilter(filter);
    }

    @Override
    public void removeGatewayEventFilter(GatewayEventFilter filter) {
        delegate.removeGatewayEventFilter(filter);
    }

    @Override
    public String getId() {
        return delegate.getId();
    }

    @Override
    public int getRemoteDSId() {
        return delegate.getRemoteDSId();
    }

    @Override
    public int getSocketBufferSize() {
        return delegate.getSocketBufferSize();
    }

    @Override
    public int getSocketReadTimeout() {
        return delegate.getSocketReadTimeout();
    }

    @Override
    public String getDiskStoreName() {
        return delegate.getDiskStoreName();
    }

    @Override
    public int getMaximumQueueMemory() {
        return delegate.getMaximumQueueMemory();
    }

    @Override
    public int getBatchSize() {
        return delegate.getBatchSize();
    }

    @Override
    public int getBatchTimeInterval() {
        return delegate.getBatchTimeInterval();
    }

    @Override
    public boolean isBatchConflationEnabled() {
        return delegate.isBatchConflationEnabled();
    }

    @Override
    public boolean isPersistenceEnabled() {
        return delegate.isPersistenceEnabled();
    }

    @Override
    public int getAlertThreshold() {
        return delegate.getAlertThreshold();
    }

    @Override
    public List<GatewayEventFilter> getGatewayEventFilters() {
        return delegate.getGatewayEventFilters();
    }

	public GatewayEventSubstitutionFilter getGatewayEventSubstitutionFilter() {
		return delegate.getGatewayEventSubstitutionFilter();
	}

	@Override
    public List<GatewayTransportFilter> getGatewayTransportFilters() {
        return delegate.getGatewayTransportFilters();
    }

    @Override
    public boolean isDiskSynchronous() {
        return delegate.isDiskSynchronous();
    }

    @Override
    public boolean isManualStart() {
        return this.manualStart;
    }

    @Override
    public boolean isParallel() {
        return delegate.isParallel();
    }

    @Override
    public int getDispatcherThreads() {
        return delegate.getDispatcherThreads();
    }

    @Override
	@SuppressWarnings("deprecation")
    public GatewaySender.OrderPolicy getOrderPolicy() {
        return delegate.getOrderPolicy();
    }

    public void setManualStart(boolean manualStart) {
        this.manualStart = manualStart;
    }

	@Override
	public int getMaxParallelismForReplicatedRegion() {
		return delegate.getMaxParallelismForReplicatedRegion();
	}

	@Override
	public String toString() {
		return this.delegate.toString();
	}

}
