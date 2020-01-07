/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
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
 * {@link GatewaySenderWrapper} is an Adapter around a {@link GatewaySender} providing the ability to control
 * manual start and stop.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.util.Gateway
 * @see org.apache.geode.cache.wan.GatewaySender
 */
public class GatewaySenderWrapper implements GatewaySender {

    private boolean manualStart;

    private final GatewaySender delegate;

    /**
     * Constructs an instance of {@link GatewaySenderWrapper} initialized with the given {@link GatewaySender} to adapt.
     *
     * @param gatewaySender {@link GatewaySender} to adapt.
     * @throws IllegalArgumentException if {@link GatewaySender} is {@literal null}.
     * @see org.apache.geode.cache.wan.GatewaySender
     */
    public GatewaySenderWrapper(GatewaySender gatewaySender) {
        Assert.notNull(gatewaySender, "GatewaySender must not be null");
        this.delegate = gatewaySender;
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean isBatchConflationEnabled() {
        return this.delegate.isBatchConflationEnabled();
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean isDiskSynchronous() {
        return this.delegate.isDiskSynchronous();
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean isManualStart() {
        return this.manualStart;
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean isParallel() {
        return this.delegate.isParallel();
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean isPaused() {
        return this.delegate.isPaused();
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean isPersistenceEnabled() {
        return this.delegate.isPersistenceEnabled();
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean isRunning() {
        return this.delegate.isRunning();
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getAlertThreshold() {
        return this.delegate.getAlertThreshold();
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getBatchSize() {
        return this.delegate.getBatchSize();
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getBatchTimeInterval() {
        return this.delegate.getBatchTimeInterval();
    }

    /**
     * @inheritDoc
     */
    @Override
    public String getDiskStoreName() {
        return this.delegate.getDiskStoreName();
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getDispatcherThreads() {
        return delegate.getDispatcherThreads();
    }

    /**
     * @inheritDoc
     */
    @Override
    public List<GatewayEventFilter> getGatewayEventFilters() {
        return this.delegate.getGatewayEventFilters();
    }

    /**
     * @inheritDoc
     */
    @Override
    public GatewayEventSubstitutionFilter getGatewayEventSubstitutionFilter() {
        return this.delegate.getGatewayEventSubstitutionFilter();
    }

    /**
     * @inheritDoc
     */
    @Override
    public List<GatewayTransportFilter> getGatewayTransportFilters() {
        return this.delegate.getGatewayTransportFilters();
    }

    /**
     * @inheritDoc
     */
    @Override
    public String getId() {
        return this.delegate.getId();
    }

    public void setManualStart(boolean manualStart) {
        this.manualStart = manualStart;
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getMaxParallelismForReplicatedRegion() {
        return this.delegate.getMaxParallelismForReplicatedRegion();
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getMaximumQueueMemory() {
        return this.delegate.getMaximumQueueMemory();
    }

    /**
     * @inheritDoc
     */
    @Override
    @SuppressWarnings("deprecation")
    public GatewaySender.OrderPolicy getOrderPolicy() {
        return delegate.getOrderPolicy();
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getRemoteDSId() {
        return this.delegate.getRemoteDSId();
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getSocketBufferSize() {
        return this.delegate.getSocketBufferSize();
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getSocketReadTimeout() {
        return this.delegate.getSocketReadTimeout();
    }

    /**
     * @inheritDoc
     */
    @Override
    public void addGatewayEventFilter(GatewayEventFilter filter) {
        delegate.addGatewayEventFilter(filter);
    }

    /**
     * @inheritDoc
     */
    @Override
    public void removeGatewayEventFilter(GatewayEventFilter filter) {
        delegate.removeGatewayEventFilter(filter);
    }

    /**
     * @inheritDoc
     */
    @Override
    public void destroy() {
        this.delegate.destroy();
    }

    /**
     * @inheritDoc
     */
    @Override
    public void pause() {
        delegate.pause();
    }

    /**
     * @inheritDoc
     */
    @Override
    public void rebalance() {
        delegate.rebalance();
    }

    /**
     * @inheritDoc
     */
    @Override
    public void resume() {
        delegate.resume();
    }

    /**
     * @inheritDoc
     */
    @Override
    public void start() {
        delegate.start();
    }

    /**
     * @inheritDoc
     */
    @Override
    public void stop() {
        delegate.stop();
    }

    /**
     * @inheritDoc
     */
    @Override
    public String toString() {
        return this.delegate.toString();
    }
}
