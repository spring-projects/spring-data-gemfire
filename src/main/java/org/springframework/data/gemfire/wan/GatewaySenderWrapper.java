package org.springframework.data.gemfire.wan;

import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.wan.GatewayEventFilter;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;

import java.util.List;

/**
 * Created by dturanski on 9/16/13.
 */
public class GatewaySenderWrapper implements GatewaySender {
    private final GatewaySender delegate;
    private boolean manualStart;

    public GatewaySenderWrapper(GatewaySender delegate) {
        this.delegate = delegate;
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
    public Gateway.OrderPolicy getOrderPolicy() {
        return delegate.getOrderPolicy();
    }

    public void setManualStart(boolean manualStart) {
        this.manualStart = manualStart;
    }
}
