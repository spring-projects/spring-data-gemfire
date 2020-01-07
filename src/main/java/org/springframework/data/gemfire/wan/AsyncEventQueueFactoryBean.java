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

import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.asyncqueue.AsyncEvent;
import org.apache.geode.cache.asyncqueue.AsyncEventListener;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewaySender;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;

/**
 * Spring {@link FactoryBean} for creating Apache Geode/Pivotal GemFire {@link AsyncEventQueue AsyncEventQueues}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.asyncqueue.AsyncEventListener
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @see org.springframework.data.gemfire.wan.AbstractWANComponentFactoryBean
 */
@SuppressWarnings("unused")
public class AsyncEventQueueFactoryBean extends AbstractWANComponentFactoryBean<AsyncEventQueue> {

	private AsyncEventListener asyncEventListener;

	private AsyncEventQueue asyncEventQueue;

	private Boolean batchConflationEnabled;
	private Boolean diskSynchronous;
	private Boolean forwardExpirationDestroy;
	private Boolean parallel;
	private Boolean persistent;
	private Boolean pauseEventDispatching;

	private Integer batchSize;
	private Integer batchTimeInterval;
	private Integer dispatcherThreads;
	private Integer maximumQueueMemory;

	private GatewayEventSubstitutionFilter gatewayEventSubstitutionFilter;

	private GatewaySender.OrderPolicy orderPolicy;

	private List<GatewayEventFilter> gatewayEventFilters;

	private String diskStoreReference;

	/**
	 * Constructs an instance of the AsyncEventQueueFactoryBean for creating an Pivotal GemFire AsyncEventQueue.
	 *
	 * @param cache the Pivotal GemFire Cache reference.
	 * @see #AsyncEventQueueFactoryBean(org.apache.geode.cache.Cache, org.apache.geode.cache.asyncqueue.AsyncEventListener)
	 */
	public AsyncEventQueueFactoryBean(final Cache cache) {
		this(cache, null);
	}

	/**
	 * Constructs an instance of the AsyncEventQueueFactoryBean for creating an Pivotal GemFire AsyncEventQueue.
	 *
	 * @param cache the Pivotal GemFire Cache reference.
	 * @param asyncEventListener required {@link AsyncEventListener}
	 */
	public AsyncEventQueueFactoryBean(Cache cache, AsyncEventListener asyncEventListener) {

		super(cache);

		setAsyncEventListener(asyncEventListener);
	}

	@Override
	public AsyncEventQueue getObject() throws Exception {
		return this.asyncEventQueue;
	}

	@Override
	public Class<?> getObjectType() {
		return this.asyncEventQueue != null ? this.asyncEventQueue.getClass() : AsyncEventQueue.class;
	}

	@Override
	protected void doInit() {

		AsyncEventListener listener = getAsyncEventListener();

		Assert.state(listener != null, "AsyncEventListener must not be null");

		AsyncEventQueueFactory asyncEventQueueFactory = resolveAsyncEventQueueFactory();

		Optional.ofNullable(this.batchConflationEnabled).ifPresent(asyncEventQueueFactory::setBatchConflationEnabled);
		Optional.ofNullable(this.batchSize).ifPresent(asyncEventQueueFactory::setBatchSize);
		Optional.ofNullable(this.batchTimeInterval).ifPresent(asyncEventQueueFactory::setBatchTimeInterval);
		Optional.ofNullable(this.diskStoreReference).ifPresent(asyncEventQueueFactory::setDiskStoreName);
		Optional.ofNullable(this.diskSynchronous).ifPresent(asyncEventQueueFactory::setDiskSynchronous);
		Optional.ofNullable(this.dispatcherThreads).ifPresent(asyncEventQueueFactory::setDispatcherThreads);
		Optional.ofNullable(this.forwardExpirationDestroy).ifPresent(asyncEventQueueFactory::setForwardExpirationDestroy);
		Optional.ofNullable(this.gatewayEventSubstitutionFilter).ifPresent(asyncEventQueueFactory::setGatewayEventSubstitutionListener);
		Optional.ofNullable(this.maximumQueueMemory).ifPresent(asyncEventQueueFactory::setMaximumQueueMemory);
		Optional.ofNullable(this.persistent).ifPresent(asyncEventQueueFactory::setPersistent);

		if (isPauseEventDispatching()) {
			asyncEventQueueFactory.pauseEventDispatching();
		}

		asyncEventQueueFactory.setParallel(isParallelEventQueue());

		if (this.orderPolicy != null) {

			Assert.state(isSerialEventQueue(), "OrderPolicy cannot be used with a Parallel AsyncEventQueue");

			asyncEventQueueFactory.setOrderPolicy(this.orderPolicy);
		}

		CollectionUtils.nullSafeList(this.gatewayEventFilters).forEach(asyncEventQueueFactory::addGatewayEventFilter);

		setAsyncEventQueue(asyncEventQueueFactory.create(getName(), listener));
	}

	private AsyncEventQueueFactory resolveAsyncEventQueueFactory() {
		return this.factory != null ? (AsyncEventQueueFactory) this.factory : this.cache.createAsyncEventQueueFactory();
	}

	@Override
	public void destroy() {

		if (!getCache().isClosed()) {
			SpringUtils.safeDoOperation(() -> this.asyncEventListener.close());
		}
	}

	/**
	 * Configures the {@link AsyncEventListener} called when {@link AsyncEvent AsyncEvents} are enqueued into
	 * the {@link AsyncEventQueue} created by this {@link FactoryBean}.
	 *
	 * @param listener the configured {@link AsyncEventListener}.
	 * @throws IllegalStateException if the {@link AsyncEventQueue} has already bean created.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventListener
	 */
	public final void setAsyncEventListener(AsyncEventListener listener) {

		Assert.state(this.asyncEventQueue == null,
			"Setting an AsyncEventListener is not allowed once the AsyncEventQueue has been created");

		this.asyncEventListener = listener;
	}

	/**
	 * Returns the configured {@link AsyncEventListener} for the {@link AsyncEventQueue}
	 * returned by this {@link FactoryBean}.
	 *
	 * @return the configured {@link AsyncEventListener}.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventListener
	 * @see #setAsyncEventListener(AsyncEventListener)
	 */
	public AsyncEventListener getAsyncEventListener() {
		return this.asyncEventListener;
	}

	/**
	 * Configures the {@link AsyncEventQueue} returned by this {@link FactoryBean}.
	 *
	 * @param asyncEventQueue overrides the {@link AsyncEventQueue} returned by this {@link FactoryBean}.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
	 */
	public void setAsyncEventQueue(AsyncEventQueue asyncEventQueue) {
		this.asyncEventQueue = asyncEventQueue;
	}

	/**
	 * Returns the {@link AsyncEventQueue} created by this {@link FactoryBean}.
	 *
	 * @return a reference to the {@link AsyncEventQueue} created by this {@link FactoryBean}.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
	 */
	public AsyncEventQueue getAsyncEventQueue() {
		return this.asyncEventQueue;
	}

	/**
	 * Enable or disable {@link AsyncEventQueue} (AEQ) message conflation.
	 *
	 * @param batchConflationEnabled {@link Boolean} indicating whether to conflate queued events.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory#setBatchConflationEnabled(boolean)
	 */
	public void setBatchConflationEnabled(Boolean batchConflationEnabled) {
		this.batchConflationEnabled = batchConflationEnabled;
	}

	public void setBatchSize(Integer batchSize) {
		this.batchSize = batchSize;
	}

	/**
	 * Configures the {@link AsyncEventQueue} (AEQ) interval between sending batches.
	 *
	 * @param batchTimeInterval {@link Integer} specifying the maximum number of milliseconds
	 * that can elapse between sending batches.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory#setBatchTimeInterval(int)
	 */
	public void setBatchTimeInterval(Integer batchTimeInterval) {
		this.batchTimeInterval = batchTimeInterval;
	}

	public void setDiskStoreRef(String diskStoreRef) {
		this.diskStoreReference = diskStoreRef;
	}

	/**
	 * Configures the {@link AsyncEventQueue} (AEQ) disk write synchronization policy.
	 *
	 * @param diskSynchronous boolean value indicating whether disk writes are synchronous.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory#setDiskSynchronous(boolean)
	 */
	public void setDiskSynchronous(Boolean diskSynchronous) {
		this.diskSynchronous = diskSynchronous;
	}

	/**
	 * Configures the number of dispatcher threads used to process Region Events
	 * from the associated {@link AsyncEventQueue} (AEQ).
	 *
	 * @param dispatcherThreads {@link Integer} specifying the number of dispatcher threads used
	 * to process {@link Region} events from the associated queue.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory#setDispatcherThreads(int)
	 */
	public void setDispatcherThreads(Integer dispatcherThreads) {
		this.dispatcherThreads = dispatcherThreads;
	}

	/**
	 * Forwards expiration (action-based) destroy events to the {@link AsyncEventQueue} (AEQ).
	 *
	 * By default, destroy events are not added to the AEQ.  Setting this attribute to {@literal true}
	 * will add all expiration destroy events to the AEQ.
	 *
	 * @param forwardExpirationDestroy boolean value indicating whether to forward expiration destroy events.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory#setForwardExpirationDestroy(boolean)
	 * @see org.apache.geode.cache.ExpirationAttributes#getAction()
	 * @see org.apache.geode.cache.ExpirationAction#DESTROY
	 */
	public void setForwardExpirationDestroy(Boolean forwardExpirationDestroy) {
		this.forwardExpirationDestroy = forwardExpirationDestroy;
	}

	public void setGatewayEventFilters(List<GatewayEventFilter> eventFilters) {
		this.gatewayEventFilters = eventFilters;
	}

	public void setGatewayEventSubstitutionFilter(GatewayEventSubstitutionFilter eventSubstitutionFilter) {
		this.gatewayEventSubstitutionFilter = eventSubstitutionFilter;
	}

	public void setMaximumQueueMemory(Integer maximumQueueMemory) {
		this.maximumQueueMemory = maximumQueueMemory;
	}

	/**
	 * Configures the {@link AsyncEventQueue} (AEQ) ordering policy (e.g. {@literal KEY}, {@literal PARTITION},
	 * {@literal THREAD}).
	 *
	 * When dispatcher threads are greater than one, the ordering policy configures the way in which
	 * multiple dispatcher threads process Region events from the queue.
	 *
	 * @param orderPolicy {@link String} specifying the name of the AEQ order policy.
	 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory#setOrderPolicy(GatewaySender.OrderPolicy)
	 */
	public void setOrderPolicy(String orderPolicy) {
		setOrderPolicy(GatewaySender.OrderPolicy.valueOf(String.valueOf(orderPolicy).toUpperCase()));
	}

	public void setOrderPolicy(GatewaySender.OrderPolicy orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

	public void setParallel(Boolean parallel) {
		this.parallel = parallel;
	}

	public boolean isParallelEventQueue() {
		return Boolean.TRUE.equals(parallel);
	}

	public void setPauseEventDispatching(Boolean pauseEventDispatching) {
		this.pauseEventDispatching = pauseEventDispatching;
	}

	public boolean isPauseEventDispatching() {
		return Boolean.TRUE.equals(this.pauseEventDispatching);
	}

	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
	}

	public boolean isSerialEventQueue() {
		return !isParallelEventQueue();
	}
}
