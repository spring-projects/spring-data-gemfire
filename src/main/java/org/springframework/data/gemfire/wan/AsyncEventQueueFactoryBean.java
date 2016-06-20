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

import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory;
import com.gemstone.gemfire.cache.wan.GatewaySender;

/**
 * FactoryBean for creating GemFire {@link AsyncEventQueue}s.
 *
 * @author David Turanski
 * @author John Blum
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

	private Integer batchSize;
	private Integer batchTimeInterval;
	private Integer dispatcherThreads;
	private Integer maximumQueueMemory;

	private String diskStoreReference;
	private String orderPolicy;

	/**
	 * Constructs an instance of the AsyncEventQueueFactoryBean for creating an GemFire AsyncEventQueue.
	 *
	 * @param cache the GemFire Cache reference.
	 * @see #AsyncEventQueueFactoryBean(com.gemstone.gemfire.cache.Cache, com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener)
	 */
	public AsyncEventQueueFactoryBean(Cache cache) {
		this(cache, null);
	}

	/**
	 * Constructs an instance of the AsyncEventQueueFactoryBean for creating an GemFire AsyncEventQueue.
	 *
	 * @param cache the GemFire Cache reference.
	 * @param asyncEventListener required {@link AsyncEventListener}
	 */
	public AsyncEventQueueFactoryBean(Cache cache, AsyncEventListener asyncEventListener) {
		super(cache);
		setAsyncEventListener(asyncEventListener);
	}

	@Override
	public AsyncEventQueue getObject() throws Exception {
		return asyncEventQueue;
	}

	@Override
	public Class<?> getObjectType() {
		return (asyncEventQueue != null ? asyncEventQueue.getClass() : AsyncEventQueue.class);
	}

	@Override
	protected void doInit() {

		Assert.notNull(this.asyncEventListener, "AsyncEventListener must not be null");

		AsyncEventQueueFactory asyncEventQueueFactory = (this.factory != null ? (AsyncEventQueueFactory) factory
			: cache.createAsyncEventQueueFactory());

		if (batchSize != null) {
			asyncEventQueueFactory.setBatchSize(batchSize);
		}

		if (batchTimeInterval != null) {
			asyncEventQueueFactory.setBatchTimeInterval(batchTimeInterval);
		}

		if (batchConflationEnabled != null) {
			asyncEventQueueFactory.setBatchConflationEnabled(batchConflationEnabled);
		}

		if (dispatcherThreads != null) {
			asyncEventQueueFactory.setDispatcherThreads(dispatcherThreads);
		}

		if (diskStoreReference != null) {
			asyncEventQueueFactory.setDiskStoreName(diskStoreReference);
		}

		if (diskSynchronous != null) {
			asyncEventQueueFactory.setDiskSynchronous(diskSynchronous);
		}

		if (forwardExpirationDestroy != null) {
			asyncEventQueueFactory.setForwardExpirationDestroy(forwardExpirationDestroy);
		}

		if (maximumQueueMemory != null) {
			asyncEventQueueFactory.setMaximumQueueMemory(maximumQueueMemory);
		}

		asyncEventQueueFactory.setParallel(isParallelEventQueue());

		if (orderPolicy != null) {
			Assert.isTrue(isSerialEventQueue(), "Order Policy cannot be used with a Parallel Event Queue");

			Assert.isTrue(VALID_ORDER_POLICIES.contains(orderPolicy.toUpperCase()), String.format(
				"The value of Order Policy '$1%s' is invalid", orderPolicy));

			asyncEventQueueFactory.setOrderPolicy(GatewaySender.OrderPolicy.valueOf(orderPolicy.toUpperCase()));
		}

		if (persistent != null) {
			asyncEventQueueFactory.setPersistent(persistent);
		}

		asyncEventQueue = asyncEventQueueFactory.create(getName(), this.asyncEventListener);
	}

	@Override
	public void destroy() throws Exception {
		if (!cache.isClosed()) {
			try {
				this.asyncEventListener.close();
			}
			catch (CacheClosedException ignore) {
			}
		}
	}

	public final void setAsyncEventListener(AsyncEventListener listener) {
		Assert.state(this.asyncEventQueue == null,
			"Setting an AsyncEventListener is not allowed once the AsyncEventQueue has been created");

		this.asyncEventListener = listener;
	}

	/**
	 * @param asyncEventQueue overrides Async Event Queue returned by this FactoryBean.
	 */
	public void setAsyncEventQueue(AsyncEventQueue asyncEventQueue) {
		this.asyncEventQueue = asyncEventQueue;
	}

	/**
	 * Enable or disable the Async Event Queue's (AEQ) should conflate messages.
	 *
	 * @param batchConflationEnabled a boolean value indicating whether to conflate queued events.
	 */
	public void setBatchConflationEnabled(Boolean batchConflationEnabled) {
		this.batchConflationEnabled = batchConflationEnabled;
	}

	public void setBatchSize(Integer batchSize) {
		this.batchSize = batchSize;
	}

	/**
	 * Set the Aysync Event Queue's (AEQ) interval between sending batches.
	 *
	 * @param batchTimeInterval an integer value indicating the maximum number of milliseconds that can elapse
	 * between sending batches.
	 */
	public void setBatchTimeInterval(Integer batchTimeInterval) {
		this.batchTimeInterval = batchTimeInterval;
	}

	public void setDiskStoreRef(String diskStoreRef) {
		this.diskStoreReference = diskStoreRef;
	}

	/**
	 * Set the Async Event Queue (AEQ) disk write synchronization policy.
	 *
	 * @param diskSynchronous a boolean value indicating whether disk writes are synchronous.
	 */
	public void setDiskSynchronous(Boolean diskSynchronous) {
		this.diskSynchronous = diskSynchronous;
	}

	/**
	 * Set the number of dispatcher threads used to process Region Events from the associated Async Event Queue (AEQ).
	 *
	 * @param dispatcherThreads an Integer indicating the number of dispatcher threads used to process Region Events
	 * from the associated Queue.
	 */
	public void setDispatcherThreads(Integer dispatcherThreads) {
		this.dispatcherThreads = dispatcherThreads;
	}

	/**
	 * Forwards expiration (action-based) destroy events to the {@link AsyncEventQueue} (AEQ).
	 *
	 * By default, destroy events are not added to the AEQ.  Setting this attribute to
	 * {@literal true} will add all expiration destroy events to the AEQ.
	 *
	 * @param forwardExpirationDestroy boolean value indicating whether to forward expiration destroy events.
	 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory#setForwardExpirationDestroy(boolean)
	 * @see com.gemstone.gemfire.cache.ExpirationAttributes#getAction()
	 * @see com.gemstone.gemfire.cache.ExpirationAction#DESTROY
	 */
	public void setForwardExpirationDestroy(Boolean forwardExpirationDestroy) {
		this.forwardExpirationDestroy = forwardExpirationDestroy;
	}

	public void setMaximumQueueMemory(Integer maximumQueueMemory) {
		this.maximumQueueMemory = maximumQueueMemory;
	}

	/**
	 * Set the Async Event Queue (AEQ) ordering policy (e.g. KEY, PARTITION, THREAD). When dispatcher threads
	 * are greater than 1, the ordering policy configures the way in which multiple dispatcher threads
	 * process Region events from the queue.
	 *
	 * @param orderPolicy a String to indicate the AEQ order policy.
	 */
	public void setOrderPolicy(String orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

	public void setParallel(Boolean parallel) {
		this.parallel = parallel;
	}

	public boolean isParallelEventQueue() {
		return Boolean.TRUE.equals(parallel);
	}

	public boolean isSerialEventQueue() {
		return !isParallelEventQueue();
	}

	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
	}
}
