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
import com.gemstone.gemfire.cache.util.Gateway;

/**
 * FactoryBean for creating GemFire {@link AsyncEventQueue}s.
 * 
 * @author David Turanski
 * @author John Blum
 */
public class AsyncEventQueueFactoryBean extends AbstractWANComponentFactoryBean<AsyncEventQueue> {

	private AsyncEventListener asyncEventListener;

	private AsyncEventQueue asyncEventQueue;

	private Boolean batchConflationEnabled;
	private Boolean diskSynchronous;
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
	public AsyncEventQueueFactoryBean(final Cache cache) {
		this(cache, null);
	}

	/**
	 * Constructs an instance of the AsyncEventQueueFactoryBean for creating an GemFire AsyncEventQueue.
	 * 
	 * @param cache the GemFire Cache reference.
	 * @param asyncEventListener required {@link AsyncEventListener}
	 */
	public AsyncEventQueueFactoryBean(final Cache cache, final AsyncEventListener asyncEventListener) {
		super(cache);
		setAsyncEventListener(asyncEventListener);
	}

	@Override
	public AsyncEventQueue getObject() throws Exception {
		return asyncEventQueue;
	}

	@Override
	public Class<?> getObjectType() {
		return AsyncEventQueue.class;
	}

	@Override
	protected void doInit() {
		Assert.notNull(this.asyncEventListener, "The AsyncEventListener cannot be null.");

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
			Assert.isTrue(isSerialEventQueue(), "The number of Dispatcher Threads cannot be specified with a Parallel Event Queue.");
			asyncEventQueueFactory.setDispatcherThreads(dispatcherThreads);
		}

		if (diskStoreReference != null) {
			asyncEventQueueFactory.setDiskStoreName(diskStoreReference);
		}

		if (diskSynchronous != null) {
			asyncEventQueueFactory.setDiskSynchronous(diskSynchronous);
		}

		if (maximumQueueMemory != null) {
			asyncEventQueueFactory.setMaximumQueueMemory(maximumQueueMemory);
		}

		asyncEventQueueFactory.setParallel(isParallelEventQueue());

		if (orderPolicy != null) {
			Assert.isTrue(isSerialEventQueue(), "Order Policy cannot be used with a Parallel Event Queue.");

			Assert.isTrue(VALID_ORDER_POLICIES.contains(orderPolicy.toUpperCase()), String.format(
				"The value of Order Policy '$1%s' is invalid.", orderPolicy));

			asyncEventQueueFactory.setOrderPolicy(Gateway.OrderPolicy.valueOf(orderPolicy.toUpperCase()));
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
			"Setting an AsyncEventListener is not allowed once the AsyncEventQueue has been created.");
		this.asyncEventListener = listener;
	}

	public void setDiskStoreRef(String diskStoreRef) {
		this.diskStoreReference = diskStoreRef;
	}

	public void setBatchSize(Integer batchSize) {
		this.batchSize = batchSize;
	}

	public void setMaximumQueueMemory(Integer maximumQueueMemory) {
		this.maximumQueueMemory = maximumQueueMemory;
	}

	public void setPersistent(Boolean persistent) {
		this.persistent = persistent;
	}

	public void setParallel(Boolean parallel) {
		this.parallel = parallel;
	}

	public boolean isSerialEventQueue() {
		return !isParallelEventQueue();
	}

	public boolean isParallelEventQueue() {
		return Boolean.TRUE.equals(parallel);
	}

	/**
	 * @param asyncEventQueue overrides Async Event Queue returned by this FactoryBean.
	 */
	public void setAsyncEventQueue(AsyncEventQueue asyncEventQueue) {
		this.asyncEventQueue = asyncEventQueue;
	}

	/**
	 * Set the number of dispatcher threads used to process Region events from the associated Aysnc Event Queue (AEQ).
	 *
	 * @param dispatcherThreads an integer indicating the number of dispatcher threads used to process Region events
	 * from the associated queue.
	 */
	public void setDispatcherThreads(Integer dispatcherThreads) {
		this.dispatcherThreads = dispatcherThreads;
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

	/**
	 * Enable or disable the Async Event Queue's (AEQ) should conflate messages.
	 *
	 * @param batchConflationEnabled a boolean value indicating whether to conflate queued events.
	 */
	public void setBatchConflationEnabled(Boolean batchConflationEnabled) {
		this.batchConflationEnabled = batchConflationEnabled;
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
	 * Set the Async Event Queue (AEQ) ordering policy (e.g. KEY, PARTITION, THREAD). When dispatcher threads
	 * are greater than 1, the ordering policy configures the way in which multiple dispatcher threads
	 * process Region events from the queue.
	 *
	 * @param orderPolicy a String to indicate the AEQ order policy.
	 */
	public void setOrderPolicy(String orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

}
