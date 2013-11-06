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

import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory;
import com.gemstone.gemfire.cache.util.Gateway;

/**
 * FactoryBean for creating GemFire {@link AsyncEventQueue}s.
 * <p/>
 * @author David Turanski
 * @author John Blum
 */
public class AsyncEventQueueFactoryBean extends AbstractWANComponentFactoryBean<AsyncEventQueue> {

	private static List<String> validOrderPolicyValues = Arrays.asList("KEY", "PARTITION", "THREAD");

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

	private String diskStoreRef;
	private String orderPolicy;

	/**
	 * Constructs an instance of the AsyncEventQueueFactoryBean for creating an GemFire AsyncEventQueue.
	 * <p/>
	 * @param cache the GemFire Cache reference.
	 * @see #AsyncEventQueueFactoryBean(com.gemstone.gemfire.cache.Cache, com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener)
	 */
	public AsyncEventQueueFactoryBean(Cache cache) {
		this(cache, null);
	}

	/**
	 * Constructs an instance of the AsyncEventQueueFactoryBean for creating an GemFire AsyncEventQueue.
	 * <p/>
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
		return AsyncEventQueue.class;
	}

	@Override
	protected void doInit() {
		Assert.notNull(this.asyncEventListener, "The AsyncEventListener cannot be null.");

		AsyncEventQueueFactory asyncEventQueueFactory = (this.factory != null ? (AsyncEventQueueFactory) factory
			: cache.createAsyncEventQueueFactory());

		if (diskStoreRef != null) {
			persistent = (persistent == null) ? Boolean.TRUE : persistent;
			Assert.isTrue(persistent, "Specifying a 'disk store' requires the persistent property to be true.");
			asyncEventQueueFactory.setDiskStoreName(diskStoreRef);
		}

		if (diskSynchronous != null) {
			persistent = (persistent == null) ? Boolean.TRUE : persistent;
			Assert.isTrue(persistent, "Specifying 'disk synchronous' requires the persistent property to be true.");
			asyncEventQueueFactory.setDiskSynchronous(diskSynchronous);
		}

		if (persistent != null) {
			asyncEventQueueFactory.setPersistent(persistent);
		}

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

		if (maximumQueueMemory != null) {
			asyncEventQueueFactory.setMaximumQueueMemory(maximumQueueMemory);
		}

		if (parallel != null) {
			asyncEventQueueFactory.setParallel(parallel);
		}
		
		if (orderPolicy != null) {
			Assert.isTrue(parallel, "specifying an order policy requires the parallel property to be true");

			Assert.isTrue(validOrderPolicyValues.contains(orderPolicy.toUpperCase()), String.format(
				"The value of order policy:'$1%s'' is invalid.", orderPolicy));

			asyncEventQueueFactory.setOrderPolicy(Gateway.OrderPolicy.valueOf(orderPolicy.toUpperCase()));
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
		this.diskStoreRef = diskStoreRef;
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

	/**
	 * @param validOrderPolicyValues the validOrderPolicyValues to set
	 */
	public static void setValidOrderPolicyValues(List<String> validOrderPolicyValues) {
		AsyncEventQueueFactoryBean.validOrderPolicyValues = validOrderPolicyValues;
	}

	/**
	 * @param asyncEventQueue the asyncEventQueue to set
	 */
	public void setAsyncEventQueue(AsyncEventQueue asyncEventQueue) {
		this.asyncEventQueue = asyncEventQueue;
	}

	/**
	 * @param dispatcherThreads the dispatcherThreads to set
	 */
	public void setDispatcherThreads(Integer dispatcherThreads) {
		this.dispatcherThreads = dispatcherThreads;
	}

	/**
	 * @param batchTimeInterval
	 */
	public void setBatchTimeInterval(Integer batchTimeInterval) {
		this.batchTimeInterval = batchTimeInterval;
	}

	/**
	 * 
	 * @param batchConflationEnabled
	 */
	public void setBatchConflationEnabled(Boolean batchConflationEnabled) {
		this.batchConflationEnabled = batchConflationEnabled;
	}

	/**
	 * @param diskSynchronous
	 */
	public void setDiskSynchronous(Boolean diskSynchronous) {
		this.diskSynchronous = diskSynchronous;
	}

	/**
	 * @param orderPolicy
	 */
	public void setOrderPolicy(String orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

}
