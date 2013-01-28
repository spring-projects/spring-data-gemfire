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

import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;

/**
 * FactoryBean for creating GemFire {@link AsyncEventQueue}s.
 * @author David Turanski
 * 
 */
public class AsyncEventQueueFactoryBean extends AbstractWANComponentFactoryBean<AsyncEventQueue> {

	public void setDiskStoreRef(String diskStoreRef) {
		this.diskStoreRef = diskStoreRef;
	}

	private final AsyncEventListener asyncEventListener;

	private AsyncEventQueue asyncEventQueue;

	private Integer batchSize;

	private Integer maximumQueueMemory;

	private Boolean persistent;

	private String diskStoreRef;
	
	private Boolean parallel;

	/**
	 * 
	 * @param cache the gemfire cache
	 * @param asyncEventListener required {@link AsyncEventListener}
	 */
	public AsyncEventQueueFactoryBean(Cache cache, AsyncEventListener asyncEventListener) {
		super(cache);
		this.asyncEventListener = asyncEventListener;
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
		Assert.notNull(asyncEventListener, "AsyncEventListener cannot be null");
		AsyncEventQueueFactory asyncEventQueueFactory = null;
		if (this.factory == null) {
			asyncEventQueueFactory = cache.createAsyncEventQueueFactory();
		} else {
			asyncEventQueueFactory = (AsyncEventQueueFactory)factory;
		}
		
		if (persistent != null) {
			asyncEventQueueFactory.setPersistent(persistent);
		}
		if (batchSize != null) {
			asyncEventQueueFactory.setBatchSize(batchSize);
		}
		if (diskStoreRef != null) {
			persistent = (persistent == null) ? Boolean.TRUE : persistent;
			Assert.isTrue(persistent, "specifying a disk store requires persistent property to be true");
			asyncEventQueueFactory.setDiskStoreName(diskStoreRef);
		}
		if (maximumQueueMemory != null) {
			asyncEventQueueFactory.setMaximumQueueMemory(maximumQueueMemory);
		}
		if( parallel != null ){
			asyncEventQueueFactory.setParallel( parallel );
		}

		asyncEventQueue = asyncEventQueueFactory.create(getName(), asyncEventListener);
	}

	@Override
	public void destroy() throws Exception {
		if (!cache.isClosed()) {
			try {
				asyncEventListener.close();
			}
			catch (CacheClosedException cce) {
				// nothing to see folks, move on.
			}
		}
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
	
	public void setParallel(Boolean parallel){
		this.parallel = parallel;
	}
}
