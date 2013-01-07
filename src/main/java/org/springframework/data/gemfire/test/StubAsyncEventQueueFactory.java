/*
 * Copyright 2002-2012 the original author or authors.
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
package org.springframework.data.gemfire.test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory;

/**
 * @author David Turanski
 *
 */
public class StubAsyncEventQueueFactory implements AsyncEventQueueFactory {

	private AsyncEventListener listener;

	private AsyncEventQueue asyncEventQueue = mock(AsyncEventQueue.class);

	private boolean persistent;
	private int maxQueueMemory;
	private String diskStoreName;
	private int batchSize;

	private String name;

	private boolean parallel;

	@Override
	public AsyncEventQueue create(String name, AsyncEventListener listener) {
		this.name = name;
		this.listener = listener;

		when(asyncEventQueue.getAsyncEventListener()).thenReturn(this.listener);
		when(asyncEventQueue.getBatchSize()).thenReturn(this.batchSize);
		when(asyncEventQueue.getDiskStoreName()).thenReturn(this.diskStoreName);
		when(asyncEventQueue.isPersistent()).thenReturn(this.persistent);
		when(asyncEventQueue.getId()).thenReturn(this.name);
		when(asyncEventQueue.getMaximumQueueMemory()).thenReturn(this.maxQueueMemory);
		when(asyncEventQueue.isParallel()).thenReturn(this.parallel);
		return this.asyncEventQueue;
	}

	@Override
	public AsyncEventQueueFactory setBatchSize(int batchSize) {
		this.batchSize = batchSize;
		return this;
	}

	@Override
	public AsyncEventQueueFactory setDiskStoreName(String diskStoreName) {
		this.diskStoreName = diskStoreName;
		return this;
	}

	@Override
	public AsyncEventQueueFactory setMaximumQueueMemory(int maxQueueMemory) {
		this.maxQueueMemory = maxQueueMemory;
		return this;
	}

	@Override
	public AsyncEventQueueFactory setPersistent(boolean persistent) {
		this.persistent = persistent;
		return this;
	}

	@Override
	public AsyncEventQueueFactory setParallel(boolean parallel) {
		this.parallel = parallel;
		return this;
	}
}
