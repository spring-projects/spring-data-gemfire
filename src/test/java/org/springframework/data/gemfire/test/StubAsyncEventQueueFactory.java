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
package org.springframework.data.gemfire.test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory;
import com.gemstone.gemfire.cache.wan.GatewayEventFilter;
import com.gemstone.gemfire.cache.wan.GatewayEventSubstitutionFilter;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewaySender.OrderPolicy;

/**
 * @author David Turanski
 * @author John Blum
 */
@SuppressWarnings("deprecated")
public class StubAsyncEventQueueFactory implements AsyncEventQueueFactory {

	private AsyncEventQueue asyncEventQueue = mock(AsyncEventQueue.class);

	private boolean batchConflationEnabled;
	private boolean diskSynchronous;
	private boolean ignoreEvictionAndExpiration;
	private boolean parallel;
	private boolean persistent;

	private int batchSize;
	private int batchTimeInterval;
	private int dispatcherThreads = GatewaySender.DEFAULT_DISPATCHER_THREADS;
	private int maxQueueMemory;

	private GatewayEventSubstitutionFilter<?, ?> gatewayEventSubstitutionFilter;

	private OrderPolicy orderPolicy;

	private List<GatewayEventFilter> gatewayEventFilters = new ArrayList<GatewayEventFilter>();

	private String diskStoreName;

	@Override
	public AsyncEventQueue create(String name, AsyncEventListener listener) {
		when(asyncEventQueue.getAsyncEventListener()).thenReturn(listener);
		when(asyncEventQueue.isBatchConflationEnabled()).thenReturn(this.batchConflationEnabled);
		when(asyncEventQueue.getBatchSize()).thenReturn(this.batchSize);
		when(asyncEventQueue.getBatchTimeInterval()).thenReturn(this.batchTimeInterval);
		when(asyncEventQueue.getDiskStoreName()).thenReturn(this.diskStoreName);
		when(asyncEventQueue.isDiskSynchronous()).thenReturn(this.diskSynchronous);
		when(asyncEventQueue.getDispatcherThreads()).thenReturn(this.dispatcherThreads);
		when(asyncEventQueue.getGatewayEventFilters()).thenReturn(Collections.unmodifiableList(gatewayEventFilters));
		when(asyncEventQueue.getGatewayEventSubstitutionFilter()).thenReturn(this.gatewayEventSubstitutionFilter);
		when(asyncEventQueue.getId()).thenReturn(name);
		when(asyncEventQueue.isIgnoreEvictionAndExpiration()).thenReturn(this.ignoreEvictionAndExpiration);
		when(asyncEventQueue.getMaximumQueueMemory()).thenReturn(this.maxQueueMemory);
		when(asyncEventQueue.getOrderPolicy()).thenReturn(this.orderPolicy);
		when(asyncEventQueue.isParallel()).thenReturn(this.parallel);
		when(asyncEventQueue.isPersistent()).thenReturn(this.persistent);

		return this.asyncEventQueue;
	}

	public AsyncEventQueueFactory setBatchConflationEnabled(boolean batchConflationEnabled) {
		this.batchConflationEnabled = batchConflationEnabled;
		return this;
	}

	@Override
	public AsyncEventQueueFactory setBatchSize(int batchSize) {
		this.batchSize = batchSize;
		return this;
	}

	public AsyncEventQueueFactory setBatchTimeInterval(int batchTimeInterval) {
		this.batchTimeInterval = batchTimeInterval;
		return this;
	}

	@Override
	public AsyncEventQueueFactory setDiskStoreName(String diskStoreName) {
		this.diskStoreName = diskStoreName;
		return this;
	}

	public AsyncEventQueueFactory setDispatcherThreads(int dispatchThreads) {
		this.dispatcherThreads = dispatchThreads;
		return this;
	}

	public AsyncEventQueueFactory setDiskSynchronous(boolean diskSynchronous) {
		this.diskSynchronous = diskSynchronous;
		return this;
	}

	public AsyncEventQueueFactory setIgnoreEvictionAndExpiration(boolean ignoreEvictionAndExpiration) {
		this.ignoreEvictionAndExpiration = ignoreEvictionAndExpiration;
		return this;
	}

	public AsyncEventQueueFactory setMaximumQueueMemory(int maxQueueMemory) {
		this.maxQueueMemory = maxQueueMemory;
		return this;
	}

	public AsyncEventQueueFactory setOrderPolicy(OrderPolicy orderPolicy) {
		this.orderPolicy = orderPolicy;
		return this;
	}

	public AsyncEventQueueFactory setParallel(boolean parallel) {
		this.parallel = parallel;
		return this;
	}

	public AsyncEventQueueFactory setPersistent(boolean persistent) {
		this.persistent = persistent;
		return this;
	}

	public AsyncEventQueueFactory addGatewayEventFilter(final GatewayEventFilter gatewayEventFilter) {
		gatewayEventFilters.add(gatewayEventFilter);
		return this;
	}

	public AsyncEventQueueFactory removeGatewayEventFilter(final GatewayEventFilter gatewayEventFilter) {
		gatewayEventFilters.remove(gatewayEventFilter);
		return this;
	}

	public AsyncEventQueueFactory setGatewayEventSubstitutionListener(final GatewayEventSubstitutionFilter gatewayEventSubstitutionFilter) {
		this.gatewayEventSubstitutionFilter = gatewayEventSubstitutionFilter;
		return this;
	}
}
