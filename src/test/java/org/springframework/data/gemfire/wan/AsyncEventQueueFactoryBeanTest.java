/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.wan;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.asyncqueue.AsyncEventListener;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewaySender;
import org.junit.Test;
import org.springframework.data.gemfire.TestUtils;

/**
 * The AsyncEventQueueFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the AsyncEventQueueFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.TestUtils
 * @see org.springframework.data.gemfire.wan.AsyncEventQueueFactoryBean
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.asyncqueue.AsyncEventListener
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory
 * @since 1.3.3
 */
public class AsyncEventQueueFactoryBeanTest {

	private Cache mockCache() {
		return mock(Cache.class);
	}

	private Cache mockCache(AsyncEventQueueFactory mockAsyncEventQueueFactory) {

		Cache mockCache = mockCache();

		when((mockCache.createAsyncEventQueueFactory())).thenReturn(mockAsyncEventQueueFactory);

		return mockCache;
	}

	private AsyncEventQueueFactory mockAsyncEventQueueFactory(String asyncEventQueueId) {

		AsyncEventQueueFactory mockAsyncEventQueueFactory = mock(AsyncEventQueueFactory.class);

		AsyncEventQueue mockAsyncEventQueue = mockAsyncEventQueue(asyncEventQueueId);

		when(mockAsyncEventQueueFactory.create(eq(asyncEventQueueId), isA(AsyncEventListener.class)))
			.thenReturn(mockAsyncEventQueue);

		return mockAsyncEventQueueFactory;
	}

	private AsyncEventQueue mockAsyncEventQueue(String asyncEventQueueId) {

		AsyncEventQueue mockAsyncEventQueue = mock(AsyncEventQueue.class);

		when(mockAsyncEventQueue.getId()).thenReturn(asyncEventQueueId);

		return mockAsyncEventQueue;
	}

	private AsyncEventListener mockAsyncEventListener() {
		return mock(AsyncEventListener.class);
	}

	@Test
	public void setAndGetAsyncEventListener() throws Exception {

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(mockCache());

		AsyncEventListener listenerOne = mockAsyncEventListener();

		factoryBean.setAsyncEventListener(listenerOne);

		assertThat(TestUtils.<AsyncEventListener>readField("asyncEventListener", factoryBean))
			.isSameAs(listenerOne);

		AsyncEventListener listenerTwo = mockAsyncEventListener();

		factoryBean.setAsyncEventListener(listenerTwo);

		assertThat(TestUtils.<AsyncEventListener>readField("asyncEventListener", factoryBean))
			.isSameAs(listenerTwo);
	}

	@Test(expected = IllegalStateException.class)
	public void setAsyncEventListenerAfterAsyncEventQueueCreationThrowsIllegalStateException() throws Exception {

		AsyncEventListener mockAsyncEventListener = mockAsyncEventListener();

		AsyncEventQueue mockAsyncEventQueue = mockAsyncEventQueue("testEventQueue");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(mockCache(), mockAsyncEventListener);

		factoryBean.setAsyncEventQueue(mockAsyncEventQueue);

		try {
			factoryBean.setAsyncEventListener(mockAsyncEventListener());
		}
		catch (IllegalStateException expected) {

			assertThat(expected)
				.hasMessage("Setting an AsyncEventListener is not allowed once the AsyncEventQueue has been created");

			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			assertThat(TestUtils.<AsyncEventListener>readField("asyncEventListener", factoryBean))
				.isSameAs(mockAsyncEventListener);
		}
	}

	@Test
	public void doInitConfiguresAsyncEventQueue() throws Exception {

		AsyncEventListener mockListener = mockAsyncEventListener();

		AsyncEventQueueFactory mockAsyncEventQueueFactory = mockAsyncEventQueueFactory("testQueue");

		AsyncEventQueueFactoryBean factoryBean =
			new AsyncEventQueueFactoryBean(mockCache(mockAsyncEventQueueFactory), mockListener);

		GatewayEventFilter mockGatewayEventFilterOne = mock(GatewayEventFilter.class);
		GatewayEventFilter mockGatewayEventFilterTwo = mock(GatewayEventFilter.class);

		GatewayEventSubstitutionFilter mockGatewayEventSubstitutionFilter = mock(GatewayEventSubstitutionFilter.class);

		factoryBean.setBatchConflationEnabled(true);
		factoryBean.setBatchSize(1024);
		factoryBean.setBatchTimeInterval(600);
		factoryBean.setDiskStoreRef("testDiskStore");
		factoryBean.setDiskSynchronous(false);
		factoryBean.setDispatcherThreads(2);
		factoryBean.setForwardExpirationDestroy(true);
		factoryBean.setGatewayEventFilters(Arrays.asList(mockGatewayEventFilterOne, mockGatewayEventFilterTwo));
		factoryBean.setGatewayEventSubstitutionFilter(mockGatewayEventSubstitutionFilter);
		factoryBean.setMaximumQueueMemory(8192);
		factoryBean.setName("testQueue");
		factoryBean.setOrderPolicy(GatewaySender.OrderPolicy.PARTITION);
		factoryBean.setParallel(false);
		factoryBean.setPersistent(false);
		factoryBean.doInit();

		verify(mockAsyncEventQueueFactory, times(1)).setBatchConflationEnabled(eq(true));
		verify(mockAsyncEventQueueFactory, times(1)).setBatchSize(eq(1024));
		verify(mockAsyncEventQueueFactory, times(1)).setBatchTimeInterval(eq(600));
		verify(mockAsyncEventQueueFactory, times(1)).setDiskStoreName(eq("testDiskStore"));
		verify(mockAsyncEventQueueFactory, times(1)).setDiskSynchronous(eq(false));
		verify(mockAsyncEventQueueFactory, times(1)).setDispatcherThreads(eq(2));
		verify(mockAsyncEventQueueFactory, times(1)).setForwardExpirationDestroy(eq(true));
		verify(mockAsyncEventQueueFactory, times(1))
			.setGatewayEventSubstitutionListener(eq(mockGatewayEventSubstitutionFilter));
		verify(mockAsyncEventQueueFactory, times(1)).setMaximumQueueMemory(eq(8192));
		verify(mockAsyncEventQueueFactory, times(1)).setOrderPolicy(eq(GatewaySender.OrderPolicy.PARTITION));
		verify(mockAsyncEventQueueFactory, times(1)).setParallel(eq(false));
		verify(mockAsyncEventQueueFactory, times(1)).setPersistent(eq(false));
		verify(mockAsyncEventQueueFactory, times(1)).addGatewayEventFilter(eq(mockGatewayEventFilterOne));
		verify(mockAsyncEventQueueFactory, times(1)).addGatewayEventFilter(eq(mockGatewayEventFilterTwo));

		AsyncEventQueue asyncEventQueue = factoryBean.getObject();

		assertThat(asyncEventQueue).isNotNull();
		assertThat(asyncEventQueue.getId()).isEqualTo("testQueue");
	}

	@Test
	public void doInitConfiguresConcurrentParallelAsyncEventQueue() throws Exception {

		AsyncEventQueueFactory mockAsyncEventQueueFactory =
			mockAsyncEventQueueFactory("concurrentParallelQueue");

		AsyncEventQueueFactoryBean factoryBean =
			new AsyncEventQueueFactoryBean(mockCache(mockAsyncEventQueueFactory));

		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setDispatcherThreads(8);
		factoryBean.setName("concurrentParallelQueue");
		factoryBean.setParallel(true);
		factoryBean.doInit();

		verify(mockAsyncEventQueueFactory, never()).setBatchConflationEnabled(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setBatchSize(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setBatchTimeInterval(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setDiskStoreName(anyString());
		verify(mockAsyncEventQueueFactory, never()).setDiskSynchronous(anyBoolean());
		verify(mockAsyncEventQueueFactory, times(1)).setDispatcherThreads(eq(8));
		verify(mockAsyncEventQueueFactory, never()).setForwardExpirationDestroy(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setGatewayEventSubstitutionListener(any());
		verify(mockAsyncEventQueueFactory, never()).setMaximumQueueMemory(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setOrderPolicy(any());
		verify(mockAsyncEventQueueFactory, times(1)).setParallel(eq(true));
		verify(mockAsyncEventQueueFactory, never()).setPersistent(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).addGatewayEventFilter(any());

		AsyncEventQueue asyncEventQueue = factoryBean.getObject();

		assertThat(asyncEventQueue).isNotNull();
		assertThat(asyncEventQueue.getId()).isEqualTo("concurrentParallelQueue");
	}

	@Test
	public void doInitConfiguresParallelAsyncEventQueue() throws Exception {

		AsyncEventQueueFactory mockAsyncEventQueueFactory =
			mockAsyncEventQueueFactory("parallelQueue");

		AsyncEventQueueFactoryBean factoryBean =
			new AsyncEventQueueFactoryBean(mockCache(mockAsyncEventQueueFactory));

		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setName("parallelQueue");
		factoryBean.setParallel(true);
		factoryBean.doInit();

		verify(mockAsyncEventQueueFactory, never()).setBatchConflationEnabled(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setBatchSize(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setBatchTimeInterval(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setDiskStoreName(anyString());
		verify(mockAsyncEventQueueFactory, never()).setDiskSynchronous(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setDispatcherThreads(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setForwardExpirationDestroy(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setGatewayEventSubstitutionListener(any());
		verify(mockAsyncEventQueueFactory, never()).setMaximumQueueMemory(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setOrderPolicy(any());
		verify(mockAsyncEventQueueFactory, times(1)).setParallel(eq(true));
		verify(mockAsyncEventQueueFactory, never()).setPersistent(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).addGatewayEventFilter(any());

		AsyncEventQueue asyncEventQueue = factoryBean.getObject();

		assertThat(asyncEventQueue).isNotNull();
		assertThat(asyncEventQueue.getId()).isEqualTo("parallelQueue");
	}

	@Test
	public void doInitConfiguresConcurrentSerialAsyncEventQueue() throws Exception {

		AsyncEventQueueFactory mockAsyncEventQueueFactory =
			mockAsyncEventQueueFactory("concurrentSerialQueue");

		AsyncEventQueueFactoryBean factoryBean =
			new AsyncEventQueueFactoryBean(mockCache(mockAsyncEventQueueFactory));

		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setDispatcherThreads(16);
		factoryBean.setName("concurrentSerialQueue");
		factoryBean.setParallel(false);
		factoryBean.doInit();

		verify(mockAsyncEventQueueFactory, never()).setBatchConflationEnabled(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setBatchSize(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setBatchTimeInterval(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setDiskStoreName(anyString());
		verify(mockAsyncEventQueueFactory, never()).setDiskSynchronous(anyBoolean());
		verify(mockAsyncEventQueueFactory, times(1)).setDispatcherThreads(eq(16));
		verify(mockAsyncEventQueueFactory, never()).setForwardExpirationDestroy(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setGatewayEventSubstitutionListener(any());
		verify(mockAsyncEventQueueFactory, never()).setMaximumQueueMemory(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setOrderPolicy(any());
		verify(mockAsyncEventQueueFactory, times(1)).setParallel(eq(false));
		verify(mockAsyncEventQueueFactory, never()).setPersistent(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).addGatewayEventFilter(any());

		AsyncEventQueue asyncEventQueue = factoryBean.getObject();

		assertThat(asyncEventQueue).isNotNull();
		assertThat(asyncEventQueue.getId()).isEqualTo("concurrentSerialQueue");
	}

	@Test
	public void doInitConfiguresSerialAsyncEventQueue() throws Exception {

		AsyncEventQueueFactory mockAsyncEventQueueFactory = mockAsyncEventQueueFactory("serialQueue");

		AsyncEventQueueFactoryBean factoryBean =
			new AsyncEventQueueFactoryBean(mockCache(mockAsyncEventQueueFactory));

		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setName("serialQueue");
		factoryBean.doInit();

		verify(mockAsyncEventQueueFactory, never()).setBatchConflationEnabled(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setBatchSize(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setBatchTimeInterval(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setDiskStoreName(anyString());
		verify(mockAsyncEventQueueFactory, never()).setDiskSynchronous(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setDispatcherThreads(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setForwardExpirationDestroy(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setGatewayEventSubstitutionListener(any());
		verify(mockAsyncEventQueueFactory, never()).setMaximumQueueMemory(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setOrderPolicy(any());
		verify(mockAsyncEventQueueFactory, times(1)).setParallel(eq(false));
		verify(mockAsyncEventQueueFactory, never()).setPersistent(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).addGatewayEventFilter(any());

		AsyncEventQueue asyncEventQueue = factoryBean.getObject();

		assertThat(asyncEventQueue).isNotNull();
		assertThat(asyncEventQueue.getId()).isEqualTo("serialQueue");
	}

	@Test
	public void doInitConfiguresSerialAsyncEventQueueWithOrderPolicy() throws Exception {

		AsyncEventQueueFactory mockAsyncEventQueueFactory =
			mockAsyncEventQueueFactory("orderedSerialQueue");

		AsyncEventQueueFactoryBean factoryBean =
			new AsyncEventQueueFactoryBean(mockCache(mockAsyncEventQueueFactory));

		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setName("orderedSerialQueue");
		factoryBean.setOrderPolicy(GatewaySender.OrderPolicy.THREAD);
		factoryBean.doInit();

		verify(mockAsyncEventQueueFactory, never()).setBatchConflationEnabled(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setBatchSize(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setBatchTimeInterval(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setDiskStoreName(anyString());
		verify(mockAsyncEventQueueFactory, never()).setDiskSynchronous(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setDispatcherThreads(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setForwardExpirationDestroy(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setGatewayEventSubstitutionListener(any());
		verify(mockAsyncEventQueueFactory, never()).setMaximumQueueMemory(anyInt());
		verify(mockAsyncEventQueueFactory, times(1)).setOrderPolicy(eq(GatewaySender.OrderPolicy.THREAD));
		verify(mockAsyncEventQueueFactory, times(1)).setParallel(eq(false));
		verify(mockAsyncEventQueueFactory, never()).setPersistent(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).addGatewayEventFilter(any());

		AsyncEventQueue asyncEventQueue = factoryBean.getObject();

		assertThat(asyncEventQueue).isNotNull();
		assertThat(asyncEventQueue.getId()).isEqualTo("orderedSerialQueue");
	}

	@Test(expected = IllegalStateException.class)
	public void doInitWithNullAsyncEventListenerThrowsIllegalStateException() throws Exception {

		try {
			new AsyncEventQueueFactoryBean(mockCache(), null).doInit();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage("AsyncEventListener must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalStateException.class)
	public void doInitWithParallelAsyncEventQueueHavingAnOrderPolicyThrowsIllegalStateException() throws Exception {

		AsyncEventQueueFactory mockAsyncEventQueueFactory =
			mockAsyncEventQueueFactory("parallelQueue");

		AsyncEventQueueFactoryBean factoryBean =
			new AsyncEventQueueFactoryBean(mockCache(mockAsyncEventQueueFactory));

		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setName("parallelQueue");
		factoryBean.setOrderPolicy(GatewaySender.OrderPolicy.KEY);
		factoryBean.setParallel(true);

		try {
			factoryBean.doInit();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage("OrderPolicy cannot be used with a Parallel AsyncEventQueue");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {

			assertThat(factoryBean.getObject()).isNull();

			verify(mockAsyncEventQueueFactory, never()).setBatchConflationEnabled(anyBoolean());
			verify(mockAsyncEventQueueFactory, never()).setBatchSize(anyInt());
			verify(mockAsyncEventQueueFactory, never()).setBatchTimeInterval(anyInt());
			verify(mockAsyncEventQueueFactory, never()).setDiskStoreName(anyString());
			verify(mockAsyncEventQueueFactory, never()).setDiskSynchronous(anyBoolean());
			verify(mockAsyncEventQueueFactory, never()).setDispatcherThreads(eq(8));
			verify(mockAsyncEventQueueFactory, never()).setForwardExpirationDestroy(anyBoolean());
			verify(mockAsyncEventQueueFactory, never()).setGatewayEventSubstitutionListener(any());
			verify(mockAsyncEventQueueFactory, never()).setMaximumQueueMemory(anyInt());
			verify(mockAsyncEventQueueFactory, never()).setOrderPolicy(any());
			verify(mockAsyncEventQueueFactory, times(1)).setParallel(eq(true));
			verify(mockAsyncEventQueueFactory, never()).setPersistent(anyBoolean());
			verify(mockAsyncEventQueueFactory, never()).addGatewayEventFilter(any());
		}
	}

	@Test
	public void doInitConfiguresAsyncEventQueueWithSynchronousOverflowDiskStoreNoPersistence() throws Exception {

		AsyncEventQueueFactory mockAsyncEventQueueFactory =
			mockAsyncEventQueueFactory("nonPersistentSynchronousOverflowQueue");

		AsyncEventQueueFactoryBean factoryBean =
			new AsyncEventQueueFactoryBean(mockCache(mockAsyncEventQueueFactory));

		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setDiskStoreRef("queueOverflowDiskStore");
		factoryBean.setDiskSynchronous(true);
		factoryBean.setName("nonPersistentSynchronousOverflowQueue");
		factoryBean.setOrderPolicy(GatewaySender.OrderPolicy.KEY);
		factoryBean.setPersistent(false);
		factoryBean.doInit();

		verify(mockAsyncEventQueueFactory, never()).setBatchConflationEnabled(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setBatchSize(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setBatchTimeInterval(anyInt());
		verify(mockAsyncEventQueueFactory, times(1)).setDiskStoreName("queueOverflowDiskStore");
		verify(mockAsyncEventQueueFactory, times(1)).setDiskSynchronous(eq(true));
		verify(mockAsyncEventQueueFactory, never()).setDispatcherThreads(anyInt());
		verify(mockAsyncEventQueueFactory, never()).setForwardExpirationDestroy(anyBoolean());
		verify(mockAsyncEventQueueFactory, never()).setGatewayEventSubstitutionListener(any());
		verify(mockAsyncEventQueueFactory, never()).setMaximumQueueMemory(anyInt());
		verify(mockAsyncEventQueueFactory, times(1)).setOrderPolicy(eq(GatewaySender.OrderPolicy.KEY));
		verify(mockAsyncEventQueueFactory, times(1)).setParallel(eq(false));
		verify(mockAsyncEventQueueFactory, times(1)).setPersistent(eq(false));
		verify(mockAsyncEventQueueFactory, never()).addGatewayEventFilter(any());

		AsyncEventQueue asyncEventQueue = factoryBean.getObject();

		assertThat(asyncEventQueue).isNotNull();
		assertThat(asyncEventQueue.getId()).isEqualTo("nonPersistentSynchronousOverflowQueue");
	}
}
