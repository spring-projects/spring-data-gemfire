/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.wan;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.springframework.data.gemfire.TestUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory;
import com.gemstone.gemfire.cache.wan.GatewaySender;

/**
 * The AsyncEventQueueFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the AsyncEventQueueFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.TestUtils
 * @see org.springframework.data.gemfire.wan.AsyncEventQueueFactoryBean
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener
 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue
 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory
 * @since 1.3.3
 */
public class AsyncEventQueueFactoryBeanTest {

	protected Cache createMockCacheWithAsyncEventQueueInfrastructure(AsyncEventQueueFactory mockAsyncEventQueueFactory) {
		Cache mockCache = mock(Cache.class);
		when((mockCache.createAsyncEventQueueFactory())).thenReturn(mockAsyncEventQueueFactory);
		return mockCache;
	}

	protected AsyncEventQueueFactory createMockAsyncEventQueueFactory(String asyncEventQueueId) {
		AsyncEventQueueFactory mockAsyncEventQueueFactory = mock(AsyncEventQueueFactory.class);
		AsyncEventQueue mockAsyncEventQueue = mock(AsyncEventQueue.class);

		when(mockAsyncEventQueue.getId()).thenReturn(asyncEventQueueId);
		when(mockAsyncEventQueueFactory.create(eq(asyncEventQueueId), notNull(AsyncEventListener.class)))
			.thenReturn(mockAsyncEventQueue);

		return mockAsyncEventQueueFactory;
	}

	protected AsyncEventListener mockAsyncEventListener() {
		return mock(AsyncEventListener.class);
	}

	protected void verifyExpectations(AsyncEventQueueFactory asyncEventQueueFactory,
			AsyncEventQueueFactoryBean asyncEventQueueFactoryBean) throws Exception {

		Boolean parallel = TestUtils.readField("parallel", asyncEventQueueFactoryBean);

		verify(asyncEventQueueFactory).setParallel(eq(Boolean.TRUE.equals(parallel)));

		String orderPolicy = TestUtils.readField("orderPolicy", asyncEventQueueFactoryBean);

		if (orderPolicy != null) {
			verify(asyncEventQueueFactory).setOrderPolicy(eq(GatewaySender.OrderPolicy.valueOf(
				orderPolicy.toUpperCase())));
		}

		Integer dispatcherThreads = TestUtils.readField("dispatcherThreads", asyncEventQueueFactoryBean);

		if (dispatcherThreads != null) {
			verify(asyncEventQueueFactory).setDispatcherThreads(eq(dispatcherThreads));
		}

		String diskStoreReference = TestUtils.readField("diskStoreReference", asyncEventQueueFactoryBean);

		if (diskStoreReference != null) {
			verify(asyncEventQueueFactory).setDiskStoreName(eq(diskStoreReference));
		}

		Boolean diskSynchronous = TestUtils.readField("diskSynchronous", asyncEventQueueFactoryBean);

		if (diskSynchronous != null) {
			verify(asyncEventQueueFactory).setDiskSynchronous(eq(diskSynchronous));
		}

		Boolean persistent = TestUtils.readField("persistent", asyncEventQueueFactoryBean);

		if (persistent != null) {
			verify(asyncEventQueueFactory).setPersistent(eq(persistent));
		}
		else {
			verify(asyncEventQueueFactory, never()).setPersistent(true);
		}
	}

	@Test
	public void testSetAsyncEventListener() throws Exception {
		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(createMockAsyncEventQueueFactory("testEventQueue")));

		AsyncEventListener listenerOne = mockAsyncEventListener();

		factoryBean.setAsyncEventListener(listenerOne);

		assertSame(listenerOne, TestUtils.readField("asyncEventListener", factoryBean));

		AsyncEventListener listenerTwo = mockAsyncEventListener();

		factoryBean.setAsyncEventListener(listenerTwo);

		assertSame(listenerTwo, TestUtils.readField("asyncEventListener", factoryBean));
	}

	@Test(expected = IllegalStateException.class)
	public void testSetAsyncEventListenerAfterAsyncEventQueueCreation() throws Exception {
		String asyncEventQueueId = "testEventQueue";

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(createMockAsyncEventQueueFactory(asyncEventQueueId)));

		factoryBean.setName(asyncEventQueueId);

		AsyncEventListener listenerOne = mockAsyncEventListener();

		factoryBean.setAsyncEventListener(listenerOne);

		assertSame(listenerOne, TestUtils.readField("asyncEventListener", factoryBean));

		factoryBean.doInit();

		assertNotNull(TestUtils.readField("asyncEventQueue", factoryBean));

		try {
			factoryBean.setAsyncEventListener(mockAsyncEventListener());
		}
		catch (IllegalStateException expected) {
			assertEquals("Setting an AsyncEventListener is not allowed once the AsyncEventQueue has been created",
				expected.getMessage());
			assertSame(listenerOne, TestUtils.readField("asyncEventListener", factoryBean));
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testDoInitWhenAsyncEventListenerIsNull() throws Exception {
		try {
			AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
				createMockCacheWithAsyncEventQueueInfrastructure(createMockAsyncEventQueueFactory("testEventQueue")));

			assertNull(TestUtils.readField("asyncEventListener", factoryBean));

			factoryBean.doInit();
		}
		catch (Exception e) {
			assertEquals("AsyncEventListener must not be null", e.getMessage());
			throw e;
		}
	}

	@Test
	public void testConcurrentParallelAsyncEventQueue() throws Exception {
		AsyncEventQueueFactory mockAsyncEventQueueFactory = createMockAsyncEventQueueFactory("000");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFactory));

		factoryBean.setName("000");
		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setDispatcherThreads(8);
		factoryBean.setParallel(true);
		factoryBean.doInit();

		verifyExpectations(mockAsyncEventQueueFactory, factoryBean);

		AsyncEventQueue eventQueue = factoryBean.getObject();

		assertNotNull(eventQueue);
		assertEquals("000", eventQueue.getId());
	}

	@Test
	public void testParallelAsyncEventQueue() throws Exception {
		AsyncEventQueueFactory mockAsyncEventQueueFactory = createMockAsyncEventQueueFactory("123");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFactory));

		factoryBean.setName("123");
		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setParallel(true);
		factoryBean.doInit();

		verifyExpectations(mockAsyncEventQueueFactory, factoryBean);

		AsyncEventQueue eventQueue = factoryBean.getObject();

		assertNotNull(eventQueue);
		assertEquals("123", eventQueue.getId());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testParallelAsyncEventQueueWithOrderPolicy() {
		AsyncEventQueueFactory mockAsyncEventQueueFactory = createMockAsyncEventQueueFactory("456");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFactory));

		factoryBean.setName("456");
		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setOrderPolicy("Key");
		factoryBean.setParallel(true);

		try {
			factoryBean.doInit();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Order Policy cannot be used with a Parallel Event Queue",
				expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSerialAsyncEventQueueWithOrderPolicy() throws Exception {
		AsyncEventQueueFactory mockAsyncEventQueueFatory = createMockAsyncEventQueueFactory("789");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFatory));

		factoryBean.setName("789");
		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setOrderPolicy("THREAD");
		factoryBean.setParallel(false);
		factoryBean.doInit();

		verifyExpectations(mockAsyncEventQueueFatory, factoryBean);

		AsyncEventQueue eventQueue = factoryBean.getObject();

		assertNotNull(eventQueue);
		assertEquals("789", eventQueue.getId());
	}

	@Test
	public void testAsyncEventQueueWithOrderPolicyAndDispatcherThreads() throws Exception {
		AsyncEventQueueFactory mockAsyncEventQueueFatory = createMockAsyncEventQueueFactory("abc");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFatory));

		factoryBean.setName("abc");
		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setDispatcherThreads(2);
		factoryBean.setOrderPolicy("THREAD");
		factoryBean.doInit();

		verifyExpectations(mockAsyncEventQueueFatory, factoryBean);

		AsyncEventQueue eventQueue = factoryBean.getObject();

		assertNotNull(eventQueue);
		assertEquals("abc", eventQueue.getId());
	}

	@Test
	public void testAsyncEventQueueWithOverflowDiskStoreNoPersistence() throws Exception {
		AsyncEventQueueFactory mockAsyncEventQueueFactory = createMockAsyncEventQueueFactory("123abc");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFactory));

		factoryBean.setName("123abc");
		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setDiskStoreRef("queueOverflowDiskStore");
		factoryBean.setPersistent(false);
		factoryBean.doInit();

		verifyExpectations(mockAsyncEventQueueFactory, factoryBean);

		AsyncEventQueue evenQueue = factoryBean.getObject();

		assertNotNull(evenQueue);
		assertEquals("123abc", evenQueue.getId());
	}

	@Test
	public void testAsyncEventQueueWithDiskSynchronousSetPersistenceUnset() throws Exception {
		AsyncEventQueueFactory mockAsyncEventQueueFactory = createMockAsyncEventQueueFactory("12345");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFactory));

		factoryBean.setName("12345");
		factoryBean.setAsyncEventListener(mockAsyncEventListener());
		factoryBean.setDiskSynchronous(true);
		factoryBean.doInit();

		verifyExpectations(mockAsyncEventQueueFactory, factoryBean);

		AsyncEventQueue evenQueue = factoryBean.getObject();

		assertNotNull(evenQueue);
		assertEquals("12345", evenQueue.getId());
	}
}
