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
import com.gemstone.gemfire.cache.util.Gateway;

/**
 * The AsyncEventQueueFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the AsyncEventQueueFactoryBean class.
 * <p/>
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

	protected Cache createMockCacheWithAsyncEventQueueInfrastructure(
			final AsyncEventQueueFactory mockAsynEventQueueFactory) {
		Cache mockCache = mock(Cache.class);
		when((mockCache.createAsyncEventQueueFactory())).thenReturn(mockAsynEventQueueFactory);
		return mockCache;
	}

	protected AsyncEventQueueFactory createMockAsyncEventQueueFactory(final String asyncEventQueueId) {
		AsyncEventQueueFactory mockAsyncEventQueueFactory = mock(AsyncEventQueueFactory.class);
		AsyncEventQueue mockAsyncEventQueue = mock(AsyncEventQueue.class);

		when(mockAsyncEventQueue.getId()).thenReturn(asyncEventQueueId);
		when(mockAsyncEventQueueFactory.create(eq(asyncEventQueueId), notNull(AsyncEventListener.class)))
			.thenReturn(mockAsyncEventQueue);

		return mockAsyncEventQueueFactory;
	}

	protected AsyncEventListener createMockAsyncEventListener() {
		return mock(AsyncEventListener.class);
	}

	protected void verifyExpectations(final AsyncEventQueueFactory mockAsyncEventQueueFactory,
			final AsyncEventQueueFactoryBean factoryBean) throws Exception {
		Boolean parallel = TestUtils.readField("parallel", factoryBean);

		verify(mockAsyncEventQueueFactory).setParallel(eq(Boolean.TRUE.equals(parallel)));

		String orderPolicy = TestUtils.readField("orderPolicy", factoryBean);

		if (orderPolicy != null) {
			verify(mockAsyncEventQueueFactory).setOrderPolicy(eq(Gateway.OrderPolicy.valueOf(orderPolicy.toUpperCase())));
		}

		Integer dispatcherThreads = TestUtils.readField("dispatcherThreads", factoryBean);

		if (dispatcherThreads != null) {
			verify(mockAsyncEventQueueFactory).setDispatcherThreads(eq(dispatcherThreads));
		}

		String diskStoreReference = TestUtils.readField("diskStoreReference", factoryBean);

		if (diskStoreReference != null) {
			verify(mockAsyncEventQueueFactory).setDiskStoreName(eq(diskStoreReference));
		}

		Boolean diskSynchronous = TestUtils.readField("diskSynchronous", factoryBean);

		if (diskSynchronous != null) {
			verify(mockAsyncEventQueueFactory).setDiskSynchronous(eq(diskSynchronous));
		}

		Boolean persistent = TestUtils.readField("persistent", factoryBean);

		if (persistent != null) {
			verify(mockAsyncEventQueueFactory).setPersistent(eq(persistent));
		}
		else {
			verify(mockAsyncEventQueueFactory, never()).setPersistent(true);
		}
	}

	@Test
	public void testSetAsyncEventListener() throws Exception {
		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(createMockAsyncEventQueueFactory("testEventQueue")));

		AsyncEventListener listenerOne = createMockAsyncEventListener();

		factoryBean.setAsyncEventListener(listenerOne);

		assertSame(listenerOne, TestUtils.readField("asyncEventListener", factoryBean));

		AsyncEventListener listenerTwo = createMockAsyncEventListener();

		factoryBean.setAsyncEventListener(listenerTwo);

		assertSame(listenerTwo, TestUtils.readField("asyncEventListener", factoryBean));
	}

	@Test(expected = IllegalStateException.class)
	public void testSetAsyncEventListenerAfterAyncEventQueueCreation() throws Exception {
		String asyncEventQueueId = "testEventQueue";

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(createMockAsyncEventQueueFactory(asyncEventQueueId)));

		factoryBean.setName(asyncEventQueueId);

		AsyncEventListener listenerOne = createMockAsyncEventListener();

		factoryBean.setAsyncEventListener(listenerOne);

		assertSame(listenerOne, TestUtils.readField("asyncEventListener", factoryBean));

		factoryBean.doInit();

		assertNotNull(TestUtils.readField("asyncEventQueue", factoryBean));

		try {
			factoryBean.setAsyncEventListener(createMockAsyncEventListener());
		}
		catch (IllegalStateException expected) {
			assertEquals("Setting an AsyncEventListener is not allowed once the AsyncEventQueue has been created.",
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
			assertEquals("The AsyncEventListener cannot be null.", e.getMessage());
			throw e;
		}
	}

	@Test
	public void testParallelAsyncEventQueue() throws Exception {
		AsyncEventQueueFactory mockAsyncEventQueueFatory = createMockAsyncEventQueueFactory("123");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFatory));

		factoryBean.setName("123");
		factoryBean.setAsyncEventListener(createMockAsyncEventListener());
		factoryBean.setParallel(true);
		factoryBean.doInit();

		verifyExpectations(mockAsyncEventQueueFatory, factoryBean);

		AsyncEventQueue eventQueue = factoryBean.getObject();

		assertNotNull(eventQueue);
		assertEquals("123", eventQueue.getId());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testParallelAsyncEventQueueWithDispatcherThreads() {
		AsyncEventQueueFactory mockAsyncEventQueueFatory = createMockAsyncEventQueueFactory("456");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFatory));

		factoryBean.setName("456");
		factoryBean.setAsyncEventListener(createMockAsyncEventListener());
		factoryBean.setDispatcherThreads(1);
		factoryBean.setParallel(true);

		try {
			factoryBean.doInit();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The number of Dispatcher Threads cannot be specified with a Parallel Event Queue.",
				expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testParallelAsyncEventQueueWithOrderPolicy() {
		AsyncEventQueueFactory mockAsyncEventQueueFatory = createMockAsyncEventQueueFactory("456");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFatory));

		factoryBean.setName("456");
		factoryBean.setAsyncEventListener(createMockAsyncEventListener());
		factoryBean.setOrderPolicy("Key");
		factoryBean.setParallel(true);

		try {
			factoryBean.doInit();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Order Policy cannot be used with a Parallel Event Queue.",
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
		factoryBean.setAsyncEventListener(createMockAsyncEventListener());
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
		AsyncEventQueueFactory mockAsyncEventQueueFatory = createMockAsyncEventQueueFactory("789");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFatory));

		factoryBean.setName("789");
		factoryBean.setAsyncEventListener(createMockAsyncEventListener());
		factoryBean.setDispatcherThreads(2);
		factoryBean.setOrderPolicy("THREAD");
		factoryBean.doInit();

		verifyExpectations(mockAsyncEventQueueFatory, factoryBean);

		AsyncEventQueue eventQueue = factoryBean.getObject();

		assertNotNull(eventQueue);
		assertEquals("789", eventQueue.getId());
	}

	@Test
	public void testAsyncEventQueueWithOverflowDiskStoreNoPersistence() throws Exception {
		AsyncEventQueueFactory mockAsyncEventQueueFactory = createMockAsyncEventQueueFactory("123abc");

		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncEventQueueInfrastructure(mockAsyncEventQueueFactory));

		factoryBean.setName("123abc");
		factoryBean.setAsyncEventListener(createMockAsyncEventListener());
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
		factoryBean.setAsyncEventListener(createMockAsyncEventListener());
		factoryBean.setDiskSynchronous(true);
		factoryBean.doInit();

		verifyExpectations(mockAsyncEventQueueFactory, factoryBean);

		AsyncEventQueue evenQueue = factoryBean.getObject();

		assertNotNull(evenQueue);
		assertEquals("12345", evenQueue.getId());
	}

}
