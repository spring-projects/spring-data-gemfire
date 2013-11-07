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
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.springframework.data.gemfire.TestUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory;

/**
 * The AsyncEventQueueFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the AsyncEventQueueFactoryBean class.
 * <p/>
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.wan.AsyncEventQueueFactoryBean
 * @since 1.3.3
 */
public class AsyncEventQueueFactoryBeanTest {

	protected Cache createMockCacheWithAsyncInfrastructure(final String asyncEventQueueId) {
		Cache mockCache = mock(Cache.class);
		AsyncEventQueueFactory mockAsynEventQueueFactory = mock(AsyncEventQueueFactory.class);
		AsyncEventQueue mockAsyncEventQueue = mock(AsyncEventQueue.class);

		when((mockCache.createAsyncEventQueueFactory())).thenReturn(mockAsynEventQueueFactory);
		when(mockAsynEventQueueFactory.create(eq(asyncEventQueueId), notNull(AsyncEventListener.class)))
			.thenReturn(mockAsyncEventQueue);
		when(mockAsyncEventQueue.getId()).thenReturn(asyncEventQueueId);

		return mockCache;
	}

	protected AsyncEventListener createMockAsyncEventListener() {
		return mock(AsyncEventListener.class);
	}

	@Test
	public void testSetAsyncEventListener() throws Exception {
		AsyncEventQueueFactoryBean factoryBean = new AsyncEventQueueFactoryBean(
			createMockCacheWithAsyncInfrastructure("testEventQueue"));

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
			createMockCacheWithAsyncInfrastructure(asyncEventQueueId));

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
				createMockCacheWithAsyncInfrastructure("testEventQueue"));

			assertNull(TestUtils.readField("asyncEventListener", factoryBean));

			factoryBean.doInit();
		}
		catch (Exception e) {
			assertEquals("The AsyncEventListener cannot be null.", e.getMessage());
			throw e;
		}
	}

}
