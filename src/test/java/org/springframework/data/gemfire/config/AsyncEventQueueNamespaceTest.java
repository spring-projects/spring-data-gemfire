/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.asyncqueue.AsyncEvent;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.wan.GatewaySender;

/**
 * The AsyncEventQueueNamespaceTest class is a test suite of test cases testing the contract and functionality
 * of configuring a Pivotal GemFire or Apache Geode {@link AsyncEventQueue} using the SDG XML namespace.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.config.AsyncEventQueueParser
 * @see org.springframework.data.gemfire.wan.AsyncEventQueueFactoryBean
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue
 * @since 1.0.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("all")
public class AsyncEventQueueNamespaceTest {

	@Autowired
	private AsyncEventQueue asyncEventQueue;

	@Test
	public void asyncEventQueueIsConfiguredProperly() {
		assertThat(asyncEventQueue, is(notNullValue(AsyncEventQueue.class)));
		assertThat(asyncEventQueue.getId(), is(equalTo("TestAsyncEventQueue")));
		assertThat(asyncEventQueue.isBatchConflationEnabled(), is(true));
		assertThat(asyncEventQueue.getBatchSize(), is(equalTo(100)));
		assertThat(asyncEventQueue.getBatchTimeInterval(), is(equalTo(30)));
		assertThat(asyncEventQueue.getDiskStoreName(), is(equalTo("TestDiskStore")));
		assertThat(asyncEventQueue.isDiskSynchronous(), is(true));
		assertThat(asyncEventQueue.getDispatcherThreads(), is(equalTo(4)));
		assertThat(asyncEventQueue.isForwardExpirationDestroy(), is(false));
		assertThat(asyncEventQueue.getMaximumQueueMemory(), is(equalTo(50)));
		assertThat(asyncEventQueue.getOrderPolicy(), is(equalTo(GatewaySender.OrderPolicy.KEY)));
		assertThat(asyncEventQueue.isParallel(), is(false));
		assertThat(asyncEventQueue.isPersistent(), is(true));
	}

	@Test
	public void asyncEventQueueListenerEqualsExpected() {
		AsyncEventListener asyncEventListener = asyncEventQueue.getAsyncEventListener();

		assertThat(asyncEventListener, is(notNullValue(AsyncEventListener.class)));
		assertThat(asyncEventListener.toString(), is(equalTo("TestAeqListener")));
	}

	public static class TestAsyncEventListener implements AsyncEventListener {

		private final String name;

		public TestAsyncEventListener(String name) {
			this.name = name;
		}

		@Override
		public boolean processEvents(List<AsyncEvent> events) {
			return false;
		}

		@Override
		public void close() {
		}

		@Override
		public String toString() {
			return this.name;
		}
	}
}
