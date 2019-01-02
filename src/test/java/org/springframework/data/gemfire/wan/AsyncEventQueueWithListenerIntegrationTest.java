/*
 * Copyright 2010-2019 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.List;

import javax.annotation.Resource;

import com.gemstone.gemfire.cache.EntryEvent;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEvent;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.wan.GatewayEventFilter;
import com.gemstone.gemfire.cache.wan.GatewayEventSubstitutionFilter;
import com.gemstone.gemfire.cache.wan.GatewayQueueEvent;
import com.gemstone.gemfire.cache.wan.GatewaySender;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The AsyncEventQueueWithListenerIntegrationTest class is a test suite of test cases testing the circular references
 * between an Async Event Queue and a registered AsyncEventListener that refers back to the Async Event Queue
 * on which the listener is registered.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue
 * @since 1.0.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(value = "asyncEventQueueWithListener.xml", initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class AsyncEventQueueWithListenerIntegrationTest {

	@Resource(name = "Q1")
	private AsyncEventQueue queueOne;

	@Resource(name = "Q2")
	private AsyncEventQueue queueTwo;

	@Resource(name = "Q3")
	private AsyncEventQueue queueThree;

	@Resource(name = "TestAsyncEventQueueWithFilters")
	private AsyncEventQueue queueWithFilters;

	@Test
	public void testAsyncEventQueueOneAndListenerConfiguration() {

		assertNotNull(queueOne);
		assertEquals("QueueOne", queueOne.getId());
		assertFalse(queueOne.isPersistent());
		assertFalse(queueOne.isParallel());
		assertEquals(50, queueOne.getMaximumQueueMemory());
		assertEquals(4, queueOne.getDispatcherThreads());
		assertTrue(queueOne.getAsyncEventListener() instanceof TestAsyncEventListener);
		assertSame(queueOne, ((TestAsyncEventListener) queueOne.getAsyncEventListener()).getQueue());
	}
	@Test
	public void testAsyncEventQueueTwoAndListenerConfiguration() {

		assertNotNull(queueTwo);
		assertEquals("QueueTwo", queueTwo.getId());
		assertFalse(queueTwo.isPersistent());
		assertTrue(queueTwo.isParallel());
		assertEquals(150, queueTwo.getMaximumQueueMemory());
		assertEquals(GatewaySender.DEFAULT_DISPATCHER_THREADS, queueTwo.getDispatcherThreads());
		assertTrue(queueTwo.getAsyncEventListener() instanceof TestAsyncEventListener);
		assertEquals("ListenerTwo", ((TestAsyncEventListener) queueTwo.getAsyncEventListener()).getName());
	}

	@Test
	public void testAsyncEventQueueThreeAndListenerConfiguration() {

		assertNotNull(queueThree);
		assertEquals("QueueThree", queueThree.getId());
		assertFalse(queueThree.isPersistent());
		assertFalse(queueThree.isParallel());
		assertEquals(25, queueThree.getMaximumQueueMemory());
		assertEquals(2, queueThree.getDispatcherThreads());
		assertTrue(queueThree.getAsyncEventListener() instanceof TestAsyncEventListener);
		assertSame(queueThree, ((TestAsyncEventListener) queueThree.getAsyncEventListener()).getQueue());
	}

	@Test
	public void asyncEventQueueWithFiltersIsConfiguredProperly() {

		assertThat(queueWithFilters).isNotNull();
		assertThat(queueWithFilters.getId()).isEqualTo("TestAsyncEventQueueWithFilters");

		AsyncEventListener listener = queueWithFilters.getAsyncEventListener();

		assertThat(listener).isNotNull();
		assertThat(listener.toString()).isEqualTo("TestListenerOne");

		List<GatewayEventFilter> gatewayEventFilters = queueWithFilters.getGatewayEventFilters();

		assertThat(gatewayEventFilters).isNotNull();
		assertThat(gatewayEventFilters).hasSize(2);
		assertThat(gatewayEventFilters.get(0).toString()).isEqualTo("GatewayEventFilterOne");
		assertThat(gatewayEventFilters.get(1).toString()).isEqualTo("GatewayEventFilterTwo");

		GatewayEventSubstitutionFilter<?, ?> gatewayEventSubstitutionFilter =
			queueWithFilters.getGatewayEventSubstitutionFilter();

		assertThat(gatewayEventSubstitutionFilter).isNotNull();
		assertThat(gatewayEventSubstitutionFilter.toString()).isEqualTo("GatewayEventSubstitutionFilterOne");
	}

	/**
	 * The QueueAsyncEventListener class is an implementation of the AsyncEventListener interface that contains
	 * a reference to the AsyncEventQueue upon which it is registered.
	 *
	 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEvent
	 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener
	 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue
	 */
	@SuppressWarnings("unused")
	public static class TestAsyncEventListener implements AsyncEventListener {

		private AsyncEventQueue queue;

		private String name;

		public TestAsyncEventListener() {
			this.queue = null;
		}

		public TestAsyncEventListener(final AsyncEventQueue queue) {
			this.queue = queue;
		}

		public void init() {
			getQueue();
			System.out.printf("%1$s initialized!%n", this);
		}

		public AsyncEventQueue getQueue() {
			Assert.state(queue != null, String.format("A reference to the Async Event Queue on which this listener"
				+ " (%1$s) has been registered was not properly configured!", this));
			return queue;
		}

		public String getName() {
			return name;
		}

		public void setName(final String name) {
			this.name = name;
		}

		public void setQueue(final AsyncEventQueue queue) {
			this.queue = queue;
		}

		@Override
		public boolean processEvents(final List<AsyncEvent> events) {
			return false;
		}

		@Override
		public void close() {
		}

		@Override
		public String toString() {
			return (StringUtils.hasText(getName()) ? getName() : getClass().getName());
		}
	}

	public static class TestGatewayEventFilter implements GatewayEventFilter {

		private final String name;

		public TestGatewayEventFilter(String name) {
			this.name = name;
		}

		@Override
		public boolean beforeEnqueue(GatewayQueueEvent event) {
			return false;
		}

		@Override
		public boolean beforeTransmit(GatewayQueueEvent event) {
			return false;
		}

		@Override
		public void afterAcknowledgement(GatewayQueueEvent event) { }

		@Override
		public void close() { }

		@Override
		public String toString() {
			return this.name;
		}
	}

	public static class TestGatewayEventSubstitutionFilter implements GatewayEventSubstitutionFilter<Object, Object> {

		private final String name;

		public TestGatewayEventSubstitutionFilter(String name) {
			this.name = name;
		}
		@Override
		public Object getSubstituteValue(EntryEvent<Object, Object> event) {
			return null;
		}

		@Override
		public void close() { }

		@Override
		public String toString() {
			return this.name;
		}
	}
}
