/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.apache.geode.cache.EntryEvent;
import org.apache.geode.cache.asyncqueue.AsyncEvent;
import org.apache.geode.cache.asyncqueue.AsyncEventListener;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewayQueueEvent;
import org.apache.geode.cache.wan.GatewaySender;

import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

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
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @since 1.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("all")
public class AsyncEventQueueNamespaceTest {

	@Resource(name = "TestAsyncEventQueue")
	private AsyncEventQueue asyncEventQueue;

	@Resource(name = "TestAsyncEventQueueWithFilters")
	private AsyncEventQueue asyncEventQueueWithFilters;

	@Resource(name = "TestPausedAsyncEventQueue")
	private AsyncEventQueue pausedAsyncEventQueue;

	@Test
	public void asyncEventQueueIsConfiguredProperly() {

		assertThat(asyncEventQueue).isNotNull();
		assertThat(asyncEventQueue.getId()).isEqualTo("TestAsyncEventQueue");
		assertThat(asyncEventQueue.isBatchConflationEnabled()).isTrue();
		assertThat(asyncEventQueue.getBatchSize()).isEqualTo(100);
		assertThat(asyncEventQueue.getBatchTimeInterval()).isEqualTo(30);
		assertThat(asyncEventQueue.getDiskStoreName()).isEqualTo("TestDiskStore");
		assertThat(asyncEventQueue.isDiskSynchronous()).isTrue();
		assertThat(asyncEventQueue.getDispatcherThreads()).isEqualTo(4);
		assertThat(asyncEventQueue.isDispatchingPaused()).isFalse();
		assertThat(asyncEventQueue.isForwardExpirationDestroy()).isTrue();
		assertThat(asyncEventQueue.getMaximumQueueMemory()).isEqualTo(50);
		assertThat(asyncEventQueue.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.KEY);
		assertThat(asyncEventQueue.isParallel()).isFalse();
		assertThat(asyncEventQueue.isPersistent()).isTrue();
	}

	@Test
	public void asyncEventQueueListenerEqualsExpected() {

		AsyncEventListener asyncEventListener = asyncEventQueue.getAsyncEventListener();

		assertThat(asyncEventListener).isNotNull();
		assertThat(asyncEventListener.toString()).isEqualTo("TestAeqListener");
	}

	@Test
	public void asyncEventQueueWithFiltersIsConfiguredProperly() {

		assertThat(asyncEventQueueWithFilters).isNotNull();
		assertThat(asyncEventQueueWithFilters.getId()).isEqualTo("TestAsyncEventQueueWithFilters");
		assertThat(asyncEventQueueWithFilters.isDispatchingPaused()).isFalse();

		AsyncEventListener listener = asyncEventQueueWithFilters.getAsyncEventListener();

		assertThat(listener).isNotNull();
		assertThat(listener.toString()).isEqualTo("TestListenerOne");

		List<GatewayEventFilter> gatewayEventFilters = asyncEventQueueWithFilters.getGatewayEventFilters();

		assertThat(gatewayEventFilters).isNotNull();
		assertThat(gatewayEventFilters).hasSize(2);
		assertThat(gatewayEventFilters.stream().map(Object::toString).collect(Collectors.toList()))
			.containsExactly("GatewayEventFilterOne", "GatewayEventFilterTwo");

		GatewayEventSubstitutionFilter<?, ?> gatewayEventSubstitutionFilter =
			asyncEventQueueWithFilters.getGatewayEventSubstitutionFilter();

		assertThat(gatewayEventSubstitutionFilter).isNotNull();
		assertThat(gatewayEventSubstitutionFilter.toString()).isEqualTo("GatewayEventSubstitutionFilterOne");
	}

	@Test
	public void pausedAsyncEventQueueIsConfiguredProperly() {

		assertThat(pausedAsyncEventQueue).isNotNull();
		assertThat(pausedAsyncEventQueue.getId()).isEqualTo("TestPausedAsyncEventQueue");
		assertThat(pausedAsyncEventQueue.isDispatchingPaused()).isTrue();
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
