/*
 * Copyright 2012-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.geode.cache.EntryEvent;
import org.apache.geode.cache.asyncqueue.AsyncEvent;
import org.apache.geode.cache.asyncqueue.AsyncEventListener;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewayQueueEvent;
import org.apache.geode.cache.wan.GatewaySender;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
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

	@Test
	public void asyncEventQueueIsConfiguredProperly() {

		Assert.assertThat(asyncEventQueue, is(notNullValue(AsyncEventQueue.class)));
		Assert.assertThat(asyncEventQueue.getId(), is(equalTo("TestAsyncEventQueue")));
		Assert.assertThat(asyncEventQueue.isBatchConflationEnabled(), is(true));
		Assert.assertThat(asyncEventQueue.getBatchSize(), is(equalTo(100)));
		Assert.assertThat(asyncEventQueue.getBatchTimeInterval(), is(equalTo(30)));
		Assert.assertThat(asyncEventQueue.getDiskStoreName(), is(equalTo("TestDiskStore")));
		Assert.assertThat(asyncEventQueue.isDiskSynchronous(), is(true));
		Assert.assertThat(asyncEventQueue.getDispatcherThreads(), is(equalTo(4)));
		Assert.assertThat(asyncEventQueue.isForwardExpirationDestroy(), is(true));
		Assert.assertThat(asyncEventQueue.getMaximumQueueMemory(), is(equalTo(50)));
		Assert.assertThat(asyncEventQueue.getOrderPolicy(), is(equalTo(GatewaySender.OrderPolicy.KEY)));
		Assert.assertThat(asyncEventQueue.isParallel(), is(false));
		Assert.assertThat(asyncEventQueue.isPersistent(), is(true));
	}

	@Test
	public void asyncEventQueueListenerEqualsExpected() {

		AsyncEventListener asyncEventListener = asyncEventQueue.getAsyncEventListener();

		Assert.assertThat(asyncEventListener, is(notNullValue(AsyncEventListener.class)));
		Assert.assertThat(asyncEventListener.toString(), is(equalTo("TestAeqListener")));
	}

	@Test
	public void asyncEventQueueWithFiltersIsConfiguredProperly() {

		assertThat(asyncEventQueueWithFilters).isNotNull();
		assertThat(asyncEventQueueWithFilters.getId()).isEqualTo("TestAsyncEventQueueWithFilters");

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
