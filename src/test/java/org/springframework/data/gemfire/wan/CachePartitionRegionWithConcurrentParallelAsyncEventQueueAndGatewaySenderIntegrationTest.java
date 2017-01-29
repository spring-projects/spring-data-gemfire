/*
 * Copyright 2010-2018 the original author or authors.
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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.asyncqueue.AsyncEvent;
import org.apache.geode.cache.asyncqueue.AsyncEventListener;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewaySender;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The CachePartitionRegionWithConcurrentParallelAsyncEventQueueAndGatewaySenderIntegrationTest class is a test suite
 * of test cases testing the concurrent, parallel functionality configuration of GemFire AsyncEventQueues
 * and GatewaySenders using Spring Data GemFire.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @see org.apache.geode.cache.wan.GatewaySender
 * @since 1.5.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class CachePartitionRegionWithConcurrentParallelAsyncEventQueueAndGatewaySenderIntegrationTest {

	@Autowired
	private AsyncEventQueue exampleQueue;

	@Autowired
	private GatewaySender exampleGateway;

	@Resource(name = "ExampleRegion")
	private Region<?, ?> exampleRegion;

	@Test
	public void testPartitionRegionWithConcurrentParallelAsyncEventQueueAndGatewaySenderConfiguration() {
		assertNotNull("The 'ExampleRegion' PARTITION Region was not properly configured and initialized!", exampleRegion);
		assertEquals("ExampleRegion", exampleRegion.getName());
		assertEquals("/ExampleRegion", exampleRegion.getFullPath());
		assertNotNull(exampleRegion.getAttributes());
		assertEquals(DataPolicy.PARTITION, exampleRegion.getAttributes().getDataPolicy());
		assertTrue(exampleRegion.getAttributes().getAsyncEventQueueIds().contains("ExampleQueue"));
		assertTrue(exampleRegion.getAttributes().getGatewaySenderIds().contains("ExampleGateway"));
	}

	@Test
	public void testConcurrentParallelAsyncEventQueue() {
		assertNotNull("The 'ExampleQueue' AsyncEventQueue was not properly configured and initialized!", exampleQueue);
		assertEquals("ExampleQueue", exampleQueue.getId());
		assertNotNull(exampleQueue.getAsyncEventListener());
		assertEquals(4, exampleQueue.getDispatcherThreads());
		assertTrue(exampleQueue.isParallel());
	}

	@Test
	public void testConcurrentParallelGatewaySender() {
		assertNotNull("The 'ExampleGateway' was not properly configured and initialized!", exampleGateway);
		assertEquals("ExampleGateway", exampleGateway.getId());
		assertEquals(123, exampleGateway.getRemoteDSId());
		assertEquals(8, exampleGateway.getDispatcherThreads());
		assertTrue(exampleGateway.isParallel());
		assertFalse(exampleGateway.isRunning());
	}

	@SuppressWarnings("unused")
	public static final class TestAsyncEventListener implements AsyncEventListener {
		@Override public boolean processEvents(final List<AsyncEvent> events) {
			return false;
		}
		@Override public void close() {
		}
	}

}
