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
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.springframework.data.gemfire.TestUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewaySenderFactory;

/**
 * The GatewaySenderFactoryBeanTest class is a test suite of test cases testing the contract and functionality of the
 * GatewaySenderFactoryBean class.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.wan.GatewaySender
 * @see com.gemstone.gemfire.cache.util.Gateway
 * @see com.gemstone.gemfire.cache.wan.GatewaySenderFactory
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.wan.GatewaySenderFactoryBean
 * @since 1.4.0
 */
public class GatewaySenderFactoryBeanTest {

	protected Cache createMockCacheWithGatewayInfrastructure(final GatewaySenderFactory gatewaySenderFactory) {
		Cache mockCache = mock(Cache.class);
		when(mockCache.createGatewaySenderFactory()).thenReturn(gatewaySenderFactory);
		return mockCache;
	}

	protected GatewaySenderFactory createMockGatewaySenderFactory(final String gatewaySenderName,
			final int remoteDistributedSystemId) {
		GatewaySenderFactory mockGatewaySenderFactory = mock(GatewaySenderFactory.class);
		GatewaySender mockGatewaySender = mock(GatewaySender.class);

		when(mockGatewaySender.getId()).thenReturn(gatewaySenderName);
		when(mockGatewaySender.getRemoteDSId()).thenReturn(remoteDistributedSystemId);
		when(mockGatewaySenderFactory.create(eq(gatewaySenderName), eq(remoteDistributedSystemId)))
			.thenReturn(mockGatewaySender);

		return mockGatewaySenderFactory;
	}

	protected void verifyExpectations(final GatewaySenderFactoryBean factoryBean,
			final GatewaySenderFactory mockGatewaySenderFactory) throws Exception {
		Boolean parallel = TestUtils.readField("parallel", factoryBean);

		verify(mockGatewaySenderFactory).setParallel(eq(Boolean.TRUE.equals(parallel)));

		String orderPolicy = TestUtils.readField("orderPolicy", factoryBean);

		if (orderPolicy != null) {
			verify(mockGatewaySenderFactory).setOrderPolicy(eq(Gateway.OrderPolicy.valueOf(orderPolicy.toUpperCase())));
		}

		Integer dispatcherThreads = TestUtils.readField("dispatcherThreads", factoryBean);

		if (dispatcherThreads != null) {
			verify(mockGatewaySenderFactory).setDispatcherThreads(eq(dispatcherThreads));
		}

		String diskStoreReference = TestUtils.readField("diskStoreReference", factoryBean);

		if (diskStoreReference != null) {
			verify(mockGatewaySenderFactory).setDiskStoreName(eq(diskStoreReference));
		}

		Boolean diskSynchronous = TestUtils.readField("diskSynchronous", factoryBean);

		if (diskSynchronous != null) {
			verify(mockGatewaySenderFactory).setDiskSynchronous(eq(diskSynchronous));
		}

		Boolean persistent = TestUtils.readField("persistent", factoryBean);

		if (persistent != null) {
			verify(mockGatewaySenderFactory).setPersistenceEnabled(eq(persistent));
		}
		else {
			verify(mockGatewaySenderFactory, never()).setPersistenceEnabled(true);
		}
	}

	@Test
	public void testParallelGatewaySender() throws Exception {
		GatewaySenderFactory mockGatewaySenderFactory = createMockGatewaySenderFactory("g1", 69);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			createMockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g1");
		factoryBean.setRemoteDistributedSystemId(69);
		factoryBean.setParallel(true);
		factoryBean.doInit();

		verifyExpectations(factoryBean, mockGatewaySenderFactory);

		GatewaySender gatewaySender = factoryBean.getObject();

		assertNotNull(gatewaySender);
		assertEquals("g1", gatewaySender.getId());
		assertEquals(69, gatewaySender.getRemoteDSId());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testParallelGatewaySenderWithOrderPolicy() {
		GatewaySenderFactory mockGatewaySenderFactory = createMockGatewaySenderFactory("g2", 69);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			createMockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g2");
		factoryBean.setRemoteDistributedSystemId(69);
		factoryBean.setParallel(true);
		factoryBean.setOrderPolicy("KEY");

		try {
			factoryBean.doInit();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Order Policy cannot be used with a Parallel Gateway Sender Queue.", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testParallelGatewaySenderWithDispatcherThreads() {
		GatewaySenderFactory mockGatewaySenderFactory = createMockGatewaySenderFactory("g3", 69);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			createMockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g3");
		factoryBean.setRemoteDistributedSystemId(69);
		factoryBean.setParallel(true);
		factoryBean.setDispatcherThreads(1);

		try {
			factoryBean.doInit();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The number of Dispatcher Threads cannot be specified with a Parallel Gateway Sender Queue.",
				expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSerialGatewaySenderWithDispatcherThreads() throws Exception {
		GatewaySenderFactory mockGatewaySenderFactory = createMockGatewaySenderFactory("g4", 21);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			createMockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g4");
		factoryBean.setRemoteDistributedSystemId(21);
		factoryBean.setParallel(false);
		factoryBean.setDispatcherThreads(1);
		factoryBean.doInit();

		verifyExpectations(factoryBean, mockGatewaySenderFactory);

		GatewaySender gatewaySender = factoryBean.getObject();

		assertNotNull(gatewaySender);
		assertEquals("g4", gatewaySender.getId());
		assertEquals(21, gatewaySender.getRemoteDSId());
	}

	@Test
	public void testGatewaySenderWithOrderPolicyAndDispatcherThreads() throws Exception {
		GatewaySenderFactory mockGatewaySenderFactory = createMockGatewaySenderFactory("g5", 42);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			createMockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g5");
		factoryBean.setRemoteDistributedSystemId(42);
		factoryBean.setOrderPolicy("THREAD");
		factoryBean.setDispatcherThreads(1);
		factoryBean.doInit();

		verifyExpectations(factoryBean, mockGatewaySenderFactory);

		GatewaySender gatewaySender = factoryBean.getObject();

		assertNotNull(gatewaySender);
		assertEquals("g5", gatewaySender.getId());
		assertEquals(42, gatewaySender.getRemoteDSId());
	}

	@Test
	public void testGatewaySenderWithOverflowDiskStoreNoPersistence() throws Exception {
		GatewaySenderFactory mockGatewaySenderFactory = createMockGatewaySenderFactory("g6", 51);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			createMockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g6");
		factoryBean.setRemoteDistributedSystemId(51);
		factoryBean.setPersistent(false);
		factoryBean.setDiskStoreRef("queueOverflowDiskStore");
		factoryBean.doInit();

		verifyExpectations(factoryBean, mockGatewaySenderFactory);

		GatewaySender gatewaySender = factoryBean.getObject();

		assertNotNull(gatewaySender);
		assertEquals("g6", gatewaySender.getId());
		assertEquals(51, gatewaySender.getRemoteDSId());
	}

	@Test
	public void testGatewaySenderWithDiskSynchronousSetPersistenceUnset() throws Exception {
		GatewaySenderFactory mockGatewaySenderFactory = createMockGatewaySenderFactory("g7", 51);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			createMockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g7");
		factoryBean.setRemoteDistributedSystemId(51);
		factoryBean.setDiskSynchronous(true);
		factoryBean.doInit();

		verifyExpectations(factoryBean, mockGatewaySenderFactory);

		GatewaySender gatewaySender = factoryBean.getObject();

		assertNotNull(gatewaySender);
		assertEquals("g7", gatewaySender.getId());
		assertEquals(51, gatewaySender.getRemoteDSId());
	}

}
