/*
 * Copyright 2010-2020 the original author or authors.
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.cache.wan.GatewaySenderFactory;

import org.junit.Test;

import org.springframework.data.gemfire.TestUtils;

/**
 * The GatewaySenderFactoryBeanTest class is a test suite of test cases testing the contract and functionality of the
 * GatewaySenderFactoryBean class.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.apache.geode.cache.util.Gateway
 * @see org.apache.geode.cache.wan.GatewaySenderFactory
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.wan.GatewaySenderFactoryBean
 * @since 1.4.0
 */
public class GatewaySenderFactoryBeanTest {

	private Cache mockCacheWithGatewayInfrastructure(GatewaySenderFactory gatewaySenderFactory) {

		Cache mockCache = mock(Cache.class);

		when(mockCache.createGatewaySenderFactory()).thenReturn(gatewaySenderFactory);

		return mockCache;
	}

	private GatewaySenderFactory mockGatewaySenderFactory(String gatewaySenderName, int remoteDistributedSystemId) {

		GatewaySenderFactory mockGatewaySenderFactory = mock(GatewaySenderFactory.class);

		GatewaySender mockGatewaySender = mock(GatewaySender.class);

		when(mockGatewaySender.getId()).thenReturn(gatewaySenderName);
		when(mockGatewaySender.getRemoteDSId()).thenReturn(remoteDistributedSystemId);
		when(mockGatewaySenderFactory.create(eq(gatewaySenderName), eq(remoteDistributedSystemId)))
			.thenReturn(mockGatewaySender);

		return mockGatewaySenderFactory;
	}

	protected void verifyExpectations(GatewaySenderFactoryBean factoryBean,
			GatewaySenderFactory mockGatewaySenderFactory) throws Exception {

		Boolean parallel = TestUtils.readField("parallel", factoryBean);

		verify(mockGatewaySenderFactory).setParallel(eq(Boolean.TRUE.equals(parallel)));

		GatewaySender.OrderPolicy orderPolicy = TestUtils.readField("orderPolicy", factoryBean);

		if (orderPolicy != null) {
			verify(mockGatewaySenderFactory).setOrderPolicy(eq(orderPolicy));
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
	public void concurrentParallelGatewaySenderCreation() throws Exception {

		GatewaySenderFactory mockGatewaySenderFactory =
			mockGatewaySenderFactory("g0", 69);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			mockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g0");
		factoryBean.setRemoteDistributedSystemId(69);
		factoryBean.setParallel(true);
		factoryBean.setDispatcherThreads(8);
		factoryBean.doInit();

		verifyExpectations(factoryBean, mockGatewaySenderFactory);

		GatewaySender gatewaySender = factoryBean.getObject();

		assertNotNull(gatewaySender);
		assertEquals("g0", gatewaySender.getId());
		assertEquals(69, gatewaySender.getRemoteDSId());
	}

	@Test
	public void concurrentSerialGatewaySenderCreation() throws Exception {

		GatewaySenderFactory mockGatewaySenderFactory =
			mockGatewaySenderFactory("g4", 21);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			mockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

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
	public void parallelGatewaySenderCreation() throws Exception {

		GatewaySenderFactory mockGatewaySenderFactory =
			mockGatewaySenderFactory("g1", 69);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			mockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

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

	@Test
	public void gatewaySenderCreationWithDiskSynchronousAndNoPersistence() throws Exception {

		GatewaySenderFactory mockGatewaySenderFactory =
			mockGatewaySenderFactory("g7", 51);

		GatewaySenderFactoryBean factoryBean =
			new GatewaySenderFactoryBean(mockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g7");
		factoryBean.setRemoteDistributedSystemId(51);
		factoryBean.setParallel(false);
		factoryBean.setDispatcherThreads(1);
		factoryBean.doInit();

		verifyExpectations(factoryBean, mockGatewaySenderFactory);

		GatewaySender gatewaySender = factoryBean.getObject();

		assertNotNull(gatewaySender);
		assertEquals("g7", gatewaySender.getId());
		assertEquals(51, gatewaySender.getRemoteDSId());
	}

	@Test
	public void gatewaySenderCreationWithOverflowDiskStoreAndNoPersistence() throws Exception {

		GatewaySenderFactory mockGatewaySenderFactory =
			mockGatewaySenderFactory("g6", 51);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			mockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g6");
		factoryBean.setRemoteDistributedSystemId(51);
		factoryBean.setPersistent(false);
		factoryBean.setDiskStoreReference("queueOverflowDiskStore");
		factoryBean.doInit();

		verifyExpectations(factoryBean, mockGatewaySenderFactory);

		GatewaySender gatewaySender = factoryBean.getObject();

		assertNotNull(gatewaySender);
		assertEquals("g6", gatewaySender.getId());
		assertEquals(51, gatewaySender.getRemoteDSId());
	}

	@Test
	public void gatewaySenderCreationWithOrderPolicyAndDispatcherThreads() throws Exception {

		GatewaySenderFactory mockGatewaySenderFactory =
			mockGatewaySenderFactory("g5", 42);

		GatewaySenderFactoryBean factoryBean = new GatewaySenderFactoryBean(
			mockCacheWithGatewayInfrastructure(mockGatewaySenderFactory));

		factoryBean.setName("g5");
		factoryBean.setRemoteDistributedSystemId(42);
		factoryBean.setDiskSynchronous(true);
		factoryBean.doInit();

		verifyExpectations(factoryBean, mockGatewaySenderFactory);

		GatewaySender gatewaySender = factoryBean.getObject();

		assertNotNull(gatewaySender);
		assertEquals("g5", gatewaySender.getId());
		assertEquals(42, gatewaySender.getRemoteDSId());
	}
}
