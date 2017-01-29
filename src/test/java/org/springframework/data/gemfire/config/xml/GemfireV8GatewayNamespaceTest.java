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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.apache.geode.cache.EntryEvent;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewaySender;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The GemfireV8GatewayNamespaceTest class is a test suite of test cases testing the contract and functionality of
 * GemFire 8 Gateway Sender/Receiver support.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.data.gemfire.wan.GatewaySenderFactoryBean
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.apache.geode.cache.wan.GatewayEventSubstitutionFilter
 * @since 2.0.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(value = "gateway-v8-ns.xml", initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class GemfireV8GatewayNamespaceTest {

	@Autowired
	@Qualifier("gateway-sender-with-event-substitution-filter")
	private GatewaySender gatewaySenderWithEventSubstitutionFilter;

	@Autowired
	@Qualifier("gateway-sender-with-event-substitution-filter-ref")
	private GatewaySender gatewaySenderWithEventSubstitutionFilterRef;

	@Test
	public void testGatewaySenderEventSubstitutionFilter() {
		assertNotNull("The 'gatewaySenderEventSubtitutionFilter' bean was not properly configured and initialized!",
			gatewaySenderWithEventSubstitutionFilter);
		assertEquals("gateway-sender-with-event-substitution-filter", gatewaySenderWithEventSubstitutionFilter.getId());
		assertEquals(3, gatewaySenderWithEventSubstitutionFilter.getRemoteDSId());
		assertEquals(10, gatewaySenderWithEventSubstitutionFilter.getDispatcherThreads());
		assertTrue(gatewaySenderWithEventSubstitutionFilter.isParallel());
		assertFalse(gatewaySenderWithEventSubstitutionFilter.isRunning());
		assertNotNull(gatewaySenderWithEventSubstitutionFilter.getGatewayEventSubstitutionFilter());
		assertEquals("inner", gatewaySenderWithEventSubstitutionFilter.getGatewayEventSubstitutionFilter().toString());
	}

	@Test
	public void testGatewaySenderEventSubstitutionFilterRef() {
		assertNotNull("The 'gatewaySenderEventSubtitutionFilter' bean was not properly configured and initialized!",
			gatewaySenderWithEventSubstitutionFilterRef);
		assertEquals("gateway-sender-with-event-substitution-filter-ref", gatewaySenderWithEventSubstitutionFilterRef.getId());
		assertEquals(33, gatewaySenderWithEventSubstitutionFilterRef.getRemoteDSId());
		assertEquals(1, gatewaySenderWithEventSubstitutionFilterRef.getDispatcherThreads());
		assertFalse(gatewaySenderWithEventSubstitutionFilterRef.isParallel());
		assertFalse(gatewaySenderWithEventSubstitutionFilterRef.isRunning());
		assertNotNull(gatewaySenderWithEventSubstitutionFilterRef.getGatewayEventSubstitutionFilter());
		assertEquals("ref", gatewaySenderWithEventSubstitutionFilterRef.getGatewayEventSubstitutionFilter().toString());
	}

	public static class TestGatewayEventSubstitutionFilter implements GatewayEventSubstitutionFilter<Object, Object> {

		private String name;

		public final void setName(final String name) {
			this.name = name;
		}

		protected String getName() {
			return name;
		}

		@Override
		public Object getSubstituteValue(final EntryEvent<Object, Object> objectObjectEntryEvent) {
			throw new UnsupportedOperationException("Not Implemented!");
		}

		@Override
		public void close() {
		}

		@Override
		public String toString() {
			return getName();
		}
	}

}
