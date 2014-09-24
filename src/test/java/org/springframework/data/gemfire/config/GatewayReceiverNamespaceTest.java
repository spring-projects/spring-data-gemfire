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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.wan.GatewayReceiver;

/**
 * The GatewayReceiverNamespaceTest class is a test suite of test cases testing the contract and functionality of
 * Gateway Receiver configuration in Spring Data GemFire using the XML namespace and schema (XSD).
 *
 * @author John Blum
 * @see org.junit.Test
 * @since 1.5.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class GatewayReceiverNamespaceTest {

	@Resource(name = "&Default")
	private GatewayReceiverFactoryBean defaultFactoryBean;

	@Resource(name = "&Auto")
	private GatewayReceiverFactoryBean autoGatewayReceiver;

	@Resource(name = "&Manual")
	private GatewayReceiverFactoryBean manualGatewayReceiver;

	@Test
	// TODO test the default value for "manual-start" (true) without explicitly setting the attribute in Spring XML
	public void testDefault() throws Exception {
		assertNotNull("The 'Default' GatewayReceiverFactoryBean was not properly configured and initialized!", defaultFactoryBean);
		assertFalse(defaultFactoryBean.isAutoStartup());

		GatewayReceiver defaultGatewayReceiver = defaultFactoryBean.getObject();

		assertNotNull(defaultGatewayReceiver);
		assertEquals("192.168.0.1", defaultGatewayReceiver.getBindAddress());
		assertEquals("skullbox", defaultGatewayReceiver.getHost());
		assertEquals(12345, defaultGatewayReceiver.getStartPort());
		assertEquals(54321, defaultGatewayReceiver.getEndPort());
		assertEquals(5000, defaultGatewayReceiver.getMaximumTimeBetweenPings());
		assertEquals(32768, defaultGatewayReceiver.getSocketBufferSize());
	}

	@Test
	public void testAuto() {
		assertNotNull("The 'Auto' GatewayReceiverFactoryBean was not properly configured and initialized!", autoGatewayReceiver);
		assertTrue(autoGatewayReceiver.isAutoStartup());
	}

	@Test
	public void testManual() {
		assertNotNull("The 'Manual' GatewayReceiverFactoryBean was not properly configured and initialized!", manualGatewayReceiver);
		assertFalse(manualGatewayReceiver.isAutoStartup());
	}

}
