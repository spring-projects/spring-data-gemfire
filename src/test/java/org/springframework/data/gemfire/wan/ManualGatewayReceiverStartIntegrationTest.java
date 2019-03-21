/*
 * Copyright 2010-2013 the original author or authors.
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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.wan.GatewayReceiver;

/**
 * The ManualGatewayReceiverStartIntegrationTest class is a test suite of test cases testing the manual start capability
 * of Gateway Receivers when configured with the Spring Data GemFire XML namespace.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ManualGatewayReceiverStartIntegrationTest {

	@Resource(name = "Auto")
	private GatewayReceiver autoGatewayReceiver;

	@Resource(name = "Manual")
	private GatewayReceiver manualGatewayReceiver;

	@Test
	public void testAutoGatewayReceiver() {
		assertNotNull("The 'Auto' GatewayReceiver was not properly configured or initialized!", autoGatewayReceiver);
		assertEquals(7070, autoGatewayReceiver.getStartPort());
		assertEquals(7700, autoGatewayReceiver.getEndPort());
		assertTrue(autoGatewayReceiver.isRunning());

		final int gatewayReceiverPort = autoGatewayReceiver.getPort();

		assertTrue(gatewayReceiverPort >= autoGatewayReceiver.getStartPort()
			&& gatewayReceiverPort <= autoGatewayReceiver.getEndPort());

		autoGatewayReceiver.stop();

		assertFalse(autoGatewayReceiver.isRunning());
	}

	@Test
	public void testManualGatewayReceiverConfiguration() throws IOException {
		assertNotNull("The 'Manual' GatewayReceiver was not properly configured or initialized!", manualGatewayReceiver);
		assertFalse(manualGatewayReceiver.isRunning());
		assertEquals(6060, manualGatewayReceiver.getStartPort());
		assertEquals(6600, manualGatewayReceiver.getEndPort());

		manualGatewayReceiver.start();

		assertTrue(manualGatewayReceiver.isRunning());

		final int gateReceiverPort = manualGatewayReceiver.getPort();

		assertTrue(gateReceiverPort >= manualGatewayReceiver.getStartPort()
			&& gateReceiverPort <= manualGatewayReceiver.getEndPort());

		manualGatewayReceiver.stop();

		assertFalse(manualGatewayReceiver.isRunning());
	}

}
