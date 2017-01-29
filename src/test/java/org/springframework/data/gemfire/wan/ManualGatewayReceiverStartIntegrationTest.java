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

import java.io.IOException;

import javax.annotation.Resource;

import org.apache.geode.cache.wan.GatewayReceiver;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The ManualGatewayReceiverStartIntegrationTest class is a test suite of test cases testing the manual start capability
 * of Gateway Receivers when configured with the Spring Data GemFire XML namespace.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class ManualGatewayReceiverStartIntegrationTest {

	@Resource(name = "Auto")
	private GatewayReceiver autoGatewayReceiver;

	@Resource(name = "Manual")
	private GatewayReceiver manualGatewayReceiver;

	protected void assertGreaterThanEqualToLessThanEqualTo(final String message,
			final int actualValue, final int lowerBound, final int upperBound) {
		assertTrue(message, actualValue >= lowerBound && actualValue <= upperBound);
	}

	@Test
	public void testAutoGatewayReceiver() {
		assertNotNull("The 'Auto' GatewayReceiver was not properly configured or initialized!", autoGatewayReceiver);
		assertTrue(autoGatewayReceiver.isRunning());
		assertEquals(7070, autoGatewayReceiver.getStartPort());
		assertEquals(7700, autoGatewayReceiver.getEndPort());

		final int gatewayReceiverPort = autoGatewayReceiver.getPort();

		assertGreaterThanEqualToLessThanEqualTo(String.format(
			"GatewayReceiver 'port' (%1$d) was not greater than equal to (%2$d) and less than equal to (%3$d)!",
				gatewayReceiverPort, autoGatewayReceiver.getStartPort(), autoGatewayReceiver.getEndPort()),
					gatewayReceiverPort, autoGatewayReceiver.getStartPort(), autoGatewayReceiver.getEndPort());

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

		final int gatewayReceiverPort = manualGatewayReceiver.getPort();

		assertGreaterThanEqualToLessThanEqualTo(String.format(
			"GatewayReceiver 'port' (%1$d) was not greater than equal to (%2$d) and less than equal to (%3$d)!",
				gatewayReceiverPort, manualGatewayReceiver.getStartPort(), manualGatewayReceiver.getEndPort()),
					gatewayReceiverPort, manualGatewayReceiver.getStartPort(), manualGatewayReceiver.getEndPort());

		manualGatewayReceiver.stop();

		assertFalse(manualGatewayReceiver.isRunning());
	}

}
