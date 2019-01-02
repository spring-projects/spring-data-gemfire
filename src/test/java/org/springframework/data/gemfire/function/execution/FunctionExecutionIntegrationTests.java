/*
 * Copyright 2002-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.springframework.data.gemfire.function.execution;

import static org.assertj.core.api.Assertions.assertThat;

import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolManager;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.fork.FunctionCacheServerProcess;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;

/**
 * @author David Turanski
 * @author John Blum
 */
public class FunctionExecutionIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static int availablePort;

	private static ProcessWrapper gemfireServer;

	private ClientCache gemfireCache = null;

	private Pool gemfirePool = null;

	private Region<String, String> gemfireRegion = null;

	@BeforeClass
	public static void startGemFireServer() throws Exception {
		availablePort = findAvailablePort();

		gemfireServer = run(FunctionCacheServerProcess.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort));

		waitForServerToStart(DEFAULT_HOSTNAME, availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}

	@Before
	public void setupGemFireClient() {
		gemfireCache = new ClientCacheFactory()
			.set("name", "FunctionExecutionIntegrationTests")
			.set("log-level", "warning")
			.setPoolSubscriptionEnabled(true)
			.addPoolServer("localhost", availablePort)
			.create();

		gemfirePool = PoolManager.find("DEFAULT");

		gemfireRegion = gemfireCache.<String, String>createClientRegionFactory(ClientRegionShortcut.PROXY)
			.create("test-function");
	}

	@After
	public void tearDownGemFireClient() {
		if (gemfireCache != null) {
			try {
				gemfireCache.close();
			}
			catch (CacheClosedException ignore) {
			}
		}
	}

	@Test
	public void basicFunctionExecutionsAreCorrect() {
		verifyFunctionExecution(new PoolServerFunctionExecution(gemfirePool));
		verifyFunctionExecution(new RegionFunctionExecution(gemfireRegion));
		verifyFunctionExecution(new ServerFunctionExecution(gemfireCache));
		verifyFunctionExecution(new ServersFunctionExecution(gemfireCache));
	}

	private void verifyFunctionExecution(AbstractFunctionExecution functionExecution) {
		Iterable<String> results = functionExecution.setArgs("1", "2", "3").setFunctionId("echoFunction").execute();
		int count = 1;

		for (String result : results) {
			assertThat(result).isEqualTo(String.valueOf(count++));
		}
	}
}
