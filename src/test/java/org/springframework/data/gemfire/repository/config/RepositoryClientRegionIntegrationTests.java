/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.repository.config;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.fork.ServerProcess;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @author David Turanski
 * @author John Blum
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class RepositoryClientRegionIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static ProcessWrapper gemfireServer;

	@Autowired
	private PersonRepository repository;

	@BeforeClass
	public static void startGemFireServer() throws Exception {

		System.setProperty("gemfire.log-level", GEMFIRE_LOG_LEVEL);

		int availablePort = findAvailablePort();

		gemfireServer = run(ServerProcess.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort),
			getServerContextXmlFileLocation(RepositoryClientRegionIntegrationTests.class));

		waitForServerToStart(DEFAULT_HOSTNAME, availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty("gemfire.log-level");
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}

	@Test
	public void findAllAndCountMatch() {

		assertThat(this.repository.count()).isEqualTo(2);
		assertThat(this.repository.findAll()).hasSize(2);
	}
}
