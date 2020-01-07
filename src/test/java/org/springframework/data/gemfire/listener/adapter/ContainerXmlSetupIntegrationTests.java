/*
 * Copyright 2011-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.listener.adapter;

import static org.assertj.core.api.Assertions.assertThat;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.query.CqQuery;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.fork.CqCacheServerProcess;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @author Costin Leau
 * @author John Blum
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
// TODO change this test to use mocks!!
public class ContainerXmlSetupIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static ProcessWrapper gemfireServer;

	@BeforeClass
	public static void startGemFireServer() throws Exception {
		int availablePort = findAvailablePort();

		gemfireServer = run(CqCacheServerProcess.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort));

		waitForServerToStart(DEFAULT_HOSTNAME, availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}


	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void containerSetup() throws Exception {
		ContinuousQueryListenerContainer container =
			applicationContext.getBean(ContinuousQueryListenerContainer.class);

		assertThat(container).isNotNull();
		assertThat(container.isRunning()).isTrue();
		assertThat(container).isSameAs(applicationContext.getBean("testContainerId",
			ContinuousQueryListenerContainer.class));

		ClientCache cache = applicationContext.getBean(ClientCache.class);
		Pool pool = applicationContext.getBean(Pool.class);

		assertThat(cache.getName()).isEqualTo("ContainerXmlSetupIntegrationTests");
		assertThat(pool.getName()).isEqualTo("client");

		CqQuery[] cacheCqs = cache.getQueryService().getCqs();
		CqQuery[] poolCqs = pool.getQueryService().getCqs();

		assertThat(pool.getQueryService().getCq("test-bean-1")).isNotNull();
		assertThat(cacheCqs.length).isEqualTo(3);
		assertThat(poolCqs.length).isEqualTo(3);
	}
}
