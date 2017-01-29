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

package org.springframework.data.gemfire.client;

import static org.assertj.core.api.Assertions.assertThat;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.fork.ServerProcess;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration test testing {@link Region sub-Region} functionality from a GemFire cache client.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 1.4.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ClientSubRegionIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static ProcessWrapper gemfireServer;

	@Autowired
	private ClientCache clientCache;

	@Resource(name = "parentTemplate")
	private GemfireTemplate parentTemplate;

	@Resource(name = "childTemplate")
	private GemfireTemplate childTemplate;

	@Resource(name = "Parent")
	private Region parent;

	@Resource(name = "/Parent/Child")
	private Region child;

	@BeforeClass
	public static void startGemFireServer() throws Exception {
		int availablePort = findAvailablePort();

		gemfireServer = run(ServerProcess.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort),
			getServerContextXmlFileLocation(ClientSubRegionIntegrationTests.class));

		waitForServerToStart(DEFAULT_HOSTNAME, availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}

	protected void assertRegion(Region<?, ?> region, String name) {
		assertRegion(region, name, String.format("%1$s%2$s", Region.SEPARATOR, name));
	}

	protected void assertRegion(Region<?, ?> region, String name, String fullPath) {
		assertThat(region).isNotNull();
		assertThat(region.getName()).isEqualTo(name);
		assertThat(region.getFullPath()).isEqualTo(fullPath);
	}
	@Test
	public void gemFireSubRegionCreationConfigurationIsCorrect() {
		assertThat(clientCache).describedAs("The Client Cache was not properly initialized!").isNotNull();

		Region parent = clientCache.getRegion("Parent");

		assertRegion(parent, "Parent");

		Region child = parent.getSubregion("Child");

		assertRegion(child, "Child", "/Parent/Child");

		Region clientCacheChild = clientCache.getRegion("/Parent/Child");

		assertThat(child).isSameAs(clientCacheChild);
	}

	@Test
	public void springSubRegionCreationConfigurationIsCorrect() {
		assertRegion(parent, "Parent");
		assertRegion(child, "Child", "/Parent/Child");
	}

	@Test
	public void templateCreationConfigurationIsCorrect() {
		assertThat(parentTemplate).isNotNull();
		assertThat(parentTemplate.getRegion()).isSameAs(parent);
		assertThat(childTemplate).isNotNull();
		assertThat(childTemplate.getRegion()).isSameAs(child);
	}
}
