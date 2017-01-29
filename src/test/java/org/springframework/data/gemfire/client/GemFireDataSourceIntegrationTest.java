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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.fork.ServerProcess;
import org.springframework.data.gemfire.process.ProcessExecutor;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;

/**
 * The GemFireDataSourceIntegrationTest class is a test suite of test cases testing the contract and functionality
 * of the &lt;gfe-data:datasource&gt; element in the context of a GemFire cluster running both native,
 * non-Spring configured GemFire Server(s) in addition to Spring configured and bootstrapped GemFire Server(s).
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @since 1.7.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings({ "rawtypes", "unused"})
public class GemFireDataSourceIntegrationTest {

	private static ProcessWrapper serverProcess;

	@Autowired
	private ApplicationContext applicationContext;

	@Autowired
	private ClientCache gemfireClientCache;

	@Resource(name = "ClientOnlyRegion")
	private Region clientOnlyRegion;

	@Resource(name = "ClientServerRegion")
	private Region clientServerRegion;

	@Resource(name = "ServerOnlyRegion")
	private Region serverOnlyRegion;

	@BeforeClass
	public static void setupBeforeClass() throws IOException {
		System.setProperty("gemfire.log-level", "warning");

		String serverName = "GemFireDataSourceSpringBasedServer";

		File serverWorkingDirectory = new File(FileSystemUtils.WORKING_DIRECTORY, serverName.toLowerCase());

		Assert.isTrue(serverWorkingDirectory.isDirectory() || serverWorkingDirectory.mkdirs());

		List<String> arguments = new ArrayList<String>();

		arguments.add(String.format("-Dgemfire.name=%1$s", serverName));
		arguments.add(GemFireDataSourceIntegrationTest.class.getName().replace(".", "/").concat("-server-context.xml"));

		serverProcess = ProcessExecutor.launch(serverWorkingDirectory, ServerProcess.class,
			arguments.toArray(new String[arguments.size()]));

		waitForProcessStart(TimeUnit.SECONDS.toMillis(20), serverProcess, ServerProcess.getServerProcessControlFilename());

		System.out.println("Spring configured/bootstrapped GemFire Cache Server Process for ClientCache DataSource Test should be running...");
	}

	private static void waitForProcessStart(final long milliseconds, final ProcessWrapper process, final String processControlFilename) {
		ThreadUtils.timedWait(milliseconds, TimeUnit.MILLISECONDS.toMillis(500), new ThreadUtils.WaitCondition() {
			private File processControlFile = new File(process.getWorkingDirectory(), processControlFilename);

			@Override public boolean waiting() {
				return !processControlFile.isFile();
			}
		});
	}

	@AfterClass
	public static void tearDown() {
		serverProcess.shutdown();

		if (Boolean.valueOf(System.getProperty("spring.gemfire.fork.clean", Boolean.TRUE.toString()))) {
			org.springframework.util.FileSystemUtils.deleteRecursively(serverProcess.getWorkingDirectory());
		}
	}

	protected void assertRegion(Region actualRegion, String expectedRegionName) {
		assertThat(actualRegion, is(not(nullValue())));
		assertThat(actualRegion.getName(), is(equalTo(expectedRegionName)));
		assertThat(actualRegion.getFullPath(), is(equalTo(String.format("%1$s%2$s",
			Region.SEPARATOR, expectedRegionName))));
		assertThat(gemfireClientCache.getRegion(actualRegion.getFullPath()), is(sameInstance(actualRegion)));
		assertThat(applicationContext.containsBean(expectedRegionName), is(true));
		assertThat(applicationContext.getBean(expectedRegionName, Region.class), is(sameInstance(actualRegion)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void clientProxyRegionBeansExist() {
		assertRegion(clientOnlyRegion, "ClientOnlyRegion");
		assertRegion(clientServerRegion, "ClientServerRegion");
		assertRegion(serverOnlyRegion, "ServerOnlyRegion");
	}

}
