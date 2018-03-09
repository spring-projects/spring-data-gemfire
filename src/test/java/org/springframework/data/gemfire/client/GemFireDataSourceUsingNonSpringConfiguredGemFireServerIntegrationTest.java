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
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.distributed.ServerLauncher;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.gemfire.fork.GemFireBasedServerProcess;
import org.springframework.data.gemfire.process.ProcessExecutor;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.Assert;
import org.springframework.util.FileCopyUtils;
import org.springframework.util.StringUtils;

/**
 * The GemFireDataSourceUsingNonSpringConfiguredGemFireServerIntegrationTest class is a test suite of test cases
 * testing the contract and functionality of the GemfireDataSourcePostProcessor using the &lt;gfe-data:datasource&gt;
 * element in Spring config to setup a GemFire ClientCache connecting to a native, non-Spring configured GemFire Server
 * as the DataSource to assert that client Region Proxies are registered as Spring beans
 * in the Spring ApplicationContext correctly.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor
 * @since 1.7.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings({ "rawtypes", "unused"})
public class GemFireDataSourceUsingNonSpringConfiguredGemFireServerIntegrationTest {

	private static ProcessWrapper serverProcess;

	@Autowired
	private ApplicationContext applicationContext;

	@Autowired
	private ClientCache gemfireClientCache;

	@Resource(name = "LocalRegion")
	private Region localRegion;

	@Resource(name = "ServerRegion")
	private Region serverRegion;

	@Resource(name = "AnotherServerRegion")
	private Region anotherServerRegion;

	@BeforeClass
	public static void setupBeforeClass() throws IOException {

		System.setProperty("gemfire.log-level", "error");

		String serverName = "GemFireDataSourceGemFireBasedServer";

		File serverWorkingDirectory = new File(FileSystemUtils.WORKING_DIRECTORY, serverName.toLowerCase());

		Assert.isTrue(serverWorkingDirectory.isDirectory() || serverWorkingDirectory.mkdirs());

		writeAsCacheXmlFileToDirectory("gemfire-datasource-integration-test-cache.xml", serverWorkingDirectory);

		Assert.isTrue(new File(serverWorkingDirectory, "cache.xml").isFile(), String.format(
			"Expected a cache.xml file to exist in directory (%1$s)!", serverWorkingDirectory));

		List<String> arguments = new ArrayList<String>(5);

		arguments.add(ServerLauncher.Command.START.getName());
		arguments.add(String.format("-Dgemfire.name=%1$s", serverName));
		arguments.add(String.format("-Dgemfire.mcast-port=%1$s", "0"));
		arguments.add(String.format("-Dgemfire.log-level=%1$s", "warning"));
		//arguments.add(String.format("-Dgemfire.cache-xml-file=%1$s", "gemfire-datasource-integration-test-cache.xml"));

		serverProcess = ProcessExecutor.launch(serverWorkingDirectory, customClasspath(), ServerLauncher.class,
			arguments.toArray(new String[arguments.size()]));

		waitForProcessStart(TimeUnit.SECONDS.toMillis(20), serverProcess, GemFireBasedServerProcess.getServerProcessControlFilename());

		System.out.println("GemFire-based Cache Server Process for ClientCache DataSource Test should be running and connected...");
	}

	private static void writeAsCacheXmlFileToDirectory(String classpathResource, File serverWorkingDirectory) throws IOException {
		FileCopyUtils.copy(new ClassPathResource(classpathResource).getInputStream(),
			new FileOutputStream(new File(serverWorkingDirectory, "cache.xml")));
	}

	private static String customClasspath() {

		String[] classpathElements = ProcessExecutor.JAVA_CLASSPATH.split(File.pathSeparator);

		List<String> customClasspath = new ArrayList<String>(classpathElements.length);

		for (String classpathElement : classpathElements) {
			if (!classpathElement.contains("spring-data-gemfire")) {
				customClasspath.add(classpathElement);
			}
		}

		return StringUtils.collectionToDelimitedString(customClasspath, File.pathSeparator);
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

		assertRegion(localRegion, "LocalRegion");
		assertRegion(serverRegion, "ServerRegion");
		assertRegion(anotherServerRegion, "AnotherServerRegion");
	}
}
