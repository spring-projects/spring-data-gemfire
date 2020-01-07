/*
 * Copyright 2010-2020 the original author or authors.
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
package org.springframework.data.gemfire;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newRuntimeException;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.Scope;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.gemfire.fork.LocatorProcess;
import org.springframework.data.gemfire.process.ProcessInputStreamListener;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.data.gemfire.test.support.FileUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.data.gemfire.test.support.ThrowableUtils;
import org.springframework.data.gemfire.test.support.ZipUtils;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.util.FileSystemUtils;
import org.springframework.util.StringUtils;

/**
 * Test suite of test cases testing the integration of Spring Data for Apache Geode with Apache Geode's
 * new shared, persistent, cluster configuration service.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.support.ClassPathXmlApplicationContext
 * @see org.springframework.core.io.ClassPathResource
 * @see org.springframework.data.gemfire.fork.LocatorProcess
 * @see org.springframework.data.gemfire.process.ProcessExecutor
 * @see org.springframework.data.gemfire.process.ProcessWrapper
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public class CacheClusterConfigurationIntegrationTest extends ClientServerIntegrationTestsSupport {

	private static File locatorWorkingDirectory;

	private static List<String> locatorProcessOutput = Collections.synchronizedList(new ArrayList<>());

	private static ProcessWrapper locatorProcess;

	private static final String LOG_LEVEL = "config";
	private static final String LOG_FILE = "Locator.log";

	@Rule
	public TestRule watchman = new TestWatcher() {

		@Override
		protected void failed(Throwable throwable, Description description) {

			System.err.printf("Test [%s] failed...%n", description.getDisplayName());
			System.err.println(ThrowableUtils.toString(throwable));
			System.err.println("Locator process log file contents were...");
			System.err.println(getLocatorProcessOutput(description));
		}

		@Override
		protected void finished(Description description) {

			if (Boolean.valueOf(System.getProperty("spring.gemfire.fork.clean", Boolean.TRUE.toString()))) {
				try {
					FileUtils.write(new File(locatorWorkingDirectory.getParent(),
						String.format("%s-clusterconfiglocator.log", description.getMethodName())),
							getLocatorProcessOutput(description));
				}
				catch (IllegalArgumentException | IOException cause) {
					throw newRuntimeException(cause, "Failed to write the contents of the Locator process log to a file");
				}
			}
		}

		private String getLocatorProcessOutput(Description description) {

			try {

				String locatorProcessOutputString = StringUtils.collectionToDelimitedString(locatorProcessOutput,
					FileUtils.LINE_SEPARATOR, String.format("[%1$s] - ", description.getMethodName()), "");

				locatorProcessOutputString = StringUtils.hasText(locatorProcessOutputString)
					? locatorProcessOutputString
					: locatorProcess.readLogFile();

				return locatorProcessOutputString;
			}
			catch (IOException cause) {
				throw newRuntimeException(cause, "Failed to read the contents of the Locator process log file");
			}
		}
	};

	@BeforeClass
	@SuppressWarnings("all")
	public static void startLocator() throws IOException {

		int availablePort = findAvailablePort();

		String locatorName = "ClusterConfigLocator";

		locatorWorkingDirectory = new File(System.getProperty("java.io.tmpdir"), locatorName.toLowerCase());

		assertTrue(locatorWorkingDirectory.isDirectory() || locatorWorkingDirectory.mkdirs());

		ZipUtils.unzip(new ClassPathResource("/cluster_config.zip"), locatorWorkingDirectory);

		List<String> arguments = new ArrayList<String>();

		arguments.add("-Dgemfire.name=" + locatorName);
		arguments.add("-Dlog4j.geode.log.level=info");
		arguments.add("-Dlogback.log.level=info");
		arguments.add("-Dspring.data.gemfire.enable-cluster-configuration=true");
		arguments.add("-Dspring.data.gemfire.load-cluster-configuration=true");
		arguments.add(String.format("-Dgemfire.log-level=%s", LOG_LEVEL));
		arguments.add(String.format("-Dgemfire.log-file=%s", LOG_FILE));
		arguments.add(String.format("-Dspring.data.gemfire.locator.port=%d", availablePort));

		locatorProcess = run(locatorWorkingDirectory, LocatorProcess.class,
			arguments.toArray(new String[arguments.size()]));

		locatorProcess.register(new ProcessInputStreamListener() {
			@Override public void onInput(final String input) {
				locatorProcessOutput.add(input);
			}
		});

		locatorProcess.registerShutdownHook();

		waitForLocatorStart(TimeUnit.SECONDS.toMillis(30));

		System.setProperty("spring.data.gemfire.locator.port", String.valueOf(availablePort));
	}

	private static void waitForLocatorStart(final long milliseconds) {

		ThreadUtils.timedWait(milliseconds, 500, new ThreadUtils.WaitCondition() {

			File pidControlFile = new File(locatorWorkingDirectory, LocatorProcess.getLocatorProcessControlFilename());

			@Override
			public boolean waiting() {
				return !pidControlFile.isFile();
			}
		});
	}

	@AfterClass
	public static void stopLocator() {

		locatorProcess.shutdown();

		System.clearProperty("spring.data.gemfire.locator.port");

		if (Boolean.valueOf(System.getProperty("spring.gemfire.fork.clean", Boolean.TRUE.toString()))) {
			FileSystemUtils.deleteRecursively(locatorWorkingDirectory);
		}

		FilenameFilter logFileFilter = (directory, name) -> name.endsWith(".log");

		File[] logFiles = ArrayUtils.nullSafeArray(locatorWorkingDirectory.listFiles(logFileFilter), File.class);

		Arrays.stream(logFiles).forEach(File::delete);
	}

	private Region assertRegion(Region actualRegion, String expectedRegionName) {
		return assertRegion(actualRegion, expectedRegionName, Region.SEPARATOR+expectedRegionName);
	}

	private Region assertRegion(Region actualRegion, String expectedRegionName, String expectedRegionFullPath) {

		assertNotNull(String.format("The [%s] was not properly configured and initialized!",
			expectedRegionName), actualRegion);
		assertEquals(expectedRegionName, actualRegion.getName());
		assertEquals(expectedRegionFullPath, actualRegion.getFullPath());

		return actualRegion;
	}

	private Region assertRegionAttributes(Region actualRegion, DataPolicy expectedDataPolicy, Scope expectedScope) {

		assertNotNull(actualRegion);
		assertNotNull(actualRegion.getAttributes());
		assertEquals(expectedDataPolicy, actualRegion.getAttributes().getDataPolicy());
		assertEquals(expectedScope, actualRegion.getAttributes().getScope());

		return actualRegion;
	}

	private String getLocation(String configLocation) {

		String baseLocation = getClass().getPackage().getName().replace('.', File.separatorChar);

		return baseLocation.concat(File.separator).concat(configLocation);
	}

	private Region getRegion(ConfigurableApplicationContext applicationContext, String regionBeanName) {
		return applicationContext.getBean(regionBeanName, Region.class);
	}

	private ConfigurableApplicationContext newApplicationContext(String... configLocations) {

		ConfigurableApplicationContext applicationContext = new ClassPathXmlApplicationContext(configLocations);

		applicationContext.registerShutdownHook();

		return applicationContext;
	}

	@Test
	@Ignore
	public void clusterConfigurationTest() {

		ConfigurableApplicationContext applicationContext =
			newApplicationContext(getLocation("cacheUsingClusterConfigurationIntegrationTest.xml"));

		assertRegionAttributes(assertRegion(getRegion(applicationContext, "ClusterConfigRegion"), "ClusterConfigRegion"),
			DataPolicy.PARTITION, Scope.DISTRIBUTED_NO_ACK);

		assertRegionAttributes(assertRegion(getRegion(applicationContext, "NativeLocalRegion"), "NativeLocalRegion"),
			DataPolicy.NORMAL, Scope.LOCAL);

		assertRegionAttributes(assertRegion(getRegion(applicationContext, "NativePartitionRegion"), "NativePartitionRegion"),
			DataPolicy.PARTITION, Scope.DISTRIBUTED_NO_ACK);

		assertRegionAttributes(assertRegion(getRegion(applicationContext, "NativeReplicateRegion"), "NativeReplicateRegion"),
			DataPolicy.REPLICATE, Scope.DISTRIBUTED_ACK);

		assertRegionAttributes(assertRegion(getRegion(applicationContext, "LocalRegion"), "LocalRegion"),
			DataPolicy.NORMAL, Scope.LOCAL);
	}

	@Test
	@SuppressWarnings("all")
	public void localConfigurationTest() {

		try {

			newApplicationContext(getLocation("cacheUsingLocalConfigurationIntegrationTest.xml"));

			fail("Loading the 'cacheUsingLocalOnlyConfigurationIntegrationTest.xml' Spring ApplicationContext"
				+ " configuration file should have resulted in an Exception due to the Region lookup on"
				+ " 'ClusterConfigRegion' when Cluster Configuration is disabled!");
		}
		catch (BeanCreationException expected) {

			assertThat(expected).hasCauseInstanceOf(BeanInitializationException.class);

			assertTrue(String.format("Message was [%s]", expected.getMessage()), expected.getCause().getMessage()
				.matches("Region \\[ClusterConfigRegion\\] in Cache \\[.*\\] not found"));
		}
	}
}
