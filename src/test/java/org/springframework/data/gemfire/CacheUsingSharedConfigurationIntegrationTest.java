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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.annotation.Resource;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.gemfire.fork.LocatorProcess;
import org.springframework.data.gemfire.process.ProcessExecutor;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.data.gemfire.test.support.ZipUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.FileSystemUtils;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.Scope;

/**
 * The CacheUsingSharedConfigurationIntegrationTest class is a test suite of test cases testing the integration of
 * Spring Data GemFire with GemFire 8's new shared, persistent, cluster configuration service.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("cacheUsingSharedConfigurationIntegrationTest.xml")
@SuppressWarnings("unused")
public class CacheUsingSharedConfigurationIntegrationTest {

	@Resource(name = "SharedConfigRegion")
	private Region<Long, String> sharedConfigRegion;

	@Resource(name = "NativeLocalRegion")
	private Region<Integer, String> nativeLocalRegion;

	@Resource(name = "NativePartitionRegion")
	private Region<Integer, String> nativePartitionRegion;

	@Resource(name = "NativeReplicateRegion")
	private Region<Integer, String> nativeReplicateRegion;

	@Resource(name = "LocalRegion")
	private Region<Integer, String> localRegion;

	private static File locatorWorkingDirectory;

	private static ProcessWrapper locatorProcess;

	@BeforeClass
	public static void testSuiteSetup() throws IOException {
		String locatorName = "SharedConfigLocator";

		locatorWorkingDirectory = new File(System.getProperty("user.dir"), locatorName.toLowerCase());

		assertTrue(locatorWorkingDirectory.isDirectory() || locatorWorkingDirectory.mkdirs());

		ZipUtils.unzip(new ClassPathResource("/shared_config.zip"), locatorWorkingDirectory);

		List<String> arguments = new ArrayList<String>();

		arguments.add("-Dgemfire.name=" + locatorName);
		arguments.add("-Dspring.gemfire.enable-shared-configuration=true");
		arguments.add("-Dspring.gemfire.load-shared-configuration=true");

		locatorProcess = ProcessExecutor.launch(locatorWorkingDirectory, LocatorProcess.class,
			arguments.toArray(new String[arguments.size()]));

		locatorProcess.registerShutdownHook();

		waitForLocatorStart(TimeUnit.SECONDS.toMillis(30));

		//System.out.println("Shared Configuration Locator should be running!");
	}

	private static void waitForLocatorStart(final long milliseconds) {
		ThreadUtils.timedWait(milliseconds, 500, new ThreadUtils.WaitCondition() {
			File pidControlFile = new File(locatorWorkingDirectory, LocatorProcess.getLocatorProcessControlFilename());
			@Override public boolean waiting() {
				return !pidControlFile.isFile();
			}
		});
	}

	@AfterClass
	public static void testSuiteTearDown() {
		locatorProcess.shutdown();
		FileSystemUtils.deleteRecursively(locatorWorkingDirectory);
	}

	protected Region assertRegion(final Region actualRegion, final String expectedRegionName) {
		return assertRegion(actualRegion, expectedRegionName, Region.SEPARATOR+expectedRegionName);
	}

	protected Region assertRegion(final Region actualRegion, final String expectedRegionName, final String expectedRegionFullPath) {
		assertNotNull(String.format("The '%1$s' was not properly configured and initialized!", expectedRegionName), actualRegion);
		assertEquals(expectedRegionName, actualRegion.getName());
		assertEquals(expectedRegionFullPath, actualRegion.getFullPath());
		return actualRegion;
	}

	protected Region assertRegionAttributes(final Region actualRegion, final DataPolicy expectedDataPolicy, final Scope expectedScope) {
		assertNotNull(actualRegion);
		assertNotNull(actualRegion.getAttributes());
		assertEquals(expectedDataPolicy, actualRegion.getAttributes().getDataPolicy());
		assertEquals(expectedScope, actualRegion.getAttributes().getScope());
		return actualRegion;
	}

	@Test
	public void testConfiguration() {
		assertRegionAttributes(assertRegion(sharedConfigRegion, "SharedConfigRegion"),
			DataPolicy.PARTITION, Scope.DISTRIBUTED_NO_ACK);

		assertRegionAttributes(assertRegion(nativeLocalRegion, "NativeLocalRegion"), DataPolicy.NORMAL, Scope.LOCAL);

		assertRegionAttributes(assertRegion(nativePartitionRegion, "NativePartitionRegion"),
			DataPolicy.PARTITION, Scope.DISTRIBUTED_NO_ACK);

		assertRegionAttributes(assertRegion(nativeReplicateRegion, "NativeReplicateRegion"),
			DataPolicy.REPLICATE, Scope.DISTRIBUTED_ACK);

		assertRegionAttributes(assertRegion(localRegion, "LocalRegion"), DataPolicy.NORMAL, Scope.LOCAL);
	}

}
