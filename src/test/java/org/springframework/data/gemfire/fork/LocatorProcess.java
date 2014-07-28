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

package org.springframework.data.gemfire.fork;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

import org.springframework.data.gemfire.process.support.ProcessUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;

import com.gemstone.gemfire.distributed.LocatorLauncher;
import com.gemstone.gemfire.distributed.internal.DistributionConfig;
import com.gemstone.gemfire.distributed.internal.InternalLocator;
import com.gemstone.gemfire.distributed.internal.SharedConfiguration;

/**
 * The LocatorProcess class is a main Java class that is used fork and launch a GemFire Locator process using the
 * LocatorLauncher class.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.distributed.LocatorLauncher
 * @since 1.5.0
 */
public class LocatorProcess {

	public static final int DEFAULT_LOCATOR_PORT = 20668;

	public static final String DEFAULT_GEMFIRE_MEMBER_NAME = "SpringDataGemFire-Locator";
	public static final String DEFAULT_HOSTNAME_FOR_CLIENTS = "localhost";
	public static final String DEFAULT_HTTP_SERVICE_PORT = "0";
	public static final String DEFAULT_LOG_LEVEL = "config";

	public static void main(final String... args) throws IOException {
		LocatorLauncher locatorLauncher = buildLocatorLauncher();

		registerShutdownHook();

		// start the GemFire Locator process...
		locatorLauncher.start();

		waitForLocatorStart(TimeUnit.SECONDS.toMillis(20));

		ProcessUtils.writePid(new File(System.getProperty("user.dir"), getLocatorProcessControlFilename()),
			ProcessUtils.currentPid());

		ProcessUtils.waitForStopSignal();
	}

	public static String getLocatorProcessControlFilename() {
		return LocatorProcess.class.getSimpleName().toLowerCase().concat(".pid");
	}

	private static LocatorLauncher buildLocatorLauncher() {
		return new LocatorLauncher.Builder()
			.setMemberName(DEFAULT_GEMFIRE_MEMBER_NAME)
			.setHostnameForClients(System.getProperty("spring.gemfire.hostname-for-clients",
				DEFAULT_HOSTNAME_FOR_CLIENTS))
			.setPort(Integer.getInteger("spring.gemfire.locator-port", DEFAULT_LOCATOR_PORT))
			.setRedirectOutput(false)
			.set(DistributionConfig.ENABLE_SHARED_CONFIGURATION_NAME, String.valueOf(Boolean.getBoolean(
				"spring.gemfire.enable-cluster-configuration")))
			.set(DistributionConfig.HTTP_SERVICE_PORT_NAME, System.getProperty("spring.gemfire.http-service-port",
				DEFAULT_HTTP_SERVICE_PORT))
			.set(DistributionConfig.JMX_MANAGER_NAME, String.valueOf(Boolean.TRUE))
			.set(DistributionConfig.JMX_MANAGER_START_NAME, String.valueOf(Boolean.FALSE))
			.set(DistributionConfig.LOAD_SHARED_CONFIG_FROM_DIR_NAME, String.valueOf(Boolean.getBoolean(
				"spring.gemfire.load-cluster-configuration")))
			.set(DistributionConfig.LOG_LEVEL_NAME, System.getProperty("spring.gemfire.log-level", DEFAULT_LOG_LEVEL))
			.build();
	}

	private static boolean isClusterConfigurationEnabled(final InternalLocator locator) {
		return (locator != null && Boolean.valueOf(locator.getDistributedSystem().getProperties().getProperty(
			DistributionConfig.ENABLE_SHARED_CONFIGURATION_NAME)));
	}

	private static void registerShutdownHook() {
		Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
			@Override public void run() {
				stopSharedConfigurationService();
				LocatorLauncher.getInstance().stop();
			}

			private void stopSharedConfigurationService() {
				InternalLocator locator = InternalLocator.getLocator();

				if (isClusterConfigurationEnabled(locator)) {
					SharedConfiguration sharedConfiguration = locator.getSharedConfiguration();

					if (sharedConfiguration != null) {
						sharedConfiguration.destroySharedConfiguration();
					}
				}
			}
		}));
	}

	private static void waitForLocatorStart(final long milliseconds) {
		final InternalLocator locator = InternalLocator.getLocator();

		if (isClusterConfigurationEnabled(locator)) {
			ThreadUtils.timedWait(milliseconds, 500, new ThreadUtils.WaitCondition() {
				@Override public boolean waiting() {
					return !locator.isSharedConfigurationRunning();
				}
			});
		}
		else {
			LocatorLauncher.getInstance().waitOnStatusResponse(milliseconds, Math.min(500, milliseconds),
				TimeUnit.MILLISECONDS);
		}
	}

}
