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

package org.springframework.data.gemfire.fork;

import java.io.File;

import org.apache.geode.distributed.ServerLauncher;
import org.apache.geode.distributed.internal.DistributionConfig;
import org.springframework.data.gemfire.process.support.ProcessUtils;
import org.springframework.data.gemfire.test.support.FileSystemUtils;

/**
 * The {@link GemFireBasedServerProcess} class is a main Java class used to launch a GemFire Server
 * using GemFire's {@link ServerLauncher} API.
 *
 * @author John Blum
 * @see org.apache.geode.distributed.ServerLauncher
 * @since 1.7.0
 */
public class GemFireBasedServerProcess {

	private static final String GEMFIRE_HTTP_SERVICE_PORT = "0";
	private static final String GEMFIRE_LOG_LEVEL = "warning";
	private static final String GEMIRE_NAME = "SpringDataGemFireServer";
	private static final String GEMFIRE_USE_CLUSTER_CONFIGURATION = "false";

	public static void main(String[] args) throws Throwable {
		runServer(args);

		registerShutdownHook();

		ProcessUtils.writePid(new File(FileSystemUtils.WORKING_DIRECTORY, getServerProcessControlFilename()),
			ProcessUtils.currentPid());

		ProcessUtils.waitForStopSignal();
	}

	private static ServerLauncher runServer(String[] args) {
		ServerLauncher serverLauncher = buildServerLauncher(args);

		// start the GemFire Server process...
		serverLauncher.start();

		return serverLauncher;
	}

	private static ServerLauncher buildServerLauncher(String[] args) {
		return new ServerLauncher.Builder(args)
			.setMemberName(getProperty("gemfire.name", GEMIRE_NAME))
			.setCommand(ServerLauncher.Command.START)
			.setDisableDefaultServer(true)
			.setRedirectOutput(false)
			.set(DistributionConfig.HTTP_SERVICE_PORT_NAME,
				getProperty("spring.data.gemfire.http-service-port", GEMFIRE_HTTP_SERVICE_PORT))
			.set(DistributionConfig.JMX_MANAGER_NAME, Boolean.TRUE.toString())
			.set(DistributionConfig.JMX_MANAGER_START_NAME, Boolean.FALSE.toString())
			.set(DistributionConfig.LOG_LEVEL_NAME,
				getProperty("spring.data.gemfire.log-level", GEMFIRE_LOG_LEVEL))
			.set(DistributionConfig.USE_CLUSTER_CONFIGURATION_NAME,
				getProperty("spring.data.gemfire.use-cluster-configuration", GEMFIRE_USE_CLUSTER_CONFIGURATION))
			.build();
	}

	private static String getProperty(String name, String defaultValue) {
		return System.getProperty(name, defaultValue);
	}

	private static void registerShutdownHook() {
		Runtime.getRuntime().addShutdownHook(new Thread(() -> {
			ServerLauncher serverLauncher = ServerLauncher.getInstance();

			if (serverLauncher != null) {
				serverLauncher.stop();
			}
		}));
	}

	public static String getServerProcessControlFilename() {
		return GemFireBasedServerProcess.class.getSimpleName().toLowerCase().concat(".pid");
	}
}
