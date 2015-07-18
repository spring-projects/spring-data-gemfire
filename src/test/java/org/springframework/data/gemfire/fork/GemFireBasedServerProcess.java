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

import org.springframework.data.gemfire.process.support.ProcessUtils;
import org.springframework.data.gemfire.test.support.FileSystemUtils;

import com.gemstone.gemfire.distributed.ServerLauncher;
import com.gemstone.gemfire.distributed.internal.DistributionConfig;

/**
 * The GemFireBasedServerProcess class is a main Java class used to launch a GemFire Server
 * using GemFire's ServerLauncher API.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.distributed.ServerLauncher
 * @since 1.7.0
 */
public class GemFireBasedServerProcess {

	protected static final String DEFAULT_GEMFIRE_MEMBER_NAME = "SpringDataGemFire-Server";
	protected static final String DEFAULT_HTTP_SERVICE_PORT = "0";
	protected static final String DEFAULT_LOG_LEVEL = "warning";
	protected static final String DEFAULT_USE_CLUSTER_CONFIGURATION = "false";

	public static void main(final String[] args) throws Throwable {
		runServer(args);

		registerShutdownHook();

		ProcessUtils.writePid(new File(FileSystemUtils.WORKING_DIRECTORY, getServerProcessControlFilename()),
			ProcessUtils.currentPid());

		ProcessUtils.waitForStopSignal();
	}

	public static String getServerProcessControlFilename() {
		return GemFireBasedServerProcess.class.getSimpleName().toLowerCase().concat(".pid");
	}

	private static ServerLauncher runServer(final String[] args) {
		ServerLauncher serverLauncher = buildServerLauncher(args);

		// start the GemFire Server process...
		serverLauncher.start();

		return serverLauncher;
	}

	private static ServerLauncher buildServerLauncher(final String[] args) {
		return new ServerLauncher.Builder(args)
			.setMemberName(System.getProperty("gemfire.name", DEFAULT_GEMFIRE_MEMBER_NAME))
			.setCommand(ServerLauncher.Command.START)
			.setDisableDefaultServer(true)
			.setRedirectOutput(false)
			.set(DistributionConfig.HTTP_SERVICE_PORT_NAME, System.getProperty("spring.gemfire.http-service-port",
				DEFAULT_HTTP_SERVICE_PORT))
			.set(DistributionConfig.JMX_MANAGER_NAME, String.valueOf(Boolean.TRUE))
			.set(DistributionConfig.JMX_MANAGER_START_NAME, String.valueOf(Boolean.FALSE))
			.set(DistributionConfig.LOG_LEVEL_NAME, System.getProperty("spring.gemfire.log-level", DEFAULT_LOG_LEVEL))
			.set(DistributionConfig.USE_CLUSTER_CONFIGURATION_NAME, System.getProperty("spring.gemfire.use-cluster-configuration",
				DEFAULT_USE_CLUSTER_CONFIGURATION))
			.build();
	}

	private static void registerShutdownHook() {
		Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
			@Override public void run() {
				ServerLauncher.getInstance().stop();
			}
		}));
	}

}
