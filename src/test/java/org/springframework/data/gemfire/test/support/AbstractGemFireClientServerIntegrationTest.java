/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.test.support;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.springframework.data.gemfire.fork.ServerProcess;
import org.springframework.data.gemfire.process.ProcessExecutor;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.util.Assert;

/**
 * The {@link AbstractGemFireClientServerIntegrationTest} class is an abstract test suite base class
 * encapsulating functionality common to all test classes implementing client/server test cases.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.fork.ServerProcess
 * @see org.springframework.data.gemfire.process.ProcessExecutor
 * @see org.springframework.data.gemfire.process.ProcessWrapper
 * @since 1.8.0
 */
@Deprecated
@SuppressWarnings("unused")
public abstract class AbstractGemFireClientServerIntegrationTest {

	protected static final long DEFAULT_TIME_TO_WAIT_FOR_SERVER_TO_START = TimeUnit.SECONDS.toMillis(20);
	protected static final long FIVE_HUNDRED_MILLISECONDS = TimeUnit.MILLISECONDS.toMillis(500);
	protected static final long ONE_SECOND_IN_MILLISECONDS = TimeUnit.SECONDS.toMillis(1);

	protected static String CLEAN_PROCESS_WORKING_DIRECTORY_SYSTEM_PROPERTY = "spring.data.gemfire.force.clean";

	protected static void pause(final long duration) {
		ThreadUtils.timedWait(Math.max(duration, ONE_SECOND_IN_MILLISECONDS), ONE_SECOND_IN_MILLISECONDS, () -> true);
	}

	protected static <T> T setSystemProperty(String propertyName, T propertyValue) {

		System.setProperty(propertyName, String.valueOf(propertyValue));

		return propertyValue;
	}

	protected static ProcessWrapper startGemFireServer(Class<?> testClass) throws IOException {
		return startGemFireServer(testClass, DEFAULT_TIME_TO_WAIT_FOR_SERVER_TO_START);
	}

	protected static ProcessWrapper startGemFireServer(Class<?> testClass, long waitTimeInMilliseconds)
			throws IOException {

		Assert.notNull(testClass, "testClass must not be null");

		String serverName = testClass.getSimpleName() + "Server";

		File serverWorkingDirectory = new File(FileSystemUtils.WORKING_DIRECTORY, serverName.toLowerCase());

		Assert.isTrue(serverWorkingDirectory.isDirectory() || serverWorkingDirectory.mkdirs());

		List<String> arguments = new ArrayList<>();

		addTestClassSystemProperties(testClass, arguments)
			.add(String.format("-Dgemfire.name=%1$s", serverName));

		arguments.add("/".concat(testClass.getName().replace(".", "/").concat("-server-context.xml")));

		ProcessWrapper gemfireServerProcess =
			ProcessExecutor.launch(serverWorkingDirectory, ServerProcess.class, arguments.toArray(new String[0]));

		waitForServerToStart(gemfireServerProcess, waitTimeInMilliseconds);

		return gemfireServerProcess;
	}

	static List<String> addTestClassSystemProperties(Class<?> testClass, List<String> arguments) {

		String testClassName = testClass.getName();

		System.getProperties().stringPropertyNames().stream()
			.filter(propertyName -> propertyName.startsWith(testClassName))
			.forEach(propertyName -> {

				String argument = String.format("-D%1$s=%2$s", propertyName, System.getProperty(propertyName));

				arguments.add(argument);
			});

		return arguments;
	}

	static void waitForServerToStart(final ProcessWrapper process, long duration) {

		ThreadUtils.timedWait(Math.max(duration, FIVE_HUNDRED_MILLISECONDS), FIVE_HUNDRED_MILLISECONDS,

			new ThreadUtils.WaitCondition() {

				private File processPidFile =
					new File(process.getWorkingDirectory(), ServerProcess.getServerProcessControlFilename());

				@Override
				public boolean waiting() {
					return !processPidFile.isFile();
				}
			}
		);
	}

	protected static ProcessWrapper stopGemFireServer(ProcessWrapper process) {

		try {

			process.shutdown();

			boolean springGemFireForceClean =
				Boolean.valueOf(System.getProperty(CLEAN_PROCESS_WORKING_DIRECTORY_SYSTEM_PROPERTY,
					String.valueOf(true)));

			if (springGemFireForceClean) {
				org.springframework.util.FileSystemUtils.deleteRecursively(process.getWorkingDirectory());
			}

			return null;
		}
		catch (Exception ignore) {
			return process;
		}
	}

	protected static void clearTestClassSystemProperties(Class<?> testClass) {

		String testClassName = testClass.getName();

		System.getProperties().stringPropertyNames().stream()
			.filter(propertyName -> propertyName.startsWith(testClassName))
			.forEach(propertyName -> System.clearProperty(propertyName));
	}
}
