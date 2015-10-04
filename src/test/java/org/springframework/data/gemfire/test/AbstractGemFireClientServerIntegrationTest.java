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

package org.springframework.data.gemfire.test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.springframework.data.gemfire.fork.ServerProcess;
import org.springframework.data.gemfire.process.ProcessExecutor;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.util.Assert;

/**
 * The AbstractGemFireClientServerIntegrationTest class is an abstract test suite base class encapsulating functionality
 * common to all test classes implementing GemFire client/server test cases.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.fork.ServerProcess
 * @see org.springframework.data.gemfire.process.ProcessExecutor
 * @see org.springframework.data.gemfire.process.ProcessWrapper
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public abstract class AbstractGemFireClientServerIntegrationTest {

	protected static long DEFAULT_WAIT_TIME_FOR_SERVER_TO_START = TimeUnit.SECONDS.toMillis(20);
	protected static long FIVE_HUNDRED_MILLISECONDS = TimeUnit.MILLISECONDS.toMillis(500);
	protected static long ONE_SECOND_IN_MILLISECONDS = TimeUnit.SECONDS.toMillis(1);

	protected static String PROCESS_WORKING_DIRECTORY_CLEAN_SYSTEM_PROPERTY = "spring.gemfire.force.clean";

	protected static void pause(final long duration) {
		ThreadUtils.timedWait(Math.max(duration, ONE_SECOND_IN_MILLISECONDS), ONE_SECOND_IN_MILLISECONDS,
			new ThreadUtils.WaitCondition() {
				@Override public boolean waiting() {
					return true;
				}
			}
		);
	}

	protected static ProcessWrapper setupGemFireServer(final Class<?> testClass) throws IOException {
		return setupGemFireServer(testClass, DEFAULT_WAIT_TIME_FOR_SERVER_TO_START);
	}

	protected static ProcessWrapper setupGemFireServer(final Class<?> testClass, final long waitTimeInMilliseconds) throws IOException {
		String serverName = testClass.getSimpleName() + "Server";

		File serverWorkingDirectory = new File(FileSystemUtils.WORKING_DIRECTORY, serverName.toLowerCase());

		Assert.isTrue(serverWorkingDirectory.isDirectory() || serverWorkingDirectory.mkdirs());

		List<String> arguments = new ArrayList<String>();

		arguments.add(String.format("-Dgemfire.name=%1$s", serverName));
		arguments.add("/".concat(testClass.getName().replace(".", "/").concat("-server-context.xml")));

		ProcessWrapper serverProcess = ProcessExecutor.launch(serverWorkingDirectory, ServerProcess.class,
			arguments.toArray(new String[arguments.size()]));

		waitForServerToStart(serverProcess, waitTimeInMilliseconds);

		System.out.printf("The Spring-based, GemFire Cache Server process for %1$s should be running...%n",
			testClass.getSimpleName());

		return serverProcess;
	}

	static void waitForServerToStart(final ProcessWrapper process, final long duration) {
		ThreadUtils.timedWait(Math.max(duration, FIVE_HUNDRED_MILLISECONDS), FIVE_HUNDRED_MILLISECONDS,
			new ThreadUtils.WaitCondition() {
				private File processPidControlFile = new File(process.getWorkingDirectory(),
					ServerProcess.getServerProcessControlFilename());

				@Override public boolean waiting() {
					return !processPidControlFile.isFile();
				}
			}
		);
	}

	protected static void tearDownGemFireServer(final ProcessWrapper process) {
		process.shutdown();

		if (Boolean.valueOf(System.getProperty(PROCESS_WORKING_DIRECTORY_CLEAN_SYSTEM_PROPERTY, Boolean.TRUE.toString()))) {
			org.springframework.util.FileSystemUtils.deleteRecursively(process.getWorkingDirectory());
		}
	}

}
