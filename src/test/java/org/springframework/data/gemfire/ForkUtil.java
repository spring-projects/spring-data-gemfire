/*
 * Copyright 2011-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.gemfire.fork.CqCacheServerProcess;
import org.springframework.data.gemfire.fork.FunctionCacheServerProcess;
import org.springframework.data.gemfire.test.support.IOUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.util.StringUtils;

/**
 * Utility class used to fork Java, JVM processes.
 *
 * @author Costin Leau
 * @author John Blum
 * @deprecated ForkUtils has serious design flaws; please use ProcessExecutor instead
 */
@Deprecated
public class ForkUtil {

	private static final long TIMEOUT = 30000L;

	@SuppressWarnings("all")
	private static final Logger logger = LoggerFactory.getLogger(ForkUtil.class);

	private static OutputStream processStandardInStream;

	private static String JAVA_CLASSPATH = System.getProperty("java.class.path");
	private static String JAVA_HOME = System.getProperty("java.home");
	private static String JAVA_EXE = JAVA_HOME.concat(File.separator).concat("bin").concat(File.separator).concat("java");
	private static String TEMP_DIRECTORY = System.getProperty("java.io.tmpdir");

	private static List<String> buildJavaCommand(String arguments) {
		return buildJavaCommand(arguments.split("\\s+"));
	}

	private static List<String> buildJavaCommand(String[] args) {
		List<String> command = new ArrayList<>(args.length + 5);

		command.add(JAVA_EXE);
		command.add("-server");
		command.add("-ea");
		command.add("-classpath");
		command.add(JAVA_CLASSPATH);
		command.addAll(Arrays.asList(args));

		return command;
	}

	private static OutputStream cloneJVM(String arguments) {
		AtomicBoolean runCondition = new AtomicBoolean(true);

		List<String> command = buildJavaCommand(arguments);

		Process javaProcess;

		try {
			javaProcess = Runtime.getRuntime().exec(command.toArray(new String[command.size()]));

			logger.debug("Started fork from command: {}",
				StringUtils.arrayToDelimitedString(command.toArray()," "));

			captureProcessStreams(runCondition, javaProcess);
			registerShutdownHook(runCondition, javaProcess);

			processStandardInStream = javaProcess.getOutputStream();

			return processStandardInStream;
		}
		catch (IOException e) {
			throw new RuntimeException(String.format("Failed to fork JVM process using command [%s]",
				StringUtils.arrayToDelimitedString(command.toArray(), " ")), e);
		}
	}

	private static void captureProcessStreams(AtomicBoolean runCondition, Process javaProcess) {
		startNewThread(newProcessStreamReader(runCondition, javaProcess.getErrorStream(), "[FORK-ERR]"));
		startNewThread(newProcessStreamReader(runCondition, javaProcess.getInputStream(), "[FORK-OUT]"));
	}

	private static Thread startNewThread(Runnable runnable) {
		Thread runnableThread = new Thread(runnable);
		runnableThread.setDaemon(true);
		runnableThread.setPriority(Thread.NORM_PRIORITY);
		runnableThread.start();
		return runnableThread;
	}

	private static Runnable newProcessStreamReader(AtomicBoolean runCondition, InputStream processStream, String label) {
		return () -> {
			BufferedReader processStreamReader = null;

			try {
				processStreamReader = new BufferedReader(new InputStreamReader(processStream));

				for (String line = "Reading..."; runCondition.get() && line != null;
					 line = processStreamReader.readLine()) {

					logger.debug("{} {}", label, line);
				}
			}
			catch (Exception ignore) {
			}
			finally {
				IOUtils.close(processStreamReader);
			}
		};
	}

	private static void registerShutdownHook(AtomicBoolean runCondition, Process javaProcess) {
		Runtime.getRuntime().addShutdownHook(new Thread(() -> {
			runCondition.set(false);
			processStandardInStream = null;

			try {
				javaProcess.destroyForcibly();
				javaProcess.waitFor();
			}
			catch (InterruptedException ignore) {
				Thread.currentThread().interrupt();
			}
		}));
	}

	public static OutputStream cacheServer(Class<?> type) {
		return startCacheServer(type.getName());
	}

	public static OutputStream cqCacheServer() {
		return cacheServer(CqCacheServerProcess.class);
	}

	public static OutputStream functionCacheServer() {
		return cacheServer(FunctionCacheServerProcess.class);
	}

	public static OutputStream startCacheServer(String args) {
		return startGemFireProcess(args, "cache server");
	}

	protected static OutputStream startGemFireProcess(String args, String processName) {
		String className = args.split(" ")[0];

		if (controlFileExists(className)) {
			deleteControlFile(className);
		}

		OutputStream outputStream = cloneJVM(args);

		long timeout = (System.currentTimeMillis() + TIMEOUT);

		while (!controlFileExists(className) && System.currentTimeMillis() < timeout) {
			ThreadUtils.sleep(500L);
		}

		if (!controlFileExists(className)) {
			throw new RuntimeException(String.format("Failed to fork %s", processName));
		}

		return outputStream;
	}

	public static void sendSignal() {
		try {
			processStandardInStream.write("\n".getBytes());
			processStandardInStream.flush();
		}
		catch (IOException e) {
			logger.info("Cannot communicate with forked VM", e);
		}
	}

	public static boolean createControlFile( String name) throws IOException {
		return new File(TEMP_DIRECTORY + File.separator + name).createNewFile();
	}

	public static boolean controlFileExists(String name) {
		return new File(TEMP_DIRECTORY + File.separator + name).isFile();
	}

	public static boolean deleteControlFile(String name) {
		return new File(TEMP_DIRECTORY + File.separator + name).delete();
	}
}
