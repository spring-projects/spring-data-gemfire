/*
 * Copyright 2011 the original author or authors.
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

import org.springframework.data.gemfire.fork.CacheServerProcess;
import org.springframework.data.gemfire.test.support.IOUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.util.StringUtils;

/**
 * Utility for forking Java processes.
 * 
 * @author Costin Leau
 * @author John Blum
 * @deprecated ForkUtils has serious design flaws; please use ProcessExecutor instead
 */
@Deprecated
public class ForkUtil {

	private static OutputStream processStandardInStream;

	private static String JAVA_CLASSPATH = System.getProperty("java.class.path");
	private static String JAVA_HOME = System.getProperty("java.home");
	private static String JAVA_EXE = JAVA_HOME.concat(File.separator).concat("bin").concat(File.separator).concat("java");
	private static String TEMP_DIRECTORY = System.getProperty("java.io.tmpdir");

	private static OutputStream cloneJVM(final String arguments) {
		String[] args = arguments.split("\\s+");

		List<String> command = new ArrayList<String>(args.length + 3);

		command.add(JAVA_EXE);
		command.add("-classpath");
		command.add(JAVA_CLASSPATH);
		command.addAll(Arrays.asList(args));

		Process javaProcess;

		try {
			javaProcess = Runtime.getRuntime().exec(command.toArray(new String[command.size()]));
		}
		catch (IOException e) {
			System.out.println("[FORK-ERROR] " + e.getMessage());
			throw new IllegalStateException(String.format("Cannot start command %1$s",
				StringUtils.arrayToDelimitedString(command.toArray(), " ")), e);
		}

		System.out.println("Started fork from command: \n" + StringUtils.arrayToDelimitedString(command.toArray()," "));

		final AtomicBoolean runCondition = new AtomicBoolean(true);
		final Process p = javaProcess;

		startNewThread(newProcessStreamReader(runCondition, p.getErrorStream(), "[FORK-ERROR]"));
		startNewThread(newProcessStreamReader(runCondition, p.getInputStream(), "[FORK]"));

		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				System.out.println("Stopping fork...");
				runCondition.set(false);
				processStandardInStream = null;

				try {
					p.destroy();
					p.waitFor();
				}
				catch (InterruptedException ignore) {
				}

				System.out.println("Fork stopped");
			}
		});

		processStandardInStream = javaProcess.getOutputStream();

		return processStandardInStream;
	}

	protected static Thread startNewThread(final Runnable runnable) {
		Thread runnableThread = new Thread(runnable);
		runnableThread.start();
		return runnableThread;
	}

	protected static Runnable newProcessStreamReader(final AtomicBoolean runCondition, final InputStream processStream, final String label) {
		return new Runnable() {
			public void run() {
				BufferedReader processStreamReader = new BufferedReader(new InputStreamReader(processStream));
				try {
					while (runCondition.get()) {
						for (String line = "Reading..."; line != null; line = processStreamReader.readLine()) {
							System.out.printf("%1$s %2$s%n ", label, line);
						}
					}
				}
				catch (Exception ignore) {
				}
				finally {
					IOUtils.close(processStreamReader);
				}
			}
		};
	}

	public static OutputStream cacheServer() {
		return cacheServer(CacheServerProcess.class);
	}

	public static OutputStream cacheServer(Class<?> type) {
		return startCacheServer(type.getName());
	}

	public static OutputStream startCacheServer(String args) {
		return startGemFireProcess(args, "cache server");
	}

	protected static OutputStream startGemFireProcess(final String args, final String processName) {
		String className = args.split(" ")[0];

		System.out.println("main class: " + className);

		if (controlFileExists(className)) {
			deleteControlFile(className);
		}

		OutputStream outputStream = cloneJVM(args);

		final long timeout = System.currentTimeMillis() + 30000;

		while (!controlFileExists(className) && System.currentTimeMillis() < timeout) {
			ThreadUtils.sleep(500);
		}

		if (controlFileExists(className)) {
			System.out.printf("[FORK] Started %1$s%n", processName);
		}
		else {
			throw new RuntimeException(String.format("Failed to fork %1$s", processName));
		}

		return outputStream;
	}

	public static void sendSignal() {
		try {
			processStandardInStream.write("\n".getBytes());
			processStandardInStream.flush();
		}
		catch (IOException ex) {
			throw new IllegalStateException("Cannot communicate with forked VM", ex);
		}
	}

	public static boolean createControlFile(final String name) throws IOException {
		return new File(TEMP_DIRECTORY + File.separator + name).createNewFile();
	}

	public static boolean controlFileExists(final String name) {
		return new File(TEMP_DIRECTORY + File.separator + name).isFile();
	}

	public static boolean deleteControlFile(final String name) {
		return new File(TEMP_DIRECTORY + File.separator + name).delete();
	}

}
