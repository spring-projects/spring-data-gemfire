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

import org.springframework.util.StringUtils;

/**
 * Utility for forking Java processes.
 * 
 * @author Costin Leau
 * @author John Blum
 */
public class ForkUtil {

	private static OutputStream processStandardInStream;

	private static String JAVA_CLASSPATH = System.getProperty("java.class.path");
	private static String JAVA_HOME = System.getProperty("java.home");
	private static String JAVA_EXE = JAVA_HOME.concat(File.separator).concat("bin").concat(File.separator).concat("java");
	private static String TEMP_DIRECTORY = System.getProperty("java.io.tmpdir");

	private static OutputStream cloneJVM(final String arguments) {
		String[] args = arguments.split("\\s+");

		List<String> command = new ArrayList<String>(args.length + 5);

		command.add(JAVA_EXE);
		command.add("-classpath");
		command.add(JAVA_CLASSPATH);
		//command.add("-Xms256m");
		//command.add("-Xmx512m");
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

		new Thread(newProcessStreamReaderRunnable(runCondition, p.getErrorStream(), "[FORK-ERROR]")).start();
		new Thread(newProcessStreamReaderRunnable(runCondition, p.getInputStream(), "[FORK]")).start();

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

	protected static Runnable newProcessStreamReaderRunnable(final AtomicBoolean runCondition, final InputStream processStream, final String label) {
		final BufferedReader processStreamReader = new BufferedReader(new InputStreamReader(processStream));

		return new Runnable() {
			public void run() {
				try {
					do {
						for (String line = "Reading..."; line != null; line = processStreamReader.readLine()) {
							System.out.printf("%1$s %2$s%n ", label, line);
						}

						Thread.sleep(200);
					}
					while (runCondition.get());
				}
				catch (Exception ignore) {
				}
			}
		};
	}

	public static OutputStream cacheServer(Class<?> clazz) {
		return startCacheServer(clazz.getName());
	}
	
	public static OutputStream cacheServer() {
		return startCacheServer("org.springframework.data.gemfire.fork.CacheServerProcess");
	}
	
	public static OutputStream startCacheServer(String args) {
		String className = args.split(" ")[0];
		
		System.out.println("main class:" + className);
		
		if (controlFileExists(className)) {
			deleteControlFile(className);
		}
		OutputStream os = cloneJVM(args);
		int maxTime = 30000;
		int time = 0;
		while (!controlFileExists(className) && time < maxTime) {
			try {
				Thread.sleep(500);
				time += 500;
			} catch (InterruptedException ex) {
				// ignore and move on
			}
		}
		if (controlFileExists(className)) {
			System.out.println("[FORK] Started cache server");
		}
		else {
			throw new RuntimeException("could not fork cache server");
		}
		return os;
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

	public static boolean deleteControlFile(String name) {
		String path = TEMP_DIRECTORY + File.separator + name;
		return new File(path).delete();
	}

	public static boolean createControlFile(String name) throws IOException {
		String path = TEMP_DIRECTORY + File.separator + name;
		return new File(path).createNewFile();
	}

	public static boolean controlFileExists(String name) {
		String path = TEMP_DIRECTORY + File.separator + name;
		return new File(path).exists();
	}

}
