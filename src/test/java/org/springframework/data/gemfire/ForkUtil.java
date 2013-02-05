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
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.concurrent.atomic.AtomicBoolean;

import org.springframework.util.StringUtils;


/**
 * Utility for forking Java processes.
 * 
 * @author Costin Leau
 */
public class ForkUtil {
	private static OutputStream os;
	private static String TEMP_DIR = System.getProperty("java.io.tmpdir");
	
	

	private static OutputStream cloneJVM(String arguments) {
		String cp = System.getProperty("java.class.path");
		String home = System.getProperty("java.home");

		Process proc = null;
		String sp = System.getProperty("file.separator");
		String java = home + sp + "bin" + sp + "java";
		
		String[] args = arguments.split("\\s+");
		String[] cmd = new String[args.length + 3];
		cmd[0]=java;
		cmd[1]="-cp";
		cmd[2]=cp;
		for (int i=3; i< cmd.length; i++) {
			cmd[i] = args[i-3];
		}
		
		try {
 			proc = Runtime.getRuntime().exec(cmd);
		} catch (IOException ioe) {
			System.out.println("[FORK-ERROR] " + ioe.getMessage());
			throw new IllegalStateException("Cannot start command " + StringUtils.arrayToDelimitedString(cmd," "), ioe);
		}

		System.out.println("Started fork from command\n" + StringUtils.arrayToDelimitedString(cmd," "));
		final Process p = proc;

		final BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
		final BufferedReader er = new BufferedReader(new InputStreamReader(p.getErrorStream()));
		final AtomicBoolean run = new AtomicBoolean(true);

	new Thread(new Runnable() {
			public void run() {
				try {
					String line = null;
					do {
						while ((line = er.readLine()) != null) {
							System.out.println("[FORK-ERROR] " + line);
						}
						Thread.sleep(200);
					} while (run.get());
				} catch (Exception ex) {
					// ignore and exit
				}
			}
		}).start();
		 
		
		 new Thread(new Runnable() {

			public void run() {
				try {
					String line = null;
					do {
						while ((line = br.readLine()) != null) {
							System.out.println("[FORK] " + line);
						}
						Thread.sleep(200);
					} while (run.get());
				} catch (Exception ex) {
					// ignore and exit
				}
			}
		}).start();

		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				System.out.println("Stopping fork...");
				run.set(false);
				os = null;
				if (p != null)
					p.destroy();

				try {
					p.waitFor();
				} catch (InterruptedException e) {
					// ignore
				}
				System.out.println("Fork stopped");
			}
		});

		os = proc.getOutputStream();
		return os;
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
			os.write("\n".getBytes());
			os.flush();
		} catch (IOException ex) {
			throw new IllegalStateException("Cannot communicate with forked VM", ex);
		}
	}

	public static boolean deleteControlFile(String name) {
		String path = TEMP_DIR + File.separator + name;
		return new File(path).delete();
	}

	public static boolean createControlFile(String name) throws IOException {
		String path = TEMP_DIR + File.separator + name;
		return new File(path).createNewFile();
	}

	public static boolean controlFileExists(String name) {
		String path = TEMP_DIR + File.separator + name;
		return new File(path).exists();
	}
}