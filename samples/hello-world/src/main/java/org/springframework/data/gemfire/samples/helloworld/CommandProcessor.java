/*
 * Copyright 2010 the original author or authors.
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

package org.springframework.data.gemfire.samples.helloworld;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Scanner;

import javax.annotation.Resource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.gemfire.GemfireCallback;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.stereotype.Component;

import com.gemstone.gemfire.GemFireCheckedException;
import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Region;

/**
 * @author Costin Leau
 */
@Component
public class CommandProcessor {

	private static final Log log = LogFactory.getLog(CommandProcessor.class);

	private static String help = initHelp();
	private static String EMPTY = "";

	boolean threadActive;
	private Thread thread;

	@Resource
	private GemfireTemplate template;

	void start() {
		if (thread == null) {
			threadActive = true;
			thread = new Thread(new Task(), "cmd-processor");
			thread.start();
		}
	}

	void stop() throws Exception {
		threadActive = false;
		thread.join(3 * 100);
	}

	void awaitCommands() throws Exception {
		thread.join();
	}

	private class Task implements Runnable {

		public void run() {
			System.out.println("Hello World!");
			System.out.println("Want to interact with the world ? ...");
			System.out.println(help);
			System.out.print("-> ");
			System.out.flush();
			try {
				BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

				while (threadActive) {
					if (br.ready()) {
						System.out.println(process(br.readLine()));
						System.out.print("-> ");
						System.out.flush();
					}
				}
			} catch (IOException ioe) {
				// just ignore any exceptions
				log.error("Caught exception while processing commands ", ioe);
			}
		}
	}

	private static String initHelp() {
		try {
			InputStream stream = CommandProcessor.class.getResourceAsStream("help.txt");
			byte[] buffer = new byte[stream.available() > 0 ? stream.available() : 300];

			BufferedInputStream bf = new BufferedInputStream(stream);
			bf.read(buffer);
			return new String(buffer);
		} catch (IOException io) {
			throw new IllegalStateException("Cannot read help file");
		}
	}

	String process(String line) {
		final Scanner sc = new Scanner(line);

		return template.execute(new GemfireCallback<String>() {
			public String doInGemfire(Region reg) throws GemFireCheckedException, GemFireException {
				Region<String, String> region = reg;

				if (!sc.hasNext()) {
					return "Invalid command";
				}
				String command = sc.next();

				// parse commands w/o arguments
				if ("exit".equalsIgnoreCase(command)) {
					threadActive = false;
					return EMPTY;
				}
				if ("help".equalsIgnoreCase(command)) {
					return help;
				}
				if ("size".equalsIgnoreCase(command)) {
					return EMPTY + region.size();
				}
				if ("clear".equalsIgnoreCase(command)) {
					region.clear();
					return EMPTY;
				}
				if ("keys".equalsIgnoreCase(command)) {
					return region.keySet().toString();
				}
				if ("values".equalsIgnoreCase(command)) {
					return region.values().toString();
				}

				if (!sc.hasNext()) {
					return "Argument(s) needed";
				}

				String arg = sc.next();

				// commands w/ 1 arg
				if ("containsKey".equalsIgnoreCase(command)) {
					return EMPTY + region.containsKey(arg);
				}
				if ("containsValue".equalsIgnoreCase(command)) {
					return EMPTY + region.containsValue(arg);
				}
				if ("query".equalsIgnoreCase(command)) {
					return region.query(command).toString();
				}
				if ("get".equalsIgnoreCase(command)) {
					return region.get(arg);
				}
				if ("remove".equalsIgnoreCase(command)) {
					return region.remove(arg);
				}

				// commands w/ 2 args

				if (!sc.hasNext()) {
					return "2 Argument needed";
				}

				String arg2 = sc.next();

				if ("put".equalsIgnoreCase(command)) {
					return region.put(arg, arg2);
				}

				sc.close();
				return "unknown command - run 'help' for available commands";
			}
		});
	}
}