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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * @author Costin Leau
 */
public class CommandProcessor {

	private static final Log log = LogFactory.getLog(CommandProcessor.class);

	boolean threadActive;
	private Thread thread;

	void start() {
		if (thread == null) {
			threadActive = false;
			thread = new Thread(new Task(), "cmd-processor");
			thread.start();
		}
	}

	void stop() throws Exception {
		threadActive = true;
		thread.join(3 * 1000);
	}

	private class Task implements Runnable {

		public void run() {
			try {

				BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

				while (threadActive) {
					if (br.ready()) {
						String line = br.readLine();
						log.info("Read line " + line);
					}
				}
			} catch (IOException ioe) {
				// just ignore any exceptions
				log.error("Caught exception while processing commands ", ioe);
			}
		}
	}
}
