/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.springframework.data.gemfire.fork;

import java.io.IOException;
import java.util.Scanner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.data.gemfire.ForkUtil;

/**
 * {@link SpringContainerProcess} launches a Spring {@link ConfigurableApplicationContext} in this JVM process.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.support.ClassPathXmlApplicationContext
 */
public class SpringContainerProcess {

	private static final Logger logger = LoggerFactory.getLogger(SpringContainerProcess.class);

	public static void main(String[] args) {
		ConfigurableApplicationContext applicationContext = null;

		try {
			applicationContext = newApplicationContext(args);
			waitForShutdown(applicationContext);
		}
		catch (Exception e) {
			logger.debug("", e);
			System.exit(1);
		}
		finally {
			close(applicationContext);
		}
	}

	private static ConfigurableApplicationContext newApplicationContext(String[] configLocations) {
		ConfigurableApplicationContext applicationContext = new ClassPathXmlApplicationContext(configLocations);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	private static void close(ConfigurableApplicationContext applicationContext) {
		if (applicationContext != null) {
			applicationContext.close();
		}
	}

	@SuppressWarnings({ "deprecation", "unused" })
	private static void waitForShutdown(ConfigurableApplicationContext applicationContext) throws IOException {
		ForkUtil.createControlFile(SpringContainerProcess.class.getName());
		Scanner scanner = new Scanner(System.in);
		scanner.nextLine();
	}
}
