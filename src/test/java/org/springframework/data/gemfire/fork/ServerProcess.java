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

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.data.gemfire.process.support.ProcessUtils;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.util.Assert;

/**
 * The ServerProcess class is a main Java class using Spring Data GemFire to configure and bootstrap
 * a GemFire Cache Server process.
 *
 * @author John Blum
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.support.ClassPathXmlApplicationContext
 * @since 1.5.2
 */
public class ServerProcess {

	public static void main(final String[] args) throws Throwable {
		ConfigurableApplicationContext applicationContext = null;

		try {
			Assert.notEmpty(args, String.format("Usage: >java -cp ... %1$s %2$s", ServerProcess.class.getName(),
				"classpath:/to/applicationContext.xml"));

			applicationContext = new ClassPathXmlApplicationContext(args[0]);
			applicationContext.registerShutdownHook();

			ProcessUtils.writePid(new File(FileSystemUtils.WORKING_DIRECTORY, getServerProcessControlFilename()),
				ProcessUtils.currentPid());

			ProcessUtils.waitForStopSignal();
		}
		catch (Throwable e) {
			e.printStackTrace(System.err);
			throw e;
		}
		finally {
			if (applicationContext != null) {
				applicationContext.close();
			}
		}
	}

	public static String getServerProcessControlFilename() {
		return ServerProcess.class.getSimpleName().toLowerCase().concat(".pid");
	}

}
