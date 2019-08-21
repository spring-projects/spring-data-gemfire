/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.process;

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The {@link ProcessExecutor} class is a utility class for launching and running Java processes.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.lang.Process
 * @see java.lang.ProcessBuilder
 * @see java.lang.System
 * @see org.springframework.data.gemfire.process.ProcessConfiguration
 * @see org.springframework.data.gemfire.process.ProcessWrapper
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public abstract class ProcessExecutor {

	public static final File JAVA_EXE = new File(new File(FileSystemUtils.JAVA_HOME, "bin"), "java");

	public static final String JAVA_CLASSPATH = System.getProperty("java.class.path");

	protected static final String SPRING_GEMFIRE_SYSTEM_PROPERTY_PREFIX = "spring.gemfire.";
	protected static final String SPRING_DATA_GEMFIRE_SYSTEM_PROPERTY_PREFIX = "spring.data.gemfire.";

	public static ProcessWrapper launch(Class<?> type, String... args) throws IOException {
		return launch(FileSystemUtils.WORKING_DIRECTORY, type, args);
	}

	public static ProcessWrapper launch(File workingDirectory, Class<?> type, String... args) throws IOException {
		return launch(workingDirectory, JAVA_CLASSPATH, type, args);
	}

	public static ProcessWrapper launch(File workingDirectory, String classpath, Class<?> type, String... args)
			throws IOException {

		ProcessBuilder processBuilder = new ProcessBuilder()
			.command(buildCommand(classpath, type, args))
			.directory(validateDirectory(workingDirectory))
			.redirectErrorStream(true);

		Process process = processBuilder.start();

		ProcessWrapper processWrapper = new ProcessWrapper(process, ProcessConfiguration.create(processBuilder));

		//processWrapper.register((input) -> System.err.printf("[FORK] - %s%n", input));

		return processWrapper;
	}

	protected static String[] buildCommand(String classpath, Class<?> type, String... args) {

		Assert.notNull(type, "The main Java class to launch must not be null");

		List<String> command = new ArrayList<>();
		List<String> programArguments = new ArrayList<>(args.length);

		command.add(JAVA_EXE.getAbsolutePath());
		command.add("-server");
		command.add("-ea");
		command.add("-classpath");
		command.add(StringUtils.hasText(classpath) ? classpath : JAVA_CLASSPATH);
		command.addAll(getSpringGemFireSystemProperties());

		for (String arg : nullSafeArray(args, String.class)) {
			if (isJvmOption(arg)) {
				command.add(arg);
			}
			else if (!StringUtils.isEmpty(arg)) {
				programArguments.add(arg);
			}
		}

		command.add(type.getName());
		command.addAll(programArguments);

		return command.toArray(new String[0]);
	}

	protected static Collection<? extends String> getSpringGemFireSystemProperties() {

		return System.getProperties().stringPropertyNames().stream()
			.filter(property -> property.startsWith(SPRING_DATA_GEMFIRE_SYSTEM_PROPERTY_PREFIX)
				|| property.startsWith(SPRING_GEMFIRE_SYSTEM_PROPERTY_PREFIX))
			.map(property -> String.format("-D%1$s=%2$s", property, System.getProperty(property)))
			.collect(Collectors.toList());
	}

	protected static boolean isJvmOption(String option) {
		return StringUtils.hasText(option) && (option.startsWith("-D") || option.startsWith("-X"));
	}

	protected static File validateDirectory(File workingDirectory) {

		Assert.isTrue(workingDirectory != null && (workingDirectory.isDirectory() || workingDirectory.mkdirs()),
			String.format("Failed to create working directory [%s]", workingDirectory));

		return workingDirectory;
	}
}
