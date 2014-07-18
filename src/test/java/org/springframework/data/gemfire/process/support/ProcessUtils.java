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

package org.springframework.data.gemfire.process.support;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.Scanner;
import java.util.logging.Logger;

import org.springframework.data.gemfire.test.support.IOUtils;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.management.internal.cli.util.spring.Assert;
import com.sun.tools.attach.VirtualMachine;
import com.sun.tools.attach.VirtualMachineDescriptor;

/**
 * The ProcessUtils class is a utilty class for working with process, or specifically instances
 * of the Java Process class.
 *
 * @author John Blum
 * @see java.lang.Process
 * @see java.lang.management.RuntimeMXBean
 * @see com.sun.tools.attach.VirtualMachine
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public abstract class ProcessUtils {

	protected static final Logger log = Logger.getLogger(ProcessUtils.class.getName());

	protected static final String TERM_TOKEN = "<TERM/>";

	public static int currentPid() {
		RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
		String runtimeMXBeanName = runtimeMXBean.getName();

		Exception cause = null;

		if (StringUtils.hasText(runtimeMXBeanName)) {
			int atSignIndex = runtimeMXBeanName.indexOf('@');

			if (atSignIndex > 0) {
				try {
					return Integer.parseInt(runtimeMXBeanName.substring(0, atSignIndex));
				}
				catch (NumberFormatException e) {
					cause = e;
				}
			}
		}

		throw new PidUnavailableException(String.format("The process ID (PID) is not available (%1$s)!",
			runtimeMXBeanName), cause);
	}

	public static boolean isRunning(final int processId) {
		for (VirtualMachineDescriptor vmDescriptor : VirtualMachine.list()) {
			if (String.valueOf(processId).equals(vmDescriptor.id())) {
				return true;
			}
		}

		return false;
	}

	public static boolean isRunning(final Process process) {
		try {
			process.exitValue();
			return false;
		}
		catch (IllegalThreadStateException ignore) {
			return true;
		}
	}

	public static void signalStop(final Process process) throws IOException {
		if (isRunning(process)) {
			OutputStream processOutputStream = process.getOutputStream();
			processOutputStream.write(TERM_TOKEN.concat("\n").getBytes());
			processOutputStream.flush();
		}
	}

	public static void waitForStopSignal() {
		Scanner in = new Scanner(System.in);
		while (!TERM_TOKEN.equals(in.next()));
	}

	public static int findAndReadPid(final File workingDirectory) {
		Assert.isTrue(workingDirectory != null && workingDirectory.isDirectory(), String.format(
			"The file system pathname (%1$s) expected to contain a PID file is not a valid directory!",
				workingDirectory));

		File pidFile = findPidFile(workingDirectory);

		if (pidFile == null) {
			throw new PidUnavailableException(String.format(
				"No PID file was found in working directory (%1$s) or any of it's sub-directories!",
					workingDirectory));
		}

		return readPid(pidFile);
	}

	protected static File findPidFile(final File workingDirectory) {
		Assert.isTrue(workingDirectory != null && workingDirectory.isDirectory(), String.format(
			"The file system pathname (%1$s) is not valid directory!", workingDirectory));

		for (File file : workingDirectory.listFiles(DirectoryPidFileFilter.INSTANCE)) {
			if (file.isDirectory()) {
				file = findPidFile(file);
			}

			if (PidFileFilter.INSTANCE.accept(file)) {
				return file;
			}
		}

		return null;
	}

	public static int readPid(final File pidFile) {
		Assert.isTrue(pidFile != null && pidFile.isFile(), String.format(
			"The file system pathname (%1$s) is not a valid file!", pidFile));

		BufferedReader fileReader = null;
		String pidValue = null;

		try {
			fileReader = new BufferedReader(new FileReader(pidFile));
			pidValue = String.valueOf(fileReader.readLine()).trim();
			return Integer.parseInt(pidValue);
		}
		catch (FileNotFoundException e) {
			throw new PidUnavailableException(String.format("PID file (%1$s) could not be found!", pidFile), e);
		}
		catch (IOException e) {
			throw new PidUnavailableException(String.format("Unable to read PID from file (%1$s)!", pidFile), e);
		}
		catch (NumberFormatException e) {
			throw new PidUnavailableException(String.format(
				"The value (%1$s) from PID file (%2$s) was not a valid numerical PID!", pidValue, pidFile), e);
		}
		finally {
			IOUtils.close(fileReader);
		}
	}

	public static void writePid(final File pidFile, final int pid) throws IOException {
		Assert.isTrue(pidFile != null && (pidFile.isFile() || pidFile.createNewFile()), String.format(
			"The file system pathname (%1$s) in which the PID will be written is not a valid file!", pidFile));

		Assert.isTrue(pid > 0, String.format("The PID value (%1$d) must greater than 0!", pid));

		PrintWriter fileWriter = new PrintWriter(new BufferedWriter(new FileWriter(pidFile, false), 16), true);

		try {
			fileWriter.println(pid);
		}
		finally {
			pidFile.deleteOnExit();
			IOUtils.close(fileWriter);
		}
	}

	protected static class DirectoryPidFileFilter extends PidFileFilter {

		protected static final DirectoryPidFileFilter INSTANCE = new DirectoryPidFileFilter();

		@Override
		public boolean accept(final File pathname) {
			return (pathname != null && (pathname.isDirectory() || super.accept(pathname)));
		}
	}

	protected static class PidFileFilter implements FileFilter {

		protected static final PidFileFilter INSTANCE = new PidFileFilter();

		@Override
		public boolean accept(final File pathname) {
			return (pathname != null && pathname.isFile() && pathname.getName().endsWith(".pid"));
		}
	}

}
