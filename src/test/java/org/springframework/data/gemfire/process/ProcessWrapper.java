/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.data.gemfire.process.support.PidUnavailableException;
import org.springframework.data.gemfire.process.support.ProcessUtils;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.data.gemfire.test.support.FileUtils;
import org.springframework.data.gemfire.test.support.IOUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.data.gemfire.test.support.ThrowableUtils;
import org.springframework.util.Assert;

/**
 * The ProcessWrapper class is a wrapper for a Process object representing an OS process and the ProcessBuilder used
 * to construct and start the process.
 *
 * @author John Blum
 * @see java.lang.Process
 * @see java.lang.ProcessBuilder
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public class ProcessWrapper {

	protected static final boolean DEFAULT_DAEMON_THREAD = true;

	protected static final long DEFAULT_WAIT_TIME_MILLISECONDS = TimeUnit.SECONDS.toMillis(15);

	private final List<ProcessInputStreamListener> listeners = new CopyOnWriteArrayList<>();

	protected final Logger log = Logger.getLogger(getClass().getName());

	private final Process process;
	private final ProcessConfiguration processConfiguration;

	/* (non-Javadoc) */
	public ProcessWrapper(Process process, ProcessConfiguration processConfiguration) {
		Assert.notNull(process, "Process must not be null");

		Assert.notNull(processConfiguration, "The context and configuration meta-data providing details"
			+ " about the environment in which the process is running and how the process was configured and executed"
			+ " must not be null");

		this.process = process;
		this.processConfiguration = processConfiguration;

		init();
	}

	/* (non-Javadoc) */
	private void init() {
		newThread("Process OUT Stream Reader Thread",
			newProcessInputStreamReaderRunnable(process.getInputStream())).start();

		if (!isRedirectingErrorStream()) {
			newThread("Process ERR Stream Reader Thread",
				newProcessInputStreamReaderRunnable(process.getErrorStream())).start();
		}
	}

	/* (non-Javadoc) */
	protected Runnable newProcessInputStreamReaderRunnable(InputStream in) {
		return () -> {
			if (isRunning()) {
				BufferedReader inputReader = new BufferedReader(new InputStreamReader(in));

				try {
					for (String input = inputReader.readLine(); input != null; input = inputReader.readLine()) {
						for (ProcessInputStreamListener listener : listeners) {
							listener.onInput(input);
						}
					}
				}
				catch (IOException ignore) {
					// Ignore IO error and just stop reading from the process input stream
					// An IO error occurred most likely because the process was terminated
				}
				finally {
					IOUtils.close(inputReader);
				}
			}
		};
	}

	/* (non-Javadoc) */
	protected Thread newThread(String name, Runnable task) {
		Assert.hasText(name, "Thread name must be specified");
		Assert.notNull(task, "Thread task must not be null");

		Thread thread = new Thread(task, name);

		thread.setDaemon(DEFAULT_DAEMON_THREAD);
		thread.setPriority(Thread.NORM_PRIORITY);

		return thread;
	}

	/* (non-Javadoc) */
	public boolean isAlive() {
		return ProcessUtils.isAlive(process);
	}

	/* (non-Javadoc) */
	public boolean isNotAlive() {
		return !isAlive();
	}

	/* (non-Javadoc) */
	public List<String> getCommand() {
		return processConfiguration.getCommand();
	}

	/* (non-Javadoc) */
	public String getCommandString() {
		return processConfiguration.getCommandString();
	}

	/* (non-Javadoc) */
	public Map<String, String> getEnvironment() {
		return processConfiguration.getEnvironment();
	}

	/* (non-Javadoc) */
	public int getPid() {
		return ProcessUtils.findAndReadPid(getWorkingDirectory());
	}

	/* (non-Javadoc) */
	public int safeGetPid() {
		try {
			return getPid();
		}
		catch (PidUnavailableException ignore) {
			return -1;
		}
	}

	/* (non-Javadoc) */
	public boolean isRedirectingErrorStream() {
		return processConfiguration.isRedirectingErrorStream();
	}

	/* (non-Javadoc) */
	public boolean isNotRunning() {
		return !isRunning();
	}

	/* (non-Javadoc) */
	public boolean isRunning() {
		return ProcessUtils.isRunning(process);
	}

	/* (non-Javadoc) */
	public File getWorkingDirectory() {
		return processConfiguration.getWorkingDirectory();
	}

	/* (non-Javadoc) */
	public int exitValue() {
		return process.exitValue();
	}

	/* (non-Javadoc) */
	public int safeExitValue() {
		try {
			return exitValue();
		}
		catch (IllegalThreadStateException ignore) {
			return -1;
		}
	}

	/* (non-Javadoc) */
	public String readLogFile() throws IOException {
		File[] logFiles = FileSystemUtils.listFiles(getWorkingDirectory(),
			(path) -> (path != null && (path.isDirectory() || path.getAbsolutePath().endsWith(".log"))));

		if (logFiles.length > 0) {
			return readLogFile(logFiles[0]);
		}
		else {
			throw new FileNotFoundException(String.format(
				"No log files found in process's [%d] working directory [%s]",
					safeGetPid(), getWorkingDirectory()));
		}
	}

	/* (non-Javadoc) */
	public String readLogFile(File log) throws IOException {
		return FileUtils.read(log);
	}

	/* (non-Javadoc) */
	public boolean register(ProcessInputStreamListener listener) {
		return (listener != null && listeners.add(listener));
	}

	/* (non-Javadoc) */
	public void registerShutdownHook() {
		Runtime.getRuntime().addShutdownHook(new Thread(this::shutdown));
	}

	/* (non-Javadoc) */
	public void signal() {
		try {
			OutputStream outputStream = process.getOutputStream();
			outputStream.write("\n".getBytes());
			outputStream.flush();
		}
		catch (IOException e) {
			log.warning("Failed to signal process");

			if (log.isLoggable(Level.FINE)) {
				log.fine(ThrowableUtils.toString(e));
			}
		}
	}

	/* (non-Javadoc) */
	public void signalStop() {
		try {
			ProcessUtils.signalStop(process);
		}
		catch (IOException e) {
			log.warning("Failed to signal the process to stop");

			if (log.isLoggable(Level.FINE)) {
				log.fine(ThrowableUtils.toString(e));
			}
		}
	}

	/* (non-Javadoc) */
	public int stop() {
		return stop(DEFAULT_WAIT_TIME_MILLISECONDS);
	}

	/* (non-Javadoc) */
	public int stop(long milliseconds) {
		if (isRunning()) {
			boolean interrupted = false;
			int exitValue = -1;
			int pid = safeGetPid();
			long timeout = (System.currentTimeMillis() + milliseconds);
			AtomicBoolean exited = new AtomicBoolean(false);

			ExecutorService executorService = Executors.newSingleThreadExecutor();

			try {
				Future<Integer> futureExitValue = executorService.submit(() -> {
					process.destroy();
					int localExitValue = process.waitFor();
					exited.set(true);
					return localExitValue;
				});

				while (!exited.get() && System.currentTimeMillis() < timeout) {
					try {
						exitValue = futureExitValue.get(milliseconds, TimeUnit.MILLISECONDS);
						log.info(String.format("Process [%s] has stopped%n", pid));
					}
					catch (InterruptedException ignore) {
						interrupted = true;
					}
				}
			}
			catch (TimeoutException e) {
				exitValue = -1;
				log.warning(String.format("Process [%1$d] did not stop within the allotted timeout of %2$d seconds%n",
					pid, TimeUnit.MILLISECONDS.toSeconds(milliseconds)));
			}
			catch (Exception ignore) {
				// handles CancellationException, ExecutionException
			}
			finally {
				executorService.shutdownNow();

				if (interrupted) {
					Thread.currentThread().interrupt();
				}
			}

			return exitValue;
		}
		else {
			return exitValue();
		}
	}

	/* (non-Javadoc) */
	public int shutdown() {
		if (isRunning()) {
			log.info(String.format("Stopping process [%d]...%n", safeGetPid()));
			signalStop();
			waitFor();
		}

		return stop();
	}

	/* (non-Javadoc) */
	public boolean unregister(ProcessInputStreamListener listener) {
		return listeners.remove(listener);
	}

	/* (non-Javadoc) */
	public void waitFor() {
		waitFor(DEFAULT_WAIT_TIME_MILLISECONDS);
	}

	/* (non-Javadoc) */
	public void waitFor(long milliseconds) {
		ThreadUtils.timedWait(milliseconds, 500, this::isRunning);
	}
}
