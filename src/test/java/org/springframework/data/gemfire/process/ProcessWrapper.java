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

package org.springframework.data.gemfire.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
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
import org.springframework.data.gemfire.test.support.IOUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.data.gemfire.test.support.ThrowableUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

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

	private final List<ProcessInputStreamListener> listeners = new CopyOnWriteArrayList<ProcessInputStreamListener>();

	protected final Logger log = Logger.getLogger(getClass().getName());

	private final Process process;
	private final ProcessConfiguration processConfiguration;

	public ProcessWrapper(final Process process, final ProcessConfiguration processConfiguration) {
		Assert.notNull(process, "The Process object backing this wrapper must not be null!");

		Assert.notNull(processConfiguration, "The context and configuration meta-data providing details about"
			+ " the environment in which the process is running and how the process was configured must not be null!");

		this.process = process;
		this.processConfiguration = processConfiguration;

		postInit();
	}

	private void postInit() {
		newThread("Process OUT Stream Reader", newProcessInputStreamReader(process.getInputStream())).start();

		if (!isRedirectingErrorStream()) {
			newThread("Process ERR Stream Reader", newProcessInputStreamReader(process.getErrorStream())).start();
		}
	}

	protected Runnable newProcessInputStreamReader(final InputStream in) {
		return new Runnable() {
			@Override public void run() {
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
						// ignore IO error and just stop reading from the process input stream
						// IO error occurred most likely because the process was terminated
					}
					finally {
						IOUtils.close(inputReader);
					}
				}
			}
		};
	}

	protected Thread newThread(final String name, final Runnable task) {
		Assert.isTrue(!StringUtils.isEmpty(name), "The name of the Thread must be specified!");
		Assert.notNull(task, "The Thread task must not be null!");
		Thread thread = new Thread(task, name);
		thread.setDaemon(DEFAULT_DAEMON_THREAD);
		thread.setPriority(Thread.NORM_PRIORITY);
		return thread;
	}

	public List<String> getCommand() {
		return processConfiguration.getCommand();
	}

	public String getCommandString() {
		return processConfiguration.getCommandString();
	}

	public Map<String, String> getEnvironment() {
		return processConfiguration.getEnvironment();
	}

	public int getPid() {
		return ProcessUtils.findAndReadPid(getWorkingDirectory());
	}

	public int safeGetPid() {
		try {
			return getPid();
		}
		catch (PidUnavailableException ignore) {
			return -1;
		}
	}

	public boolean isRedirectingErrorStream() {
		return processConfiguration.isRedirectingErrorStream();
	}

	public boolean isRunning() {
		return ProcessUtils.isRunning(this.process);
	}

	public File getWorkingDirectory() {
		return processConfiguration.getWorkingDirectory();
	}

	public int exitValue() {
		return process.exitValue();
	}

	public int safeExitValue() {
		try {
			return exitValue();
		}
		catch (IllegalThreadStateException ignore) {
			return -1;
		}
	}

	public boolean register(final ProcessInputStreamListener listener) {
		return (listener != null && listeners.add(listener));
	}

	public void registerShutdownHook() {
		Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
			@Override public void run() {
				shutdown();
			}
		}));
	}

	public void signalStop() {
		try {
			ProcessUtils.signalStop(this.process);
		}
		catch (IOException e) {
			log.warning("Failed to signal the process to stop!");

			if (log.isLoggable(Level.FINE)) {
				log.fine(ThrowableUtils.toString(e));
			}
		}
	}

	public int stop() {
		return stop(DEFAULT_WAIT_TIME_MILLISECONDS);
	}

	public int stop(final long milliseconds) {
		if (isRunning()) {
			int exitValue = -1;
			final int pid = safeGetPid();
			final long timeout = (System.currentTimeMillis() + milliseconds);
			final AtomicBoolean exited = new AtomicBoolean(false);

			ExecutorService executorService = Executors.newSingleThreadExecutor();

			try {
				Future<Integer> futureExitValue = executorService.submit(new Callable<Integer>() {
					@Override public Integer call() throws Exception {
						process.destroy();
						int exitValue = process.waitFor();
						exited.set(true);
						return exitValue;
					}
				});

				while (!exited.get() && System.currentTimeMillis() < timeout) {
					try {
						exitValue = futureExitValue.get(milliseconds, TimeUnit.MILLISECONDS);
						log.info(String.format("Process [%1$s] has been stopped.%n", pid));
					}
					catch (InterruptedException ignore) {
					}
				}
			}
			catch (TimeoutException e) {
				exitValue = -1;
				log.warning(String.format("Process [%1$d] did not stop within the allotted timeout of %2$d seconds.%n",
					pid, TimeUnit.MILLISECONDS.toSeconds(milliseconds)));
			}
			catch (Exception ignore) {
				// handles CancellationException, ExecutionException
			}
			finally {
				executorService.shutdownNow();
			}

			return exitValue;
		}
		else {
			return exitValue();
		}
	}

	public int shutdown() {
		if (isRunning()) {
			log.info(String.format("Stopping process [%1$d]...%n", safeGetPid()));
			signalStop();
			waitFor();
		}

		return stop();
	}

	public boolean unregister(final ProcessInputStreamListener listener) {
		return listeners.remove(listener);
	}

	public void waitFor() {
		waitFor(DEFAULT_WAIT_TIME_MILLISECONDS);
	}

	public void waitFor(final long milliseconds) {
		ThreadUtils.timedWait(milliseconds, 500, new ThreadUtils.WaitCondition() {
			@Override public boolean waiting() {
				return isRunning();
			}
		});
	}

}
