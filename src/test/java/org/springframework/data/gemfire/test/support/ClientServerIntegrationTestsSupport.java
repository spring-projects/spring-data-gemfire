/*
 * Copyright 2016 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.test.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.data.gemfire.process.ProcessExecutor.launch;
import static org.springframework.data.gemfire.util.ArrayUtils.asArray;

import java.io.File;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.server.CacheServer;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.util.CollectionUtils;

/**
 * The {@link ClientServerIntegrationTestsSupport} class is a abstract base class encapsulating common functionality
 * to support the implementation of GemFire client/server tests.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @see java.time.LocalDateTime
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext
 * @see org.springframework.data.gemfire.process.ProcessExecutor
 * @see org.springframework.data.gemfire.process.ProcessWrapper
 * @see org.springframework.data.gemfire.test.support.SocketUtils
 * @see org.springframework.data.gemfire.test.support.ThreadUtils
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public class ClientServerIntegrationTestsSupport {

	protected static final long DEFAULT_WAIT_DURATION = TimeUnit.SECONDS.toMillis(30);
	protected static final long DEFAULT_WAIT_INTERVAL = 500L; // milliseconds

	protected static final String DEBUG_ENDPOINT = "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005";
	protected static final String DEBUGGING_ENABLED_PROPERTY = "spring.data.gemfire.debugging.enabled";
	protected static final String DEFAULT_HOSTNAME = "localhost";
	protected static final String DIRECTORY_DELETE_ON_EXIT_PROPERTY = "spring.data.gemfire.directory.delete-on-exit";
	protected static final String GEMFIRE_CACHE_SERVER_PORT_PROPERTY = "spring.data.gemfire.cache.server.port";
	protected static final String GEMFIRE_POOL_LOCATORS_PROPERTY = "spring.data.gemfire.pool.locators";
	protected static final String GEMFIRE_POOL_SERVERS_PROPERTY = "spring.data.gemfire.pool.servers";
	protected static final String GEMFIRE_LOG_FILE = "gemfire-server.log";
	protected static final String GEMFIRE_LOG_FILE_PROPERTY = "spring.data.gemfire.log.file";
	protected static final String GEMFIRE_LOG_LEVEL = "error";
	protected static final String GEMFIRE_LOG_LEVEL_PROPERTY = "spring.data.gemfire.log.level";
	protected static final String PROCESS_RUN_MANUAL_PROPERTY = "spring.data.gemfire.process.run-manual";
	protected static final String SYSTEM_PROPERTIES_LOG_FILE = "system-properties.log";
	protected static final String TEST_GEMFIRE_LOG_LEVEL = "error";

	/* (non-Javadoc) */
	protected static String asApplicationName(Class<?> type) {
		return type.getSimpleName();
	}

	/* (non-Javadoc) */
	protected static String asDirectoryName(Class<?> type) {
		return String.format("%1$s-%2$s", asApplicationName(type),
			LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd-hh-mm-ss")));
	}

	/* (non-Javadoc) */
	protected static File createDirectory(String pathname) {
		return createDirectory(new File(pathname));
	}

	/* (non-Javadoc) */
	protected static File createDirectory(File directory) {

		assertThat(directory.isDirectory() || directory.mkdirs())
			.as(String.format("Failed to create directory [%s]", directory)).isTrue();

		if (isDeleteDirectoryOnExit()) {
			directory.deleteOnExit();
		}

		return directory;
	}

	/* (non-Javadoc) */
	protected static int findAvailablePort() throws IOException {

		ServerSocket serverSocket = null;

		try {
			serverSocket = new ServerSocket(0);
			return serverSocket.getLocalPort();
		}
		finally {
			SocketUtils.close(serverSocket);
		}
	}

	/* (non-Javadoc) */
	protected static String getClassNameAsPath(Class type) {
		return type.getName().replaceAll("\\.", "/");
	}

	/* (non-Javadoc) */
	protected static String getClassNameAsPath(Object obj) {
		return getClassNameAsPath(obj.getClass());
	}

	/* (non-Javadoc) */
	protected static String getPackageNameAsPath(Class type) {
		return type.getPackage().getName().replaceAll("\\.", "/");
	}

	/* (non-Javadoc) */
	protected static String getPackageNameAsPath(Object obj) {
		return getPackageNameAsPath(obj.getClass());
	}

	/* (non-Javadoc) */
	protected static String getContextXmlFileLocation(Class type) {
		return getClassNameAsPath(type).concat("-context.xml");
	}

	/* (non-Javadoc) */
	protected static String getServerContextXmlFileLocation(Class type) {
		return getClassNameAsPath(type).concat("-server-context.xml");
	}

	/* (non-Javadoc) */
	protected static boolean isDeleteDirectoryOnExit() {
		return Boolean.valueOf(System.getProperty(DIRECTORY_DELETE_ON_EXIT_PROPERTY, Boolean.TRUE.toString()));
	}

	/* (non-Javadoc) */
	protected static int intValue(Number number) {
		return (number != null ? number.intValue() : 0);
	}

	/* (non-Javadoc) */
	protected static String logFile() {
		return logFile(GEMFIRE_LOG_FILE);
	}

	/* (non-Javadoc) */
	protected static String logFile(String defaultLogFilePathname) {
		return System.getProperty(GEMFIRE_LOG_FILE_PROPERTY, defaultLogFilePathname);
	}

	/* (non-Javadoc) */
	protected static String logLevel() {
		return logLevel(GEMFIRE_LOG_LEVEL);
	}

	/* (non-Javadoc) */
	protected static String logLevel(String defaultLogLevel) {
		return System.getProperty(GEMFIRE_LOG_LEVEL_PROPERTY, defaultLogLevel);
	}

	/* (non-Javadoc) */
	protected static void logSystemProperties() throws IOException {
		FileUtils.write(new File(SYSTEM_PROPERTIES_LOG_FILE),
			String.format("%s", CollectionUtils.toString(System.getProperties())));
	}

	/* (non-Javadoc) */
	protected static ProcessWrapper run(Class<?> type, String... arguments) throws IOException {
		return run(createDirectory(asDirectoryName(type)), type, arguments);
	}

	/* (non-Javadoc) */
	protected static ProcessWrapper run(File workingDirectory, Class<?> type, String... arguments) throws IOException {
		return (isProcessRunAuto() ? launch(createDirectory(workingDirectory), type, arguments) : null);
	}

	/* (non-Javadoc) */
	protected static ProcessWrapper run(String classpath, Class<?> type, String... arguments) throws IOException {
		return run(createDirectory(asDirectoryName(type)), classpath, type, arguments);
	}

	/* (non-Javadoc) */
	protected static ProcessWrapper run(File workingDirectory, String classpath, Class<?> type, String... arguments)
			throws IOException {

		return (isProcessRunAuto() ? launch(createDirectory(workingDirectory), classpath, type, arguments) : null);
	}

	/* (non-Javadoc) */
	protected static boolean isProcessRunAuto() {
		return !isProcessRunManual();
	}

	/* (non-Javadoc) */
	protected static boolean isProcessRunManual() {
		return Boolean.getBoolean(PROCESS_RUN_MANUAL_PROPERTY);
	}

	/* (non-Javadoc) */
	protected static AnnotationConfigApplicationContext runSpringApplication(Class<?> annotatedClass, String... args) {
		return runSpringApplication(asArray(annotatedClass), args);
	}

	/* (non-Javadoc) */
	protected static AnnotationConfigApplicationContext runSpringApplication(Class<?>[] annotatedClasses,
			String... args) {

		AnnotationConfigApplicationContext applicationContext =
			new AnnotationConfigApplicationContext(annotatedClasses);

		applicationContext.registerShutdownHook();

		return applicationContext;
	}

	/* (non-Javadoc) */
	protected static boolean stop(ProcessWrapper process) {
		return stop(process, DEFAULT_WAIT_DURATION);
	}

	/* (non-Javadoc) */
	protected static boolean stop(ProcessWrapper process, long duration) {

		return Optional.ofNullable(process)
			.map(it -> {

				it.stop(duration);

				if (it.isNotRunning() && isDeleteDirectoryOnExit()) {
					FileSystemUtils.deleteRecursive(it.getWorkingDirectory());
				}

				return it.isRunning();
			})
			.orElse(true);
	}

	/* (non-Javadoc) */
	protected static boolean waitForCacheServerToStart(CacheServer cacheServer) {
		return waitForServerToStart(cacheServer.getBindAddress(), cacheServer.getPort(), DEFAULT_WAIT_DURATION);
	}

	/* (non-Javadoc) */
	protected static boolean waitForCacheServerToStart(CacheServer cacheServer, long duration) {
		return waitForServerToStart(cacheServer.getBindAddress(), cacheServer.getPort(), duration);
	}

	/* (non-Javadoc) */
	protected static boolean waitForServerToStart(String host, int port) {
		return waitForServerToStart(host, port, DEFAULT_WAIT_DURATION);
	}

	/* (non-Javadoc) */
	protected static boolean waitForServerToStart(final String host, final int port, long duration) {

		return ThreadUtils.timedWait(duration, DEFAULT_WAIT_INTERVAL, new ThreadUtils.WaitCondition() {

			AtomicBoolean connected = new AtomicBoolean(false);

			public boolean waiting() {

				Socket socket = null;

				try {
					if (!connected.get()) {
						socket = new Socket(host, port);
						connected.set(true);
					}
				}
				catch (IOException ignore) {
				}
				finally {
					SocketUtils.close(socket);
				}

				return !connected.get();
			}
		});
	}

	protected static boolean waitOn(Condition condition) {
		return waitOn(condition, DEFAULT_WAIT_DURATION);
	}

	@SuppressWarnings("all")
	protected static boolean waitOn(Condition condition, long duration) {

		long timeout = (System.currentTimeMillis() + duration);

		try {
			while (!condition.evaluate() && System.currentTimeMillis() < timeout) {
				synchronized (condition) {
					TimeUnit.MILLISECONDS.timedWait(condition, DEFAULT_WAIT_INTERVAL);
				}
			}
		}
		catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		return condition.evaluate();
	}

	protected interface Condition {
		boolean evaluate();
	}
}
