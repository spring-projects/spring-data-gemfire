/*
 * Copyright 2016-2019 the original author or authors.
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

import java.io.File;
import java.io.IOException;
import java.net.Socket;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import com.gemstone.gemfire.cache.server.CacheServer;

/**
 * The {@link ClientServerIntegrationTestsSupport} class is a abstract base class encapsulating common functionality
 * to support the implementation of GemFire client/server tests.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.cache.server.CacheServer
 * @see org.springframework.data.gemfire.test.support.SocketUtils
 * @see org.springframework.data.gemfire.test.support.ThreadUtils
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public class ClientServerIntegrationTestsSupport {

	protected static final long DEFAULT_WAIT_DURATION = TimeUnit.SECONDS.toMillis(20);
	protected static final long DEFAULT_WAIT_INTERVAL = 500L;

	/* (non-Javadoc) */
	protected static File createDirectory(String pathname) {
		File directory = new File(pathname);

		assertThat(directory.isDirectory() || directory.mkdirs())
			.as(String.format("Failed to create directory [%s]", directory)).isTrue();

		directory.deleteOnExit();

		return directory;
	}

	/* (non-Javadoc) */
	protected static boolean waitForCacheServerToStart(CacheServer cacheServer) {
		return waitForCacheServerToStart(cacheServer.getBindAddress(), cacheServer.getPort(), DEFAULT_WAIT_DURATION);
	}

	/* (non-Javadoc) */
	protected static boolean waitForCacheServerToStart(CacheServer cacheServer, long duration) {
		return waitForCacheServerToStart(cacheServer.getBindAddress(), cacheServer.getPort(), duration);
	}

	/* (non-Javadoc) */
	protected static boolean waitForCacheServerToStart(String host, int port) {
		return waitForCacheServerToStart(host, port, DEFAULT_WAIT_DURATION);
	}

	/* (non-Javadoc) */
	protected static boolean waitForCacheServerToStart(final String host, final int port, long duration) {
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
}
