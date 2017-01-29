/*
 * Copyright 2016-2018 the original author or authors.
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

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Logger;

/**
 * {@link SocketUtils} is a utility class for managing {@link Socket} and {@link ServerSocket} objects.
 *
 * @author John Blum
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public abstract class SocketUtils {

	private static final Logger log = Logger.getLogger(SocketUtils.class.getName());

	/* (non-Javadoc) */
	public static boolean close(Socket socket) {
		try {
			if (socket != null) {
				socket.close();
				return true;
			}
		}
		catch (IOException ignore) {
			log.warning(String.format("Failed to close Socket [%s]", socket));
			log.warning(ThrowableUtils.toString(ignore));
		}

		return false;
	}

	/* (non-Javadoc) */
	public static boolean close(ServerSocket serverSocket) {
		try {
			if (serverSocket != null) {
				serverSocket.close();
				return true;
			}
		}
		catch (IOException ignore) {
			log.warning(String.format("Failed to close ServerSocket [%s]", serverSocket));
			log.warning(ThrowableUtils.toString(ignore));
		}

		return false;
	}
}
