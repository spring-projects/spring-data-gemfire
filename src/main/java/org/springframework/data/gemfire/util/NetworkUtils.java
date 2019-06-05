/*
 * Copyright 2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.util;

/**
 * Abstract utility class providing functions for networking.
 *
 * @author John Blum
 * @since 2.2.0
 */
public abstract class NetworkUtils {

	public static final String INVALID_PORT_MESSAGE =
		"Port [%d] must be greater than equal to 0 and less than 65536";

	public static final String INVALID_NO_EPHEMERAL_PORT_MESSAGE =
		"Port [%d] must be greater than 0 and less than 65536";

	/**
	 * Determines whether the given {@link Integer#TYPE port} is valid.
	 *
	 * Technically, port 0 is valid too but no client would use port 0 (the ephemeral port) to connect to a service.
	 *
	 * @param port port to evaluate.
	 * @return a boolean value indicating whether the {@link Integer#TYPE port} is valid or not.
	 */
	public static boolean isValidPort(int port) {
		return port > -1 && port < 65536;
	}

	public static boolean isValidNonEphemeralPort(int port) {
		return isValidPort(port) && port > 0;
	}
}
