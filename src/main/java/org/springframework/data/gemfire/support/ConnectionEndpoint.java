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

package org.springframework.data.gemfire.support;

import java.net.InetSocketAddress;

import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

/**
 * The ConnectionEndpoint class models a GemFire connection endpoint in the format of hostname[portnumber],
 * where hostname is the network name or IP address of the host.
 *
 * @author John Blum
 * @see java.lang.Cloneable
 * @see java.lang.Comparable
 * @since 1.6.3
 */
@SuppressWarnings("unused")
public class ConnectionEndpoint implements Cloneable, Comparable<ConnectionEndpoint> {

	protected static final int DEFAULT_PORT = 0; // ephemeral port

	protected static final String DEFAULT_HOST = "localhost";

	private final int port;
	private final String host;

	/**
	 * Converts the InetSocketAddress into a ConnectionEndpoint.
	 *
	 * @param socketAddress the InetSocketAddress used to construct and initialize the ConnectionEndpoint.
	 * @return a ConnectionEndpoint representing the InetSocketAddress.
	 * @see java.net.InetSocketAddress
	 */
	public static ConnectionEndpoint from(InetSocketAddress socketAddress) {
		return new ConnectionEndpoint(socketAddress.getHostString(), socketAddress.getPort());
	}

	/**
	 * Parses the host and port String value into a valid ConnectionEndpoint.
	 *
	 * @param hostPort a String value containing the host and port formatted as 'host[port]'.
	 * @return a valid ConnectionEndpoint initialized with the host and port, or with DEFAULT_PORT
	 * if port was unspecified.
	 * @see #parse(String, int)
	 * @see #DEFAULT_PORT
	 */
	public static ConnectionEndpoint parse(String hostPort) {
		return parse(hostPort, DEFAULT_PORT);
	}

	/**
	 * Parses the host and port value into a valid ConnectionEndpoint.
	 *
	 * @param hostPort a String value containing the host and port formatted as 'host[port]'.
	 * @param defaultPort an Integer value indicating the default port to use if the port is unspecified
	 * in the host and port String value.
	 * @return a valid ConnectionEndpoint initialized with the host and port, or with the default port
	 * if port was unspecified.
	 * @see #ConnectionEndpoint(String, int)
	 */
	public static ConnectionEndpoint parse(String hostPort, int defaultPort) {
		Assert.hasText(hostPort, "'hostPort' must be specified");

		String host = StringUtils.trimAllWhitespace(hostPort);

		int port = defaultPort;
		int portIndex = hostPort.indexOf("[");

		if (portIndex > -1) {
			port = parsePort(parseDigits(host.substring(portIndex)), defaultPort);
			host = host.substring(0, portIndex).trim();
		}

		return new ConnectionEndpoint(host, port);
	}

	/* (non-Javadoc) */
	static String parseDigits(String value) {
		StringBuilder digits = new StringBuilder();

		if (StringUtils.hasText(value)) {
			for (char character : value.toCharArray()) {
				if (Character.isDigit(character)) {
					digits.append(character);
				}
			}
		}

		return digits.toString();
	}

	/* (non-Javadoc) */
	static int parsePort(String port, int defaultPort) {
		try {
			return Integer.parseInt(port);
		}
		catch (NumberFormatException ignore) {
			return defaultPort;
		}
	}

	/**
	 * Constructs a ConnectionEndpoint initialized with the specific host and port.
	 *
	 * @param host the hostname or IP address of the ConnectionEndpoint.  If the host is unspecified,
	 * then ConnectionEndpoint.DEFAULT_HOST will be used.
	 * @param port the (service) port number in this ConnectionEndpoint.
	 * @throws IllegalArgumentException if the port number is less than 0.
	 * @see ConnectionEndpoint#DEFAULT_HOST
	 */
	public ConnectionEndpoint(String host, int port) {
		Assert.isTrue(isValidPort(port), String.format("port number [%d] must be between 0 and 65535", port));

		this.host = SpringUtils.defaultIfEmpty(host, DEFAULT_HOST);
		this.port = port;
	}

	/* (non-Javadoc) */
	private boolean isValidPort(int port) {
		return (port >= 0 && port <= 65535);
	}

	/**
	 * Gets the host in this ConnectionEndpoint.
	 *
	 * @return a String value indicating the hostname or IP address in this ConnectionEndpoint.
	 */
	public String getHost() {
		return host;
	}

	/**
	 * Gets the port number in this ConnectionEndpoint.
	 *
	 * @return an Integer value indicating the (service) port number in this ConnectionEndpoint.
	 */
	public int getPort() {
		return port;
	}

	/**
	 * Converts this {@link ConnectionEndpoint} into an {@link InetSocketAddress} representation.
	 *
	 * @return an {@link InetSocketAddress} representation of this {@link ConnectionEndpoint}.
	 * @see java.net.InetSocketAddress
	 * @see #getHost()
	 * @see #getPort()
	 */
	public InetSocketAddress toInetSocketAddress() {
		return new InetSocketAddress(getHost(), getPort());
	}

	/* (non-Javadoc) */
	@Override
	@SuppressWarnings("all")
	protected Object clone() throws CloneNotSupportedException {
		return new ConnectionEndpoint(this.getHost(), this.getPort());
	}

	/* (non-Javadoc) */
	@Override
	@SuppressWarnings("all")
	public int compareTo(ConnectionEndpoint connectionEndpoint) {
		int compareValue = getHost().compareTo(connectionEndpoint.getHost());
		return (compareValue != 0 ? compareValue : getPort() - connectionEndpoint.getPort());
	}

	/* (non-Javadoc) */
	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		}

		if (!(obj instanceof ConnectionEndpoint)) {
			return false;
		}

		ConnectionEndpoint that = (ConnectionEndpoint) obj;

		return ObjectUtils.nullSafeEquals(this.getHost(), that.getHost())
			&& ObjectUtils.nullSafeEquals(this.getPort(), that.getPort());
	}

	/* (non-Javadoc) */
	@Override
	public int hashCode() {
		int hashValue = 17;
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getHost());
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getPort());
		return hashValue;
	}

	/* (non-Javadoc) */
	@Override
	public String toString() {
		return String.format("%1$s[%2$d]", getHost(), getPort());
	}
}
