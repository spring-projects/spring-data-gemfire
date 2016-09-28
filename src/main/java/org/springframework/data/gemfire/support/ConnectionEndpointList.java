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

package org.springframework.data.gemfire.support;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.CollectionUtils;

/**
 * The ConnectionEndpointList class is an Iterable collection of ConnectionEndpoint objects.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see java.net.InetSocketAddress
 * @see java.util.AbstractList
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @since 1.6.3
 */
@SuppressWarnings("unused")
public class ConnectionEndpointList implements Iterable<ConnectionEndpoint> {

	private final List<ConnectionEndpoint> connectionEndpoints;

	/**
	 * Converts the array of InetSocketAddresses into an instance of ConnectionEndpointList.
	 *
	 * @param socketAddresses the array of InetSocketAddresses used to initialize an instance of ConnectionEndpointList.
	 * @return a ConnectionEndpointList representing the array of InetSocketAddresses.
	 * @see java.net.InetSocketAddress
	 * @see #from(Iterable)
	 */
	public static ConnectionEndpointList from(InetSocketAddress... socketAddresses) {
		return from(Arrays.asList(socketAddresses));
	}

	/**
	 * Converts the Iterable collection of InetSocketAddresses into an instance of ConnectionEndpointList.
	 *
	 * @param socketAddresses in Iterable collection of InetSocketAddresses used to initialize an instance
	 * of ConnectionEndpointList.
	 * @return a ConnectionEndpointList representing the array of InetSocketAddresses.
	 * @see java.net.InetSocketAddress
	 */
	public static ConnectionEndpointList from(Iterable<InetSocketAddress> socketAddresses) {
		List<ConnectionEndpoint> connectionEndpoints = new ArrayList<ConnectionEndpoint>();

		for (InetSocketAddress socketAddress : CollectionUtils.nullSafeIterable(socketAddresses)) {
			connectionEndpoints.add(ConnectionEndpoint.from(socketAddress));
		}

		return new ConnectionEndpointList(connectionEndpoints);
	}

	/**
	 * Parses the array of hosts and ports in the format 'host[port]' to convert into an instance
	 * of ConnectionEndpointList.
	 *
	 * @param defaultPort the default port number to use if port is not specified in a host and port value.
	 * @param hostsPorts the array of hosts and ports to parse.
	 * @return a ConnectionEndpointList representing the hosts and ports in the array.
	 * @see org.springframework.data.gemfire.support.ConnectionEndpoint#parse(String, int)
	 */
	public static ConnectionEndpointList parse(int defaultPort, String... hostsPorts) {
		List<ConnectionEndpoint> connectionEndpoints = new ArrayList<ConnectionEndpoint>(
			ArrayUtils.length((Object) hostsPorts));

		for (String hostPort : ArrayUtils.nullSafeArray(hostsPorts, String.class)) {
			connectionEndpoints.add(ConnectionEndpoint.parse(hostPort, defaultPort));
		}

		return new ConnectionEndpointList(connectionEndpoints);
	}

	/**
	 * Constructs an empty, uninitialized instance of the ConnectionEndpointList collection.
	 */
	public ConnectionEndpointList() {
		this(Collections.<ConnectionEndpoint>emptyList());
	}

	/**
	 * Constructs an instance of ConnectionEndpointList initialized with the the array of ConnectionEndpoints.
	 *
	 * @param connectionEndpoints is an array containing ConnectionEndpoints to add to this collection.
	 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
	 * @see #ConnectionEndpointList(Iterable)
	 */
	public ConnectionEndpointList(ConnectionEndpoint... connectionEndpoints) {
		this(Arrays.asList(connectionEndpoints));
	}

	/**
	 * Constructs an instance of ConnectionEndpointList initialized with the Iterable collection of ConnectionEndpoints.
	 *
	 * @param connectionEndpoints the Iterable object containing ConnectionEndpoints to add to this collection.
	 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
	 * @see java.lang.Iterable
	 */
	public ConnectionEndpointList(Iterable<ConnectionEndpoint> connectionEndpoints) {
		this.connectionEndpoints = new ArrayList<ConnectionEndpoint>();
		add(connectionEndpoints);
	}

	/**
	 * Adds the array of ConnectionEndpoints to this list.
	 *
	 * @param connectionEndpoints the array of ConnectionEndpoints to add to this list.
	 * @return this ConnectionEndpointList to support the Builder pattern style of chaining.
	 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
	 * @see #add(Iterable)
	 */
	public final ConnectionEndpointList add(ConnectionEndpoint... connectionEndpoints) {
		Collections.addAll(this.connectionEndpoints, connectionEndpoints);
		return this;
	}

	/**
	 * Adds the Iterable collection of ConnectionEndpoints to this list.
	 *
	 * @param connectionEndpoints the Iterable collection of ConnectionEndpoints to add to this list.
	 * @return this ConnectionEndpointList to support the Builder pattern style of chaining.
	 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
	 * @see #add(ConnectionEndpoint...)
	 */
	public final ConnectionEndpointList add(Iterable<ConnectionEndpoint> connectionEndpoints) {
		for (ConnectionEndpoint connectionEndpoint : CollectionUtils.nullSafeIterable(connectionEndpoints)) {
			this.connectionEndpoints.add(connectionEndpoint);
		}

		return this;
	}

	/**
	 * Finds all ConnectionEndpoints in this list with the specified hostname.
	 *
	 * @param host a String indicating the hostname to use in the match.
	 * @return a ConnectionEndpointList (sub-List) containing all the ConnectionEndpoints matching the given hostname.
	 * @see #findBy(int)
	 */
	public ConnectionEndpointList findBy(String host) {
		List<ConnectionEndpoint> connectionEndpoints = new ArrayList<ConnectionEndpoint>(size());

		for (ConnectionEndpoint connectionEndpoint : this) {
			if (connectionEndpoint.getHost().equals(host)) {
				connectionEndpoints.add(connectionEndpoint);
			}
		}

		return new ConnectionEndpointList(connectionEndpoints);
	}

	/**
	 * Finds all ConnectionEndpoints in this list with the specified port number.
	 *
	 * @param port an Integer value indicating the port number to use in the match.
	 * @return a ConnectionEndpointList (sub-List) containing all the ConnectionEndpoints matching the given port number.
	 * @see #findBy(String)
	 */
	public ConnectionEndpointList findBy(int port) {
		List<ConnectionEndpoint> connectionEndpoints = new ArrayList<ConnectionEndpoint>(size());

		for (ConnectionEndpoint connectionEndpoint : this) {
			if (connectionEndpoint.getPort() == port) {
				connectionEndpoints.add(connectionEndpoint);
			}
		}

		return new ConnectionEndpointList(connectionEndpoints);
	}

	/**
	 * Determines whether this collection contains any ConnectionEndpoints.
	 *
	 * @return a boolean value indicating whether this collection contains any ConnectionEndpoints.
	 */
	public boolean isEmpty() {
		return connectionEndpoints.isEmpty();
	}

	/* (non-Javadoc) */
	@Override
	public Iterator<ConnectionEndpoint> iterator() {
		return Collections.unmodifiableList(connectionEndpoints).iterator();
	}

	/**
	 * Determines the number of ConnectionEndpoints contained in this collection.
	 *
	 * @return an integer value indicating the number of ConnectionEndpoints contained in this collection.
	 */
	public int size() {
		return connectionEndpoints.size();
	}

	/* (non-Javadoc) */
	@Override
	public String toString() {
		return connectionEndpoints.toString();
	}
}
