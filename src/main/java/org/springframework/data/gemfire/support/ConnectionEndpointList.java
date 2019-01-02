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
import java.util.AbstractList;
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
public class ConnectionEndpointList extends AbstractList<ConnectionEndpoint> {

	private final List<ConnectionEndpoint> connectionEndpoints;

	/**
	 * Factory method for creating a {@link ConnectionEndpointList} from an array of {@link ConnectionEndpoint}s.
	 *
	 * @param connectionEndpoints the array of {@link ConnectionEndpoint}s used to initialize
	 * the {@link ConnectionEndpointList}.
	 * @return a {@link ConnectionEndpointList} initialized with the array of {@link ConnectionEndpoint}s.
	 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
	 */
	public static ConnectionEndpointList from(ConnectionEndpoint... connectionEndpoints) {
		return new ConnectionEndpointList(connectionEndpoints);
	}

	/**
	 * Converts an array of {@link InetSocketAddress} into an instance of {@link ConnectionEndpointList}.
	 *
	 * @param socketAddresses the array of {@link InetSocketAddress} used to initialize
	 * an instance of {@link ConnectionEndpointList}.
	 * @return a {@link ConnectionEndpointList} initialized with the array of {@link InetSocketAddress}.
	 * @see java.net.InetSocketAddress
	 * @see #from(Iterable)
	 */
	public static ConnectionEndpointList from(InetSocketAddress... socketAddresses) {
		return from(Arrays.asList(socketAddresses));
	}

	/**
	 * Converts an {@link Iterable} collection of {@link InetSocketAddress} into an instance
	 * of {@link ConnectionEndpointList}.
	 *
	 * @param socketAddresses in {@link Iterable} collection of {@link InetSocketAddress} used to initialize
	 * an instance of {@link ConnectionEndpointList}.
	 * @return a {@link ConnectionEndpointList} initialized with the array of {@link InetSocketAddress}.
	 * @see java.lang.Iterable
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
			ArrayUtils.length(hostsPorts));

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

	/* (non-Javadoc) */
	@Override
	public boolean add(ConnectionEndpoint connectionEndpoint) {
		return (add(ArrayUtils.asArray(connectionEndpoint)) == this);
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
	 * Clears the current list of {@link ConnectionEndpoint}s.
	 */
	@Override
	public void clear() {
		this.connectionEndpoints.clear();
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
	 * Finds the first {@link ConnectionEndpoint} in the collection with the given host.
	 *
	 * @param host a String indicating the hostname of the {@link ConnectionEndpoint} to find.
	 * @return the first {@link ConnectionEndpoint} in this collection with the given host,
	 * or null if no {@link ConnectionEndpoint} exists with the given hostname.
	 * @see #findBy(String)
	 */
	public ConnectionEndpoint findOne(String host) {
		ConnectionEndpointList connectionEndpoints = findBy(host);
		return (connectionEndpoints.isEmpty() ? null : connectionEndpoints.connectionEndpoints.get(0));
	}

	/**
	 * Finds the first {@link ConnectionEndpoint} in the collection with the given port.
	 *
	 * @param port an integer indicating the port number of the {@link ConnectionEndpoint} to find.
	 * @return the first {@link ConnectionEndpoint} in this collection with the given port,
	 * or null if no {@link ConnectionEndpoint} exists with the given port number.
	 * @see #findBy(int)
	 */
	public ConnectionEndpoint findOne(int port) {
		ConnectionEndpointList connectionEndpoints = findBy(port);
		return (connectionEndpoints.isEmpty() ? null : connectionEndpoints.connectionEndpoints.get(0));
	}

	/**
	 * Gets the {@link ConnectionEndpoint} at the given index in this list.
	 *
	 * @param index an integer value indicating the index of the {@link ConnectionEndpoint} of interest.
	 * @return the {@link ConnectionEndpoint} at index in this list.
	 * @throws IndexOutOfBoundsException if the index is not within the bounds of this list.
	 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
	 * @see java.util.AbstractList#get(int)
	 */
	@Override
	public ConnectionEndpoint get(int index) {
		return connectionEndpoints.get(index);
	}

	/**
	 * Sets the element at the given index in this list to the given {@link ConnectionEndpoint}.
	 *
	 * @param index the index in the list at which to set the {@link ConnectionEndpoint}.
	 * @param element the {@link ConnectionEndpoint} to set in this list at the given index.
	 * @return the old {@link ConnectionEndpoint} at index in this list or null if no {@link ConnectionEndpoint}
	 * at index existed.
	 * @throws IndexOutOfBoundsException if the index is not within the bounds of this list.
	 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
	 * @see java.util.AbstractList#set(int, Object)
	 */
	@Override
	public ConnectionEndpoint set(int index, ConnectionEndpoint element) {
		return connectionEndpoints.set(index, element);
	}

	/**
	 * Determines whether this collection contains any ConnectionEndpoints.
	 *
	 * @return a boolean value indicating whether this collection contains any ConnectionEndpoints.
	 */
	@Override
	public boolean isEmpty() {
		return connectionEndpoints.isEmpty();
	}

	/* (non-Javadoc) */
	@Override
	@SuppressWarnings("all")
	public Iterator<ConnectionEndpoint> iterator() {
		return Collections.unmodifiableList(connectionEndpoints).iterator();
	}

	/**
	 * Determines the number of ConnectionEndpoints contained in this collection.
	 *
	 * @return an integer value indicating the number of ConnectionEndpoints contained in this collection.
	 */
	@Override
	public int size() {
		return connectionEndpoints.size();
	}

	/**
	 * Converts this collection of {@link ConnectionEndpoint}s into an array of {@link ConnectionEndpoint}s.
	 *
	 * @return an array of {@link ConnectionEndpoint}s representing this collection.
	 */
	@Override
	@SuppressWarnings("all")
	public ConnectionEndpoint[] toArray() {
		return connectionEndpoints.toArray(new ConnectionEndpoint[connectionEndpoints.size()]);
	}

	/**
	 * Converts this collection of {@link ConnectionEndpoint}s into a {@link List} of {@link InetSocketAddress}es.
	 *
	 * @return a {@link List} of {@link InetSocketAddress}es representing this collection of {@link ConnectionEndpoint}s.
	 * @see org.springframework.data.gemfire.support.ConnectionEndpoint#toInetSocketAddress()
	 * @see java.net.InetSocketAddress
	 * @see java.util.List
	 */
	public List<InetSocketAddress> toInetSocketAddresses() {
		List<InetSocketAddress> inetSocketAddresses = new ArrayList<InetSocketAddress>(size());

		for (ConnectionEndpoint connectionEndpoint : this) {
			inetSocketAddresses.add(connectionEndpoint.toInetSocketAddress());
		}

		return inetSocketAddresses;
	}

	/* (non-Javadoc) */
	@Override
	public String toString() {
		return connectionEndpoints.toString();
	}
}
