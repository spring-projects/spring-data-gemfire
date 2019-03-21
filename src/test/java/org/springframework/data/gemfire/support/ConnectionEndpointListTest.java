/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.support;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The ConnectionEndpointListTest class is a test suite of test cases testing the contract and functionality
 * of the ConnectionEndpointList class.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.support.ConnectionEndpointList
 * @since 1.6.3
 */
public class ConnectionEndpointListTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	protected ConnectionEndpoint newConnectionEndpoint(String host, int port) {
		return new ConnectionEndpoint(host, port);
	}

	@Test
	public void constructNewEmptyConnectionEndpointList() {
		ConnectionEndpointList connectionEndpoints = new ConnectionEndpointList();

		assertThat(connectionEndpoints.isEmpty(), is(true));
		assertThat(connectionEndpoints.size(), is(equalTo(0)));
	}

	@Test
	public void constructNewInitializedConnectionEndpointList() {
		ConnectionEndpoint[] connectionEndpoints = {
			newConnectionEndpoint("jambox", 1234),
			newConnectionEndpoint("skullbox", 9876)
		};

		ConnectionEndpointList connectionEndpointList = new ConnectionEndpointList(connectionEndpoints);

		assertThat(connectionEndpointList.isEmpty(), is(false));
		assertThat(connectionEndpointList.size(), is(equalTo(connectionEndpoints.length)));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : connectionEndpointList) {
			assertThat(connectionEndpoint, is(equalTo(connectionEndpoints[index++])));
		}
	}

	@Test
	public void fromInetSocketAddresses() {
		InetSocketAddress[] inetSocketAddresses = {
			new InetSocketAddress("localhost", 1234),
			new InetSocketAddress("localhost", 9876)
		};

		ConnectionEndpointList connectionEndpoints = ConnectionEndpointList.from(inetSocketAddresses);

		assertThat(connectionEndpoints, is(notNullValue()));
		assertThat(connectionEndpoints.isEmpty(), is(false));
		assertThat(connectionEndpoints.size(), is(equalTo(2)));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : connectionEndpoints) {
			assertThat(connectionEndpoint.getHost(), is(equalTo(inetSocketAddresses[index].getHostString())));
			assertThat(connectionEndpoint.getPort(), is(equalTo(inetSocketAddresses[index++].getPort())));
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void fromIterableInetSocketAddressesIsNullSafe() {
		ConnectionEndpointList connectionEndpoints = ConnectionEndpointList.from((Iterable) null);

		assertThat(connectionEndpoints, is(notNullValue()));
		assertThat(connectionEndpoints.isEmpty(), is(true));
		assertThat(connectionEndpoints.size(), is(equalTo(0)));
	}

	@Test
	public void parse() {
		ConnectionEndpointList connectionEndpoints = ConnectionEndpointList
			.parse(24842, "mercury[11235]", "venus", "[12480]",
				"[]", "jupiter[]", "saturn[1, 2-Hundred and 34.zero5]", "neptune[four]");

		String[] expectedHostPorts = { "mercury[11235]", "venus[24842]", "localhost[12480]", "localhost[24842]",
			"jupiter[24842]", "saturn[12345]", "neptune[24842]" };

		assertThat(connectionEndpoints, is(notNullValue()));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : connectionEndpoints) {
			assertThat(connectionEndpoint.toString(), is(equalTo(expectedHostPorts[index++])));
		}
	}

	@Test
	public void parseWithEmptyHostsPortsArgument() {
		ConnectionEndpointList connectionEndpoints = ConnectionEndpointList.parse(1234);

		assertThat(connectionEndpoints, is(notNullValue()));
		assertThat(connectionEndpoints.isEmpty(), is(true));
		assertThat(connectionEndpoints.size(), is(equalTo(0)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void addAdditionalConnectionEndpoints() {
		ConnectionEndpointList connectionEndpointList = new ConnectionEndpointList();

		assertThat(connectionEndpointList.isEmpty(), is(true));

		ConnectionEndpoint[] connectionEndpointsArray = { newConnectionEndpoint("Mercury", 1111) };

		Iterable<ConnectionEndpoint> connectionEndpointsIterable = Arrays.asList(
			newConnectionEndpoint("Venus", 2222),
			newConnectionEndpoint("Earth", 3333),
			newConnectionEndpoint("Mars", 4444),
			newConnectionEndpoint("Jupiter", 5555),
			newConnectionEndpoint("Saturn", 6666),
			newConnectionEndpoint("Uranis", 7777),
			newConnectionEndpoint("Neptune", 8888),
			newConnectionEndpoint("Pluto", 9999)
		);

		assertThat(connectionEndpointList.add(connectionEndpointsArray).add(connectionEndpointsIterable),
			is(sameInstance(connectionEndpointList)));

		List<ConnectionEndpoint> expected = new ArrayList<ConnectionEndpoint>(9);

		expected.add(connectionEndpointsArray[0]);
		expected.addAll((List) connectionEndpointsIterable);

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : connectionEndpointList) {
			assertThat(connectionEndpoint, is(equalTo(expected.get(index++))));
		}
	}

	@Test
	public void findByHostName() {
		ConnectionEndpointList connectionEndpoints = new ConnectionEndpointList(
			newConnectionEndpoint("Earth", 10334),
			newConnectionEndpoint("Earth", 40404),
			newConnectionEndpoint("Mars", 10334),
			newConnectionEndpoint("Jupiter", 1234),
			newConnectionEndpoint("Saturn", 9876),
			newConnectionEndpoint("Neptune", 12345)
		);

		assertThat(connectionEndpoints.isEmpty(), is(false));
		assertThat(connectionEndpoints.size(), is(equalTo(6)));

		ConnectionEndpointList actual = connectionEndpoints.findBy("Earth");

		assertThat(actual, is(notNullValue()));
		assertThat(actual.isEmpty(), is(false));
		assertThat(actual.size(), is(equalTo(2)));

		String[] expected = { "Earth[10334]", "Earth[40404]" };
		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : actual) {
			assertThat(connectionEndpoint.toString(), is(equalTo(expected[index++])));
		}

		actual = connectionEndpoints.findBy("Saturn");

		assertThat(actual, is(notNullValue()));
		assertThat(actual.isEmpty(), is(false));
		assertThat(actual.size(), is(equalTo(1)));
		assertThat(actual.iterator().next().toString(), is(equalTo("Saturn[9876]")));

		actual = connectionEndpoints.findBy("Pluto");

		assertThat(actual, is(notNullValue()));
		assertThat(actual.isEmpty(), is(true));
		assertThat(actual.size(), is(equalTo(0)));
	}

	@Test
	public void findByPortNumber() {
		ConnectionEndpointList connectionEndpoints = new ConnectionEndpointList(
			newConnectionEndpoint("Earth", 10334),
			newConnectionEndpoint("Earth", 40404),
			newConnectionEndpoint("Mars", 10334),
			newConnectionEndpoint("Jupiter", 1234),
			newConnectionEndpoint("Saturn", 9876),
			newConnectionEndpoint("Neptune", 12345)
		);

		assertThat(connectionEndpoints.isEmpty(), is(false));
		assertThat(connectionEndpoints.size(), is(equalTo(6)));

		ConnectionEndpointList actual = connectionEndpoints.findBy(10334);

		assertThat(actual, is(notNullValue()));
		assertThat(actual.isEmpty(), is(false));
		assertThat(actual.size(), is(equalTo(2)));

		String[] expected = { "Earth[10334]", "Mars[10334]" };
		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : actual) {
			assertThat(connectionEndpoint.toString(), is(equalTo(expected[index++])));
		}

		actual = connectionEndpoints.findBy(1234);

		assertThat(actual, is(notNullValue()));
		assertThat(actual.isEmpty(), is(false));
		assertThat(actual.size(), is(equalTo(1)));
		assertThat(actual.iterator().next().toString(), is(equalTo("Jupiter[1234]")));

		actual = connectionEndpoints.findBy(80);

		assertThat(actual, is(notNullValue()));
		assertThat(actual.isEmpty(), is(true));
		assertThat(actual.size(), is(equalTo(0)));
	}

	@Test
	public void toStringRepresentation() {
		ConnectionEndpointList connectionEndpoints = ConnectionEndpointList.parse(10334,
			"skullbox[12480]", "saturn[ 1  12 3  5]", "neptune");

		assertThat(connectionEndpoints.toString(), is(equalTo("[skullbox[12480], saturn[11235], neptune[10334]]")));
	}

}
