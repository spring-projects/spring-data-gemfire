/*
 * Copyright 2010-2019 the original author or authors.
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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
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
 * @see java.net.InetSocketAddress
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @see org.springframework.data.gemfire.support.ConnectionEndpointList
 * @since 1.6.3
 */
public class ConnectionEndpointListTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

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
	public void fromConnectionEndpoints() {
		ConnectionEndpoint[] connectionEndpoints = {
			newConnectionEndpoint("boombox", 10334),
			newConnectionEndpoint("skullbox", 40404),
			newConnectionEndpoint("toolbox", 1099)
		};

		ConnectionEndpointList list = ConnectionEndpointList.from(connectionEndpoints);

		assertThat(list, is(notNullValue()));
		assertThat(list.size(), is(equalTo(connectionEndpoints.length)));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : list) {
			assertThat(connectionEndpoint, is(equalTo(connectionEndpoints[index++])));
		}
	}

	@Test
	public void fromInetSocketAddresses() {
		InetSocketAddress[] inetSocketAddresses = {
			new InetSocketAddress("localhost", 1234),
			new InetSocketAddress("localhost", 9876)
		};

		ConnectionEndpointList list = ConnectionEndpointList.from(inetSocketAddresses);

		assertThat(list, is(notNullValue()));
		assertThat(list.size(), is(equalTo(inetSocketAddresses.length)));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : list) {
			assertThat(connectionEndpoint.getHost(), is(equalTo(inetSocketAddresses[index].getHostString())));
			assertThat(connectionEndpoint.getPort(), is(equalTo(inetSocketAddresses[index++].getPort())));
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void fromIterableInetSocketAddressesIsNullSafe() {
		ConnectionEndpointList list = ConnectionEndpointList.from((Iterable) null);

		assertThat(list, is(notNullValue()));
		assertThat(list.isEmpty(), is(true));
	}

	@Test
	public void parse() {
		ConnectionEndpointList list = ConnectionEndpointList.parse(24842, "mercury[11235]", "venus", "[12480]", "[]",
			"jupiter[]", "saturn[1, 2-Hundred and 34.zero5]", "neptune[four]");

		String[] expectedHostPorts = { "mercury[11235]", "venus[24842]", "localhost[12480]", "localhost[24842]",
			"jupiter[24842]", "saturn[12345]", "neptune[24842]" };

		assertThat(list, is(notNullValue()));
		assertThat(list.size(), is(equalTo(expectedHostPorts.length)));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : list) {
			assertThat(connectionEndpoint.toString(), is(equalTo(expectedHostPorts[index++])));
		}
	}

	@Test
	public void parseWithEmptyHostsPortsArgument() {
		ConnectionEndpointList list = ConnectionEndpointList.parse(1234);

		assertThat(list, is(notNullValue()));
		assertThat(list.isEmpty(), is(true));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void addAdditionalConnectionEndpoints() {
		ConnectionEndpointList list = new ConnectionEndpointList();

		assertThat(list.isEmpty(), is(true));

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

		assertThat(list.add(connectionEndpointsArray).add(connectionEndpointsIterable),
			is(sameInstance(list)));

		List<ConnectionEndpoint> expected = new ArrayList<ConnectionEndpoint>(9);

		expected.add(connectionEndpointsArray[0]);
		expected.addAll((List) connectionEndpointsIterable);

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : list) {
			assertThat(connectionEndpoint, is(equalTo(expected.get(index++))));
		}
	}

	@Test
	public void findByHostName() {
		ConnectionEndpointList list = new ConnectionEndpointList(
			newConnectionEndpoint("Earth", 10334),
			newConnectionEndpoint("Earth", 40404),
			newConnectionEndpoint("Mars", 10334),
			newConnectionEndpoint("Jupiter", 1234),
			newConnectionEndpoint("Saturn", 9876),
			newConnectionEndpoint("Neptune", 12345)
		);

		assertThat(list.size(), is(equalTo(6)));

		ConnectionEndpointList result = list.findBy("Earth");

		assertThat(result, is(notNullValue()));
		assertThat(result.size(), is(equalTo(2)));

		String[] expected = { "Earth[10334]", "Earth[40404]" };

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : result) {
			assertThat(connectionEndpoint.toString(), is(equalTo(expected[index++])));
		}

		result = list.findBy("Saturn");

		assertThat(result, is(notNullValue()));
		assertThat(result.isEmpty(), is(false));
		assertThat(result.size(), is(equalTo(1)));
		assertThat(result.iterator().next().toString(), is(equalTo("Saturn[9876]")));

		result = list.findBy("Pluto");

		assertThat(result, is(notNullValue()));
		assertThat(result.isEmpty(), is(true));
		assertThat(result.size(), is(equalTo(0)));
	}

	@Test
	public void findByPortNumber() {
		ConnectionEndpointList list = new ConnectionEndpointList(
			newConnectionEndpoint("Earth", 10334),
			newConnectionEndpoint("Earth", 40404),
			newConnectionEndpoint("Mars", 10334),
			newConnectionEndpoint("Jupiter", 1234),
			newConnectionEndpoint("Saturn", 9876),
			newConnectionEndpoint("Neptune", 12345)
		);

		assertThat(list.size(), is(equalTo(6)));

		ConnectionEndpointList result = list.findBy(10334);

		assertThat(result, is(notNullValue()));
		assertThat(result.size(), is(equalTo(2)));

		String[] expected = { "Earth[10334]", "Mars[10334]" };

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : result) {
			assertThat(connectionEndpoint.toString(), is(equalTo(expected[index++])));
		}

		result = list.findBy(1234);

		assertThat(result, is(notNullValue()));
		assertThat(result.size(), is(equalTo(1)));
		assertThat(result.iterator().next().toString(), is(equalTo("Jupiter[1234]")));

		result = list.findBy(80);

		assertThat(result, is(notNullValue()));
		assertThat(result.isEmpty(), is(true));
	}

	@Test
	public void findOneByHostName() {
		ConnectionEndpointList list = new ConnectionEndpointList();

		assertThat(list.findOne("localhost"), is(nullValue()));

		list.add(newConnectionEndpoint("skullbox", 11235));

		assertThat(list.findOne("localhost"), is(nullValue()));
		assertThat(list.findOne("skullbox"), is(equalTo(newConnectionEndpoint("skullbox", 11235))));

		list.add(newConnectionEndpoint("toolbox", 12480));

		assertThat(list.findOne("boombox"), is(nullValue()));
		assertThat(list.findOne("localhost"), is(nullValue()));
		assertThat(list.findOne("skullbox"), is(equalTo(newConnectionEndpoint("skullbox", 11235))));
		assertThat(list.findOne("toolbox"), is(equalTo(newConnectionEndpoint("toolbox", 12480))));

		list.add(newConnectionEndpoint("skullbox", 10334));

		assertThat(list.findOne("localhost"), is(nullValue()));
		assertThat(list.findOne("boombox"), is(nullValue()));
		assertThat(list.findOne("skullbox"), is(equalTo(newConnectionEndpoint("skullbox", 11235))));
		assertThat(list.findOne("toolbox"), is(equalTo(newConnectionEndpoint("toolbox", 12480))));
	}

	@Test
	public void findOneByPortNumber() {
		ConnectionEndpointList list = new ConnectionEndpointList();

		assertThat(list.findOne(10334), is(nullValue()));

		list.add(newConnectionEndpoint("skullbox", 11235));

		assertThat(list.findOne(10334), is(nullValue()));
		assertThat(list.findOne(11235), is(equalTo(newConnectionEndpoint("skullbox", 11235))));

		list.add(newConnectionEndpoint("toolbox", 12480));

		assertThat(list.findOne(10334), is(nullValue()));
		assertThat(list.findOne(40404), is(nullValue()));
		assertThat(list.findOne(11235), is(equalTo(newConnectionEndpoint("skullbox", 11235))));
		assertThat(list.findOne(12480), is(equalTo(newConnectionEndpoint("toolbox", 12480))));

		list.add(newConnectionEndpoint("boombox", 11235));

		assertThat(list.findOne(10334), is(nullValue()));
		assertThat(list.findOne(40404), is(nullValue()));
		assertThat(list.findOne(11235), is(equalTo(newConnectionEndpoint("skullbox", 11235))));
		assertThat(list.findOne(12480), is(equalTo(newConnectionEndpoint("toolbox", 12480))));
	}

	@Test
	public void toArrayFromList() {
		ConnectionEndpointList list = ConnectionEndpointList.from(
			newConnectionEndpoint("boombox", 11235),
			newConnectionEndpoint("skullbox", 12480),
			newConnectionEndpoint("toolbox", 10334));

		ConnectionEndpoint[] connectionEndpointArray = list.toArray();

		assertThat(connectionEndpointArray, is(notNullValue()));
		assertThat(connectionEndpointArray.length, is(equalTo(list.size())));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : list) {
			assertThat(connectionEndpointArray[index++], is(equalTo(connectionEndpoint)));
		}
	}

	@Test
	public void toArrayFromEmptyList() {
		ConnectionEndpoint[] connectionEndpoints = new ConnectionEndpointList().toArray();

		assertThat(connectionEndpoints, is(notNullValue()));
		assertThat(connectionEndpoints.length, is(equalTo(0)));
	}

	@Test
	public void toInetSocketAddressesFromList() {
		ConnectionEndpointList list = ConnectionEndpointList.from(
			newConnectionEndpoint("localhost", 10334),
			newConnectionEndpoint("localhost", 40404));

		List<InetSocketAddress> socketAddresses = list.toInetSocketAddresses();

		assertThat(socketAddresses, is(notNullValue()));
		assertThat(socketAddresses.size(), is(equalTo(list.size())));

		int index = 0;

		for (ConnectionEndpoint connectionEndpoint : list) {
			assertThat(socketAddresses.get(index).getHostName(), is(equalTo(connectionEndpoint.getHost())));
			assertThat(socketAddresses.get(index++).getPort(), is(equalTo(connectionEndpoint.getPort())));
		}
	}

	@Test
	public void toInetSocketAddressesFromEmptyList() {
		List<InetSocketAddress> socketAddresses = new ConnectionEndpointList().toInetSocketAddresses();

		assertThat(socketAddresses, is(notNullValue()));
		assertThat(socketAddresses.isEmpty(), is(true));
	}

	@Test
	public void toStringFromList() {
		ConnectionEndpointList list = ConnectionEndpointList.parse(10334,
			"skullbox[12480]", "saturn[ 1  12 3  5]", "neptune");

		assertThat(list.toString(), is(equalTo("[skullbox[12480], saturn[11235], neptune[10334]]")));
	}

	@Test
	public void toStringFromEmptyList() {
		assertThat(new ConnectionEndpointList().toString(), is(equalTo("[]")));
	}

}
