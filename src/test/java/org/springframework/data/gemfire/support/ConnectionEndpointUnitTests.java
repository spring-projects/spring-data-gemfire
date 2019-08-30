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

import static org.assertj.core.api.Assertions.assertThat;

import java.net.InetSocketAddress;

import org.junit.Test;

/**
 * Unit Tests for {@link ConnectionEndpoint}.
 *
 * @author John Blum
 * @see java.net.InetSocketAddress
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @since 1.6.3
 */
public class ConnectionEndpointUnitTests {

	@Test
	public void fromInetSocketAddress() {

		InetSocketAddress socketAddress = new InetSocketAddress("localhost", 1234);

		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.from(socketAddress);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo(socketAddress.getHostString());
		assertThat(connectionEndpoint.getPort()).isEqualTo(socketAddress.getPort());
	}

	@Test
	public void parseUsingDefaultHostAndDefaultPort() {

		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.parse("[]");

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo(ConnectionEndpoint.DEFAULT_HOST);
		assertThat(connectionEndpoint.getPort()).isEqualTo(ConnectionEndpoint.DEFAULT_PORT);

		connectionEndpoint = ConnectionEndpoint.parse("[]", 1234);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo(ConnectionEndpoint.DEFAULT_HOST);
		assertThat(connectionEndpoint.getPort()).isEqualTo(1234);
	}

	@Test
	public void parseWithHostPort() {

		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.parse("skullbox[12345]", 80);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("skullbox");
		assertThat(connectionEndpoint.getPort()).isEqualTo(12345);

		connectionEndpoint = ConnectionEndpoint.parse("localhost[0]", 8080);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("localhost");
		assertThat(connectionEndpoint.getPort()).isEqualTo(0);

		connectionEndpoint = ConnectionEndpoint.parse("jambox[1O1O1]", 443);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("jambox");
		assertThat(connectionEndpoint.getPort()).isEqualTo(111);
	}

	@Test
	public void parseWithHostPortHavingSpacing() {

		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.parse(" saturn  [1  23 4 ]");

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("saturn");
		assertThat(connectionEndpoint.getPort()).isEqualTo(1234);
	}

	@Test
	public void parseWithHostUsingDefaultPort() {

		ConnectionEndpoint connectionEndpoint =
			ConnectionEndpoint.parse("mercury[oneTwoThreeFourFive]", 80);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("mercury");
		assertThat(connectionEndpoint.getPort()).isEqualTo(80);

		connectionEndpoint = ConnectionEndpoint.parse("venus[OxCAFEBABE]", 443);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("venus");
		assertThat(connectionEndpoint.getPort()).isEqualTo(443);

		connectionEndpoint = ConnectionEndpoint.parse("jupiter[#(^$)*!]", 21);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("jupiter");
		assertThat(connectionEndpoint.getPort()).isEqualTo(21);

		connectionEndpoint = ConnectionEndpoint.parse("saturn[]", 22);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("saturn");
		assertThat(connectionEndpoint.getPort()).isEqualTo(22);

		connectionEndpoint = ConnectionEndpoint.parse("uranis[", 23);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("uranis");
		assertThat(connectionEndpoint.getPort()).isEqualTo(23);

		connectionEndpoint = ConnectionEndpoint.parse("neptune", 25);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("neptune");
		assertThat(connectionEndpoint.getPort()).isEqualTo(25);

		connectionEndpoint = ConnectionEndpoint.parse("pluto");

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo("pluto");
		assertThat(connectionEndpoint.getPort()).isEqualTo(ConnectionEndpoint.DEFAULT_PORT);
	}

	@Test
	public void parseWithPortUsingDefaultHost() {

		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.parse("[12345]", 80);

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo(ConnectionEndpoint.DEFAULT_HOST);
		assertThat(connectionEndpoint.getPort()).isEqualTo(12345);
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseWithBlankHost() {

		try {
			ConnectionEndpoint.parse("  ", 12345);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("'hostPort' must be specified");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseWithEmptyHost() {

		try {
			ConnectionEndpoint.parse("", 12345);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("'hostPort' must be specified");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseWithNullHost() {

		try {
			ConnectionEndpoint.parse(null, 12345);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("'hostPort' must be specified");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseWithInvalidDefaultPort() {

		try {
			ConnectionEndpoint.parse("localhost", -1248);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("port number [-1248] must be between 0 and 65535");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void parseDigits() {

		assertThat(ConnectionEndpoint.parseDigits("123456789")).isEqualTo("123456789");
		assertThat(ConnectionEndpoint.parseDigits("3.14159")).isEqualTo("314159");
		assertThat(ConnectionEndpoint.parseDigits("-21.5")).isEqualTo("215");
		assertThat(ConnectionEndpoint.parseDigits("$156.78^#99!")).isEqualTo("1567899");
		assertThat(ConnectionEndpoint.parseDigits("oneTwoThree")).isEqualTo("");
		assertThat(ConnectionEndpoint.parseDigits("  ")).isEqualTo("");
		assertThat(ConnectionEndpoint.parseDigits("")).isEqualTo("");
		assertThat(ConnectionEndpoint.parseDigits(null)).isEqualTo("");
	}

	@Test
	public void parsePort() {

		assertThat(ConnectionEndpoint.parsePort("12345", 80)).isEqualTo(12345);
		assertThat(ConnectionEndpoint.parsePort("zero", 80)).isEqualTo(80);
	}

	@Test
	public void constructConnectionEndpoint() {

		ConnectionEndpoint connectionEndpoint = new ConnectionEndpoint("skullbox", 12345);

		assertThat(connectionEndpoint.getHost()).isEqualTo("skullbox");
		assertThat(connectionEndpoint.getPort()).isEqualTo(12345);
		assertThat(connectionEndpoint.toString()).isEqualTo("skullbox[12345]");
	}

	@Test
	public void constructConnectionEndpointWithDefaultHost() {

		ConnectionEndpoint connectionEndpoint = new ConnectionEndpoint(" ", 12345);

		assertThat(connectionEndpoint.getHost()).isEqualTo(ConnectionEndpoint.DEFAULT_HOST);
		assertThat(connectionEndpoint.getPort()).isEqualTo(12345);
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructConnectionEndpointWithInvalidPort() {

		try {
			new ConnectionEndpoint("localhost", -1);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("port number [-1] must be between 0 and 65535");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void cloneConnectionEndpoint() throws CloneNotSupportedException {

		ConnectionEndpoint originalConnectionEndpoint = new ConnectionEndpoint("skullbox", 12345);

		assertThat(originalConnectionEndpoint.getHost()).isEqualTo("skullbox");
		assertThat(originalConnectionEndpoint.getPort()).isEqualTo(12345);

		ConnectionEndpoint clonedConnectionEndpoint = (ConnectionEndpoint) originalConnectionEndpoint.clone();

		assertThat(clonedConnectionEndpoint).isNotNull();
		assertThat(clonedConnectionEndpoint).isEqualTo(originalConnectionEndpoint);
	}

	@Test
	@SuppressWarnings("all")
	public void compareConnectionEndpoints() {

		ConnectionEndpoint connectionEndpointOne = new ConnectionEndpoint("localhost", 10334);
		ConnectionEndpoint connectionEndpointTwo = new ConnectionEndpoint("localhost", 40404);
		ConnectionEndpoint connectionEndpointThree = new ConnectionEndpoint("skullbox", 11235);

		assertThat(connectionEndpointOne.compareTo(connectionEndpointOne)).isEqualTo(0);
		assertThat(connectionEndpointOne.compareTo(connectionEndpointTwo)).isLessThan(0);
		assertThat(connectionEndpointOne.compareTo(connectionEndpointThree)).isLessThan(0);
		assertThat(connectionEndpointTwo.compareTo(connectionEndpointOne)).isGreaterThan(0);
		assertThat(connectionEndpointTwo.compareTo(connectionEndpointTwo)).isEqualTo(0);
		assertThat(connectionEndpointTwo.compareTo(connectionEndpointThree)).isLessThan(0);
		assertThat(connectionEndpointThree.compareTo(connectionEndpointOne)).isGreaterThan(0);
		assertThat(connectionEndpointThree.compareTo(connectionEndpointTwo)).isGreaterThan(0);
		assertThat(connectionEndpointThree.compareTo(connectionEndpointThree)).isEqualTo(0);
	}

	@Test
	public void toInetSocketAddressEqualsHostPort() {

		ConnectionEndpoint connectionEndpoint = new ConnectionEndpoint("localhost", 12345);

		assertThat(connectionEndpoint.getHost()).isEqualTo("localhost");
		assertThat(connectionEndpoint.getPort()).isEqualTo(12345);

		InetSocketAddress socketAddress = connectionEndpoint.toInetSocketAddress();

		assertThat(socketAddress).isNotNull();
		assertThat(socketAddress.getHostName()).isEqualTo(connectionEndpoint.getHost());
		assertThat(socketAddress.getPort()).isEqualTo(connectionEndpoint.getPort());
	}
}
