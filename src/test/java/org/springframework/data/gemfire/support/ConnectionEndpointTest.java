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
import static org.hamcrest.number.OrderingComparison.greaterThan;
import static org.hamcrest.number.OrderingComparison.lessThan;
import static org.junit.Assert.assertThat;

import java.net.InetSocketAddress;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The ConnectionEndpointTest class is a test suite of test cases testing the contract and functionality
 * of the ConnectionEndpoint class representing GemFire Socket connection endpoints to GemFire services
 * (such as Locators, etc).
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @since 1.6.3
 */
public class ConnectionEndpointTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Test
	public void fromInetSocketAddress() {
		InetSocketAddress socketAddress = new InetSocketAddress("localhost", 1234);

		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.from(socketAddress);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo(socketAddress.getHostString())));
		assertThat(connectionEndpoint.getPort(), is(equalTo(socketAddress.getPort())));
	}

	@Test
	public void parseUsingDefaultHostAndDefaultPort() {
		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.parse("[]");

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo(ConnectionEndpoint.DEFAULT_HOST)));
		assertThat(connectionEndpoint.getPort(), is(equalTo(ConnectionEndpoint.DEFAULT_PORT)));

		connectionEndpoint = ConnectionEndpoint.parse("[]", 1234);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo(ConnectionEndpoint.DEFAULT_HOST)));
		assertThat(connectionEndpoint.getPort(), is(equalTo(1234)));
	}

	@Test
	public void parseWithHostPort() {
		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.parse("skullbox[12345]", 80);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("skullbox")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(12345)));

		connectionEndpoint = ConnectionEndpoint.parse("localhost[0]", 8080);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("localhost")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(0)));

		connectionEndpoint = ConnectionEndpoint.parse("jambox[1O1O1]", 443);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("jambox")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(111)));
	}

	@Test
	public void parseWithHostUsingDefaultPort() {
		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.parse("mercury[oneTwoThreeFourFive]", 80);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("mercury")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(80)));

		connectionEndpoint = ConnectionEndpoint.parse("venus[OxCAFEBABE]", 443);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("venus")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(443)));

		connectionEndpoint = ConnectionEndpoint.parse("jupiter[#(^$)*!]", 21);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("jupiter")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(21)));

		connectionEndpoint = ConnectionEndpoint.parse("saturn[]", 22);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("saturn")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(22)));

		connectionEndpoint = ConnectionEndpoint.parse("uranis[", 23);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("uranis")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(23)));

		connectionEndpoint = ConnectionEndpoint.parse("neptune", 25);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("neptune")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(25)));

		connectionEndpoint = ConnectionEndpoint.parse("pluto");

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo("pluto")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(ConnectionEndpoint.DEFAULT_PORT)));
	}

	@Test
	public void parseWithPortUsingDefaultHost() {
		ConnectionEndpoint connectionEndpoint = ConnectionEndpoint.parse("[12345]", 80);

		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo(ConnectionEndpoint.DEFAULT_HOST)));
		assertThat(connectionEndpoint.getPort(), is(equalTo(12345)));
	}

	@Test
	public void parseWithBlankHost() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("'hostPort' must be specified");
		ConnectionEndpoint.parse("  ", 12345);
	}

	@Test
	public void parseWithEmptyHost() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("'hostPort' must be specified");
		ConnectionEndpoint.parse("", 12345);
	}

	@Test
	public void parseWithNullHost() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("'hostPort' must be specified");
		ConnectionEndpoint.parse(null, 12345);
	}

	@Test
	public void parseWithInvalidDefaultPort() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("port number [-1248] must be between 0 and 65535");

		ConnectionEndpoint.parse("localhost", -1248);
	}

	@Test
	public void parseDigits() {
		assertThat(ConnectionEndpoint.parseDigits("123456789"), is(equalTo("123456789")));
		assertThat(ConnectionEndpoint.parseDigits("3.14159"), is(equalTo("314159")));
		assertThat(ConnectionEndpoint.parseDigits("-21.5"), is(equalTo("215")));
		assertThat(ConnectionEndpoint.parseDigits("$156.78^#99!"), is(equalTo("1567899")));
		assertThat(ConnectionEndpoint.parseDigits("oneTwoThree"), is(equalTo("")));
		assertThat(ConnectionEndpoint.parseDigits("  "), is(equalTo("")));
		assertThat(ConnectionEndpoint.parseDigits(""), is(equalTo("")));
		assertThat(ConnectionEndpoint.parseDigits(null), is(equalTo("")));
	}

	@Test
	public void parsePort() {
		assertThat(ConnectionEndpoint.parsePort("12345", 80), is(equalTo(12345)));
		assertThat(ConnectionEndpoint.parsePort("zero", 80), is(equalTo(80)));
	}

	@Test
	public void constructConnectionEndpoint() {
		ConnectionEndpoint connectionEndpoint = new ConnectionEndpoint("skullbox", 12345);

		assertThat(connectionEndpoint.getHost(), is(equalTo("skullbox")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(12345)));
		assertThat(connectionEndpoint.toString(), is(equalTo("skullbox[12345]")));
	}

	@Test
	public void constructConnectionEndpointWithDefaultHost() {
		ConnectionEndpoint connectionEndpoint = new ConnectionEndpoint(" ", 12345);

		assertThat(connectionEndpoint.getHost(), is(equalTo(ConnectionEndpoint.DEFAULT_HOST)));
		assertThat(connectionEndpoint.getPort(), is(equalTo(12345)));
	}

	@Test
	public void constructConnectionEndpointWithInvalidPort() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("port number [-1] must be between 0 and 65535");

		new ConnectionEndpoint("localhost", -1);
	}

	@Test
	public void cloneConnectionEndpoint() throws CloneNotSupportedException {
		ConnectionEndpoint originalConnectionEndpoint = new ConnectionEndpoint("skullbox", 12345);

		assertThat(originalConnectionEndpoint.getHost(), is(equalTo("skullbox")));
		assertThat(originalConnectionEndpoint.getPort(), is(equalTo(12345)));

		ConnectionEndpoint clonedConnectionEndpoint = (ConnectionEndpoint) originalConnectionEndpoint.clone();

		assertThat(clonedConnectionEndpoint, is(notNullValue()));
		assertThat(clonedConnectionEndpoint, is(equalTo(originalConnectionEndpoint)));
	}

	@Test
	public void compareConnectionEndpoints() {
		ConnectionEndpoint connectionEndpointOne = new ConnectionEndpoint("localhost", 10334);
		ConnectionEndpoint connectionEndpointTwo = new ConnectionEndpoint("localhost", 40404);
		ConnectionEndpoint connectionEndpointThree = new ConnectionEndpoint("skullbox", 11235);

		assertThat(connectionEndpointOne.compareTo(connectionEndpointOne), is(equalTo(0)));
		assertThat(connectionEndpointOne.compareTo(connectionEndpointTwo), is(lessThan(0)));
		assertThat(connectionEndpointOne.compareTo(connectionEndpointThree), is(lessThan(0)));
		assertThat(connectionEndpointTwo.compareTo(connectionEndpointOne), is(greaterThan(0)));
		assertThat(connectionEndpointTwo.compareTo(connectionEndpointTwo), is(equalTo(0)));
		assertThat(connectionEndpointTwo.compareTo(connectionEndpointThree), is(lessThan(0)));
		assertThat(connectionEndpointThree.compareTo(connectionEndpointOne), is(greaterThan(0)));
		assertThat(connectionEndpointThree.compareTo(connectionEndpointTwo), is(greaterThan(0)));
		assertThat(connectionEndpointThree.compareTo(connectionEndpointThree), is(equalTo(0)));
	}

	@Test
	public void toInetSocketAddressEqualsHostPort() {
		ConnectionEndpoint connectionEndpoint = new ConnectionEndpoint("localhost", 12345);

		assertThat(connectionEndpoint.getHost(), is(equalTo("localhost")));
		assertThat(connectionEndpoint.getPort(), is(equalTo(12345)));

		InetSocketAddress socketAddress = connectionEndpoint.toInetSocketAddress();

		assertThat(socketAddress, is(notNullValue()));
		assertThat(socketAddress.getHostName(), is(equalTo(connectionEndpoint.getHost())));
		assertThat(socketAddress.getPort(), is(equalTo(connectionEndpoint.getPort())));
	}
}
