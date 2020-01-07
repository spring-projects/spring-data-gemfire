/*
 * Copyright 2018-2020 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit Tests for {@link NetworkUtils}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.util.NetworkUtils
 * @since 2.2.0
 */
public class NetworkUtilsUnitTests {

	@Test
	public void invalidPortMessageIsCorrect() {
		assertThat(String.format(NetworkUtils.INVALID_PORT_MESSAGE, -1))
			.isEqualTo("Port [-1] must be greater than equal to 0 and less than 65536");
	}

	@Test
	public void invalidNonEphemeralPortMessageIsCorrect() {
		assertThat(String.format(NetworkUtils.INVALID_NO_EPHEMERAL_PORT_MESSAGE, -1))
			.isEqualTo("Port [-1] must be greater than 0 and less than 65536");
	}

	@Test
	public void withValidPortsReturnsTrue() {

		for (int port = 0; port < 65536; port++) {
			assertThat(NetworkUtils.isValidPort(port)).isTrue();
		}
	}

	@Test
	public void withInvalidPortsReturnsFalse() {

		assertThat(NetworkUtils.isValidPort(-21)).isFalse();
		assertThat(NetworkUtils.isValidPort(-1)).isFalse();
		assertThat(NetworkUtils.isValidPort(65536)).isFalse();
		assertThat(NetworkUtils.isValidPort(99199)).isFalse();
	}

	@Test
	public void withValidNonEphemeralPortsReturnsTrue() {

		for (int port = 1; port < 65536; port++) {
			assertThat(NetworkUtils.isValidNonEphemeralPort(port)).isTrue();
		}
	}

	@Test
	public void withInvalidNonEphemeralPortsReturnsFalse() {

		assertThat(NetworkUtils.isValidNonEphemeralPort(-21)).isFalse();
		assertThat(NetworkUtils.isValidNonEphemeralPort(-1)).isFalse();
		assertThat(NetworkUtils.isValidNonEphemeralPort(0)).isFalse();
		assertThat(NetworkUtils.isValidNonEphemeralPort(65536)).isFalse();
		assertThat(NetworkUtils.isValidNonEphemeralPort(99199)).isFalse();
	}
}
