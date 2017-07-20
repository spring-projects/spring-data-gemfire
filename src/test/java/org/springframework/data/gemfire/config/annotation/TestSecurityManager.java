/*
 * Copyright 2017 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.annotation;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.security.Principal;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.geode.security.AuthenticationFailedException;
import org.springframework.util.ObjectUtils;

/**
 * The {@link TestSecurityManager} class is an Apache Geode / Pivotal GemFire
 * {@link org.apache.geode.security.SecurityManager} implementation used for testing purposes.
 *
 * @author John Blum
 * @see java.security.Principal
 * @see java.util.Properties
 * @see org.apache.geode.security.SecurityManager
 * @since 2.0.0
 */
public final class TestSecurityManager implements org.apache.geode.security.SecurityManager {

	public static final String SECURITY_USERNAME_PROPERTY = "security-username";
	public static final String SECURITY_PASSWORD_PROPERTY = "security-password";

	public static final String SECURITY_USERNAME = "testUser";
	public static final String SECURITY_PASSWORD = "testP@55w0rd";

	private final ConcurrentMap<String, String> authorizedUsers;

	public TestSecurityManager() {
		this.authorizedUsers = new ConcurrentHashMap<>();
		this.authorizedUsers.putIfAbsent(SECURITY_USERNAME, SECURITY_PASSWORD);
	}

	@Override
	public Object authenticate(Properties properties) throws AuthenticationFailedException {

		String username = properties.getProperty(SECURITY_USERNAME_PROPERTY);
		String password = properties.getProperty(SECURITY_PASSWORD_PROPERTY);

		return validateIdentified(isIdentified(username, password) ? newPrincipal(username) : null, username);
	}

	private boolean isIdentified(String username, String password) {
		return ObjectUtils.nullSafeEquals(this.authorizedUsers.get(String.valueOf(username)), password);
	}

	private Principal newPrincipal(String username) {

		Principal mockPrincipal = mock(Principal.class, username);

		when(mockPrincipal.getName()).thenReturn(username);

		return mockPrincipal;
	}

	private Principal validateIdentified(Principal principal, String username) {

		if (principal == null) {
			throw new AuthenticationFailedException(String.format("User [%s] is not valid", username));
		}

		return principal;
	}
}
