/*
 * Copyright 2016-2020 the original author or authors.
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
 *
 */
package org.springframework.data.gemfire.config.annotation;

import static org.apache.geode.security.ResourcePermission.Operation;
import static org.apache.geode.security.ResourcePermission.Resource;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.apache.geode.security.AuthenticationFailedException;
import org.apache.geode.security.ResourcePermission;

import org.junit.BeforeClass;
import org.junit.runner.RunWith;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for Apache Geode Integrated Security using an application-specific, Apache Geode
 * {@link org.apache.geode.security.SecurityManager}.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.AbstractGeodeSecurityIntegrationTests
 * @since 1.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = AbstractGeodeSecurityIntegrationTests.GeodeClientConfiguration.class)
@ActiveProfiles("apache-geode-client")
public class ApacheGeodeSecurityManagerSecurityIntegrationTests extends AbstractGeodeSecurityIntegrationTests {

	protected static final String GEODE_SECURITY_MANAGER_PROPERTY_CONFIGURATION_PROFILE =
		"geode-security-manager-property-configuration";

	@BeforeClass
	public static void setup() throws IOException {
		runGeodeServer(GEODE_SECURITY_MANAGER_PROPERTY_CONFIGURATION_PROFILE);
	}

	@Configuration
	@EnableSecurity(securityManagerClassName =
		"org.springframework.data.gemfire.config.annotation.ApacheGeodeSecurityManagerSecurityIntegrationTests$TestGeodeSecurityManager")
	@Profile(GEODE_SECURITY_MANAGER_PROPERTY_CONFIGURATION_PROFILE)
	public static class ApacheGeodeSecurityManagerConfiguration {
	}

	protected interface GeodeSecurityRepository {

		ResourcePermission CLUSTER_MANAGE = new ResourcePermission(Resource.CLUSTER, Operation.MANAGE);
		ResourcePermission CLUSTER_READ = new ResourcePermission(Resource.CLUSTER, Operation.READ);
		ResourcePermission CLUSTER_WRITE = new ResourcePermission(Resource.CLUSTER, Operation.WRITE);

		ResourcePermission DATA_MANAGE = new ResourcePermission(Resource.DATA, Operation.MANAGE);
		ResourcePermission DATA_READ = new ResourcePermission(Resource.DATA, Operation.READ);
		ResourcePermission DATA_WRITE = new ResourcePermission(Resource.DATA, Operation.WRITE);

		Role ADMIN = Role.newRole("ADMIN").with(CLUSTER_MANAGE, CLUSTER_READ, CLUSTER_WRITE);
		Role DBA = Role.newRole("DBA").with(DATA_MANAGE, DATA_READ, DATA_WRITE);
		Role DATA_SCIENTIST = Role.newRole("DATA_SCIENTIST").with(DATA_READ, DATA_WRITE);
		Role DATA_ANALYST = Role.newRole("DATA_ANALYST").with(DATA_READ);
		Role GUEST = Role.newRole("GUEST");

		User root = User.newUser("root").with("s3cr3t!").with(ADMIN, DBA);
		User scientist = User.newUser("scientist").with("w0rk!ng4u").with(DATA_SCIENTIST);
		User analyst = User.newUser("analyst").with("p@55w0rd").with(DATA_ANALYST);
		User guest = User.newUser("guest").with("guest").with(GUEST);

		Set<User> users = new HashSet<>(Arrays.asList(root, scientist, analyst, guest));

		default User findBy(String username) {
			return users.stream().filter((user) -> user.getName().equals(username)).findFirst().orElse(null);
		}
	}

	@SuppressWarnings("unused")
	public static class TestGeodeSecurityManager implements org.apache.geode.security.SecurityManager {

		private final GeodeSecurityRepository securityRepository;

		public TestGeodeSecurityManager() {
			this.securityRepository = new GeodeSecurityRepository() { };
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public Object authenticate(Properties credentials) throws AuthenticationFailedException {

			String username = credentials.getProperty(SECURITY_USERNAME_PROPERTY);
			String password = credentials.getProperty(SECURITY_PASSWORD_PROPERTY);

			User user = securityRepository.findBy(username);

			if (user == null || !user.getCredentials().equals(password)) {
				throw new AuthenticationFailedException(String.format("Failed to authenticate user [%s]", username));
			}

			return user;
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public boolean authorize(Object principal, ResourcePermission permission) {
			return (principal instanceof User && ((User) principal).hasPermission(permission));
		}
	}
}
