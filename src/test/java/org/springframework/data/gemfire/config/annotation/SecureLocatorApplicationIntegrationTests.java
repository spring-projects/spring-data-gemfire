/*
 * Copyright 2020 the original author or authors.
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
package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Properties;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.apache.geode.distributed.ConfigurationProperties;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.distributed.Locator;
import org.apache.geode.security.AuthenticationFailedException;
import org.apache.geode.security.ResourcePermission;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests testing the configuration of a "secure" Apache Geode Locator.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.apache.geode.distributed.ConfigurationProperties
 * @see org.apache.geode.distributed.Locator
 * @see org.apache.geode.distributed.DistributedSystem
 * @see org.springframework.data.gemfire.config.annotation.LocatorApplication
 * @see org.springframework.data.gemfire.config.annotation.EnableSecurity
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.3.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class SecureLocatorApplicationIntegrationTests {

	@Autowired
	private Locator locator;

	@BeforeClass
	public static void setup() {
		System.setProperty(ApacheShiroSecurityConfiguration.ApacheShiroPresentCondition.SPRING_DATA_GEMFIRE_SECURITY_SHIRO_ENABLED,
			Boolean.FALSE.toString());
	}

	@AfterClass
	public static void tearDown() {
		System.clearProperty(ApacheShiroSecurityConfiguration.ApacheShiroPresentCondition.SPRING_DATA_GEMFIRE_SECURITY_SHIRO_ENABLED);
	}

	@Test
	public void locatorIsSecure() {

		assertThat(this.locator).isNotNull();

		DistributedSystem distributedSystem = this.locator.getDistributedSystem();

		assertThat(distributedSystem).isNotNull();
		assertThat(distributedSystem.getProperties()).isNotNull();
		assertThat(distributedSystem.getProperties().getProperty(ConfigurationProperties.SECURITY_MANAGER))
			.isEqualTo(TestSecurityManager.class.getName());
	}

	@LocatorApplication(port = 0)
	//@EnableSecurity(securityManagerClass = TestSecurityManager.class)
	@EnableSecurity(securityManagerClassName = "org.springframework.data.gemfire.config.annotation.TestSecurityManager")
	static class TestConfiguration { }

	static final class MockSecurityManager implements org.apache.geode.security.SecurityManager {

		@Override
		public Object authenticate(Properties credentials) throws AuthenticationFailedException {
			throw new AuthenticationFailedException("Identity could not be verified");
		}

		@Override
		public boolean authorize(Object principal, ResourcePermission permission) {
			return false;
		}
	}
}
