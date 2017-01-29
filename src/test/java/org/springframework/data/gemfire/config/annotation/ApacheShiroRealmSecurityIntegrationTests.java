/*
 * Copyright 2016-2018 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import java.io.IOException;

import org.apache.geode.internal.security.shiro.GeodePermissionResolver;
import org.apache.shiro.realm.text.PropertiesRealm;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for Apache Geode Integrated Security using an Apache Shiro Realm security configuration
 * as a Spring managed bean in a Spring {@link org.springframework.context.ApplicationContext}.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.AbstractGeodeSecurityIntegrationTests
 * @since 1.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = AbstractGeodeSecurityIntegrationTests.GeodeClientConfiguration.class)
@ActiveProfiles("apache-geode-client")
public class ApacheShiroRealmSecurityIntegrationTests extends AbstractGeodeSecurityIntegrationTests {

	protected static final String SHIRO_REALM_CONFIGURATION_PROFILE = "shiro-realm-configuration";

	@BeforeClass
	public static void setup() throws IOException {
		runGeodeServer(SHIRO_REALM_CONFIGURATION_PROFILE);
	}

	@Configuration
	@EnableSecurity
	@Profile(SHIRO_REALM_CONFIGURATION_PROFILE)
	@SuppressWarnings("unused")
	public static class ApacheShiroRealmConfiguration {

		@Bean
		public PropertiesRealm shiroRealm() {

			PropertiesRealm propertiesRealm = new PropertiesRealm();

			propertiesRealm.setResourcePath("classpath:shiro.properties");
			propertiesRealm.setPermissionResolver(new GeodePermissionResolver());

			return propertiesRealm;
		}
	}
}
