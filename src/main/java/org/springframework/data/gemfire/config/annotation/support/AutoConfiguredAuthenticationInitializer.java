/*
 * Copyright 2017-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation.support;

import java.util.Properties;

import org.apache.geode.security.AuthInitialize;
import org.apache.shiro.util.StringUtils;

/**
 * The {@link AutoConfiguredAuthenticationInitializer} class is an {@link AuthInitialize} implementation,
 * which auto-configures security, and specifically authentication, for Apache Geode/Pivotal GemFire.
 *
 * @author John Blum
 * @see org.apache.geode.security.AuthInitialize
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class AutoConfiguredAuthenticationInitializer extends AbstractAuthInitialize {

	public static final String SDG_SECURITY_USERNAME_PROPERTY = "spring.data.gemfire.security.username";
	public static final String SDG_SECURITY_PASSWORD_PROPERTY = "spring.data.gemfire.security.password";

	public static final String SECURITY_USERNAME_PROPERTY = "security-username";
	public static final String SECURITY_PASSWORD_PROPERTY = "security-password";

	protected static final Properties NO_PARAMETERS = new Properties();

	/**
	 * Factory method used to construct a new instance of {@link AutoConfiguredAuthenticationInitializer}.
	 *
	 * @return a new instance of {@link AutoConfiguredAuthenticationInitializer}.
	 * @see org.springframework.data.gemfire.config.annotation.support.AutoConfiguredAuthenticationInitializer
	 */
	public static AutoConfiguredAuthenticationInitializer newAuthenticationInitializer() {

		AutoConfiguredAuthenticationInitializer authenticationInitializer =
			new AutoConfiguredAuthenticationInitializer();

		authenticationInitializer.init(NO_PARAMETERS);

		return authenticationInitializer;
	}

	@Override
	protected Properties doGetCredentials(Properties properties) {

		getEnvironment()
			.filter(environment -> StringUtils.hasText(environment.getProperty(SDG_SECURITY_USERNAME_PROPERTY)))
			.ifPresent(environment -> {

				String securityUsername = environment.getProperty(SDG_SECURITY_USERNAME_PROPERTY);
				String securityPassword = environment.getProperty(SDG_SECURITY_PASSWORD_PROPERTY);

				properties.setProperty(SECURITY_USERNAME_PROPERTY, securityUsername);
				properties.setProperty(SECURITY_PASSWORD_PROPERTY, securityPassword);

			});

		return properties;
	}
}
