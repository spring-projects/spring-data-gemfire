/*
 * Copyright 2012-2019 the original author or authors.
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

import java.lang.annotation.Annotation;
import java.util.Map;
import java.util.Properties;

import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The {@link AuthConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that applies
 * additional configuration using Pivotal GemFire/Apache Geode {@link Properties} to configure
 * Pivotal GemFire/Apache Geode Authentication and Authorization framework and services.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.config.annotation.EnableAuth
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @see <a href="Security">http://gemfire.docs.pivotal.io/docs-gemfire/managing/security/chapter_overview.html</a>
 * @since 1.9.0
 */
public class AuthConfiguration extends EmbeddedServiceConfigurationSupport {

	public static final int DEFAULT_PEER_VERIFY_MEMBER_TIMEOUT = 1000;

	public static final String DEFAULT_SECURITY_LOG_LEVEL = "config";

	protected static final String GEMFIRE_SECURITY_PROPERTY_FILE = "gemfireSecurityPropertyFile";
	protected static final String SECURITY_CLIENT_ACCESSOR = "security-client-accessor";
	protected static final String SECURITY_CLIENT_ACCESSOR_POST_PROCESSOR = "security-client-accessor-pp";
	protected static final String SECURITY_CLIENT_AUTH_INIT = "security-client-auth-init";
	protected static final String SECURITY_CLIENT_AUTHENTICATOR = "security-client-authenticator";
	protected static final String SECURITY_CLIENT_DIFFIE_HELLMAN_ALGORITHM = "security-client-dhalgo";
	protected static final String SECURITY_LOG_FILE = "security-log-file";
	protected static final String SECURITY_LOG_LEVEL = "security-log-level";
	protected static final String SECURITY_PEER_AUTH_INIT = "security-peer-auth-init";
	protected static final String SECURITY_PEER_AUTHENTICATOR = "security-peer-authenticator";
	protected static final String SECURITY_PEER_VERIFY_MEMBER_TIMEOUT = "security-peer-verifymember-timeout";

	/**
	 * Returns the {@link EnableAuth} {@link java.lang.annotation.Annotation} {@link Class} type.
	 *
	 * @return the {@link EnableAuth} {@link java.lang.annotation.Annotation} {@link Class} type.
	 * @see org.springframework.data.gemfire.config.annotation.EnableAuth
	 */
	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableAuth.class;
	}

	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {

		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		gemfireProperties.setProperty(GEMFIRE_SECURITY_PROPERTY_FILE,
			resolveProperty(securityProperty("properties-file"),
				(String) annotationAttributes.get("securityPropertiesFile")));

		gemfireProperties.setProperty(SECURITY_CLIENT_ACCESSOR,
			resolveProperty(securityProperty("client.accessor"),
				(String) annotationAttributes.get("clientAccessor")));

		gemfireProperties.setProperty(SECURITY_CLIENT_ACCESSOR_POST_PROCESSOR,
			resolveProperty(securityProperty("client.accessor-post-processor"),
				(String) annotationAttributes.get("clientAccessorPostProcessor")));

		gemfireProperties.setProperty(SECURITY_CLIENT_AUTH_INIT,
			resolveProperty(securityProperty("client.authentication-initializer"),
				(String) annotationAttributes.get("clientAuthenticationInitializer")));

		gemfireProperties.setProperty(SECURITY_CLIENT_AUTHENTICATOR,
			resolveProperty(securityProperty("client.authenticator"),
				(String) annotationAttributes.get("clientAuthenticator")));

		gemfireProperties.setProperty(SECURITY_CLIENT_DIFFIE_HELLMAN_ALGORITHM,
			resolveProperty(securityProperty("client.diffie-hellman-algorithm"),
				(String) annotationAttributes.get("clientDiffieHellmanAlgorithm")));

		gemfireProperties.setProperty(SECURITY_PEER_AUTH_INIT,
			resolveProperty(securityProperty("peer.authentication-initializer"),
				(String) annotationAttributes.get("peerAuthenticationInitializer")));

		gemfireProperties.setProperty(SECURITY_PEER_AUTHENTICATOR,
			resolveProperty(securityProperty("peer.authenticator"),
				(String) annotationAttributes.get("peerAuthenticator")));

		gemfireProperties.setPropertyIfNotDefault(SECURITY_PEER_VERIFY_MEMBER_TIMEOUT,
			resolveProperty(securityProperty("peer.verify-member-timeout"),
				(Long) annotationAttributes.get("peerVerifyMemberTimeout")), DEFAULT_PEER_VERIFY_MEMBER_TIMEOUT);

		gemfireProperties.setProperty(SECURITY_LOG_FILE,
			resolveProperty(securityProperty("log.file"),
				(String) annotationAttributes.get("securityLogFile")));

		gemfireProperties.setPropertyIfNotDefault(SECURITY_LOG_LEVEL,
			resolveProperty(securityProperty("log.level"),
				(String) annotationAttributes.get("securityLogLevel")), DEFAULT_SECURITY_LOG_LEVEL);

		return gemfireProperties.build();
	}
}
