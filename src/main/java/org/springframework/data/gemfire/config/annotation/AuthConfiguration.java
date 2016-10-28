/*
 * Copyright 2012 the original author or authors.
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

import java.util.Map;
import java.util.Properties;

import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The AuthConfiguration class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * that applies additional GemFire/Geode configuration by way of GemFire/Geode System properties to configure
 * GemFire/Geode Authentication and Authorization framework services.
 *
 * @author John Blum
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

	/* (non-Javadoc) */
	@Override
	protected Class getAnnotationType() {
		return EnableAuth.class;
	}

	/* (non-Javadoc) */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		gemfireProperties.setProperty(GEMFIRE_SECURITY_PROPERTY_FILE, annotationAttributes.get("securityPropertiesFile"));

		gemfireProperties.setProperty(SECURITY_CLIENT_ACCESSOR, annotationAttributes.get("clientAccessor"));

		gemfireProperties.setProperty(SECURITY_CLIENT_ACCESSOR_POST_PROCESSOR,
			annotationAttributes.get("clientAccessPostOperation"));

		gemfireProperties.setProperty(SECURITY_CLIENT_AUTH_INIT,
			annotationAttributes.get("clientAuthenticationInitializer"));

		gemfireProperties.setProperty(SECURITY_CLIENT_AUTHENTICATOR, annotationAttributes.get("clientAuthenticator"));

		gemfireProperties.setProperty(SECURITY_CLIENT_DIFFIE_HELLMAN_ALGORITHM,
			annotationAttributes.get("clientDiffieHellmanAlgorithm"));

		gemfireProperties.setProperty(SECURITY_PEER_AUTH_INIT,
			annotationAttributes.get("peerAuthenticationInitializer"));

		gemfireProperties.setProperty(SECURITY_PEER_AUTHENTICATOR, annotationAttributes.get("peerAuthenticator"));

		gemfireProperties.setPropertyIfNotDefault(SECURITY_PEER_VERIFY_MEMBER_TIMEOUT,
			annotationAttributes.get("peerVerifyMemberTimeout"), DEFAULT_PEER_VERIFY_MEMBER_TIMEOUT);

		gemfireProperties.setProperty(SECURITY_LOG_FILE, annotationAttributes.get("securityLogFile"));

		gemfireProperties.setPropertyIfNotDefault(SECURITY_LOG_LEVEL,
			annotationAttributes.get("securityLogLevel"), DEFAULT_SECURITY_LOG_LEVEL);

		return gemfireProperties.build();
	}
}
