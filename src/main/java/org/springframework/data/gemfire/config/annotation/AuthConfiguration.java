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

	/* (non-Javadoc) */
	@Override
	protected Class getAnnotationType() {
		return EnableAuth.class;
	}

	/* (non-Javadoc) */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		gemfireProperties.setProperty("gemfireSecurityPropertyFile", annotationAttributes.get("securityPropertiesFile"));

		gemfireProperties.setProperty("security-client-accessor", annotationAttributes.get("clientAccessor"));

		gemfireProperties.setProperty("security-client-accessor-pp",
			annotationAttributes.get("clientAccessPostOperation"));

		gemfireProperties.setProperty("security-client-auth-init",
			annotationAttributes.get("clientAuthenticationInitializer"));

		gemfireProperties.setProperty("security-client-authenticator", annotationAttributes.get("clientAuthenticator"));

		gemfireProperties.setProperty("security-client-dhalgo",
			annotationAttributes.get("clientDiffieHellmanAlgorithm"));

		gemfireProperties.setProperty("security-peer-auth-init",
			annotationAttributes.get("peerAuthenticationInitializer"));

		gemfireProperties.setProperty("security-peer-authenticator", annotationAttributes.get("peerAuthenticator"));

		gemfireProperties.setPropertyIfNotDefault("security-peer-verifymember-timeout",
			annotationAttributes.get("peerVerifyMemberTimeout"), DEFAULT_PEER_VERIFY_MEMBER_TIMEOUT);

		gemfireProperties.setProperty("security-log-file", annotationAttributes.get("securityLogFile"));

		gemfireProperties.setPropertyIfNotDefault("security-log-level",
			annotationAttributes.get("securityLogLevel"), DEFAULT_SECURITY_LOG_LEVEL);

		return gemfireProperties.build();
	}
}
