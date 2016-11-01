/*
 * Copyright 2016 the original author or authors.
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
 * The {@link GeodeIntegratedSecurityConfiguration} class is a {@link EmbeddedServiceConfigurationSupport} implementation
 * that enables Apache Geode's Integrated Security framework and services.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class GeodeIntegratedSecurityConfiguration extends EmbeddedServiceConfigurationSupport {

	protected static final String SECURITY_CLIENT_AUTH_INIT = "security-client-auth-init";
	protected static final String SECURITY_CLIENT_AUTHENTICATOR = "security-client-authenticator";
	protected static final String SECURITY_MANAGER = "security-manager";
	protected static final String SECURITY_PEER_AUTH_INIT = "security-peer-auth-init";
	protected static final String SECURITY_PEER_AUTHENTICATOR = "security-peer-authenticator";
	protected static final String SECURITY_POST_PROCESSOR = "security-post-processor";
	protected static final String SECURITY_SHIRO_INIT = "security-shiro-init";

	/**
	 * @inheritDoc
	 */
	@Override
	protected Class getAnnotationType() {
		return EnableSecurity.class;
	}

	/* (non-Javadoc) */
	protected boolean isShiroSecurityConfigured() {
		try {
			// NOTE experimental...
			//return resolveBean(ApacheShiroSecurityConfiguration.class).isRealmsPresent();
			return false;
		}
		catch (Exception ignore) {
			return false;
		}
	}

	/* (non-Javadoc) */
	protected boolean isShiroSecurityNotConfigured() {
		return !isShiroSecurityConfigured();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		PropertiesBuilder gemfireProperties = new PropertiesBuilder();

		gemfireProperties.setProperty(SECURITY_CLIENT_AUTH_INIT,
			annotationAttributes.get("clientAuthenticationInitializer"));

		gemfireProperties.setProperty(SECURITY_CLIENT_AUTHENTICATOR, annotationAttributes.get("clientAuthenticator"));

		if (isShiroSecurityNotConfigured()) {
			gemfireProperties.setPropertyIfNotDefault(SECURITY_MANAGER,
				annotationAttributes.get("securityManagerClass"), Void.class);

			gemfireProperties.setProperty(SECURITY_MANAGER, annotationAttributes.get("securityManagerClassName"));

			gemfireProperties.setProperty(SECURITY_SHIRO_INIT, annotationAttributes.get("shiroIniResourcePath"));
		}

		gemfireProperties.setProperty(SECURITY_PEER_AUTH_INIT,
			annotationAttributes.get("peerAuthenticationInitializer"));

		gemfireProperties.setProperty(SECURITY_PEER_AUTHENTICATOR, annotationAttributes.get("peerAuthenticator"));

		gemfireProperties.setPropertyIfNotDefault(SECURITY_POST_PROCESSOR,
			annotationAttributes.get("securityPostProcessorClass"), Void.class);

		gemfireProperties.setProperty(SECURITY_POST_PROCESSOR,
			annotationAttributes.get("securityPostProcessorClassName"));

		return gemfireProperties.build();
	}
}
