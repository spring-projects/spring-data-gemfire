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

/**
 * The EmbeddedManagerConfiguration class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * that applies additional GemFire configuration by way of GemFire System properties to configure
 * an embedded GemFire Manager.
 *
 * @author John Blum
 * @see EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class EmbeddedManagerConfiguration extends EmbeddedServiceConfigurationSupport {

	@Override
	protected Class getAnnotationType() {
		return EnableEmbeddedManager.class;
	}

	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		Properties gemfireProperties = new Properties();

		boolean jmxManager = Boolean.valueOf(String.valueOf(annotationAttributes.get("jmxManager")));

		if (jmxManager) {
			setProperty(gemfireProperties, "jmx-manager", Boolean.TRUE.toString());
			setProperty(gemfireProperties, "jmx-manager-access-file", annotationAttributes.get("jmxManagerAccessFile"));
			setProperty(gemfireProperties, "jmx-manager-bind-address", annotationAttributes.get("jmxManagerBindAddress"));
			setProperty(gemfireProperties, "jmx-manager-hostname-for-clients", annotationAttributes.get("jmxManagerHostnameForClients"));
			setProperty(gemfireProperties, "jmx-manager-password-file", annotationAttributes.get("jmxManagerPasswordFile"));
			setProperty(gemfireProperties, "jmx-manager-port", annotationAttributes.get("jmxManagerPort"));
			setProperty(gemfireProperties, "jmx-manager-start", annotationAttributes.get("jmxManagerStart"));
			setProperty(gemfireProperties, "jmx-manager-update-rate", annotationAttributes.get("jmxManagerUpdateRate"));

			boolean jmxManagerSslEnabled = Boolean.valueOf(String.valueOf(annotationAttributes.get("jmxManagerSslEnabled")));

			if (jmxManagerSslEnabled) {
				setProperty(gemfireProperties, "jmx-manager-ssl-enabled", Boolean.TRUE.toString());
				setProperty(gemfireProperties, "jmx-manager-ssl-ciphers", annotationAttributes.get("jmxManagerSslCiphers"));
				setProperty(gemfireProperties, "jmx-manager-ssl-protocols", annotationAttributes.get("jmxManagerSslProtocols"));
				setProperty(gemfireProperties, "jmx-manager-ssl-require-authentication", annotationAttributes.get("jmxManagerSslRequireAuthentication"));
			}
		}

		return gemfireProperties;
	}
}
