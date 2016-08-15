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
 * The ManagerConfiguration class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * that applies additional GemFire configuration by way of GemFire System properties to configure
 * an embedded GemFire Manager.
 *
 * @author John Blum
 * @see EnableManager
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class ManagerConfiguration extends EmbeddedServiceConfigurationSupport {

	protected static final int DEFAULT_JMX_MANAGER_PORT = 1099;

	@Override
	protected Class getAnnotationType() {
		return EnableManager.class;
	}

	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		PropertiesBuilder gemfireProperties = new PropertiesBuilder();

		boolean jmxManager = Boolean.valueOf(String.valueOf(annotationAttributes.get("jmxManager")));

		if (jmxManager) {
			gemfireProperties.setProperty("jmx-manager", Boolean.TRUE.toString());
			gemfireProperties.setProperty("jmx-manager-access-file", annotationAttributes.get("jmxManagerAccessFile"));
			gemfireProperties.setProperty("jmx-manager-bind-address", annotationAttributes.get("jmxManagerBindAddress"));
			gemfireProperties.setProperty("jmx-manager-hostname-for-clients", annotationAttributes.get("jmxManagerHostnameForClients"));
			gemfireProperties.setProperty("jmx-manager-password-file", annotationAttributes.get("jmxManagerPasswordFile"));
			gemfireProperties.setProperty("jmx-manager-port", resolvePort((Integer) annotationAttributes.get("jmxManagerPort"), DEFAULT_JMX_MANAGER_PORT));
			gemfireProperties.setProperty("jmx-manager-start", annotationAttributes.get("jmxManagerStart"));
			gemfireProperties.setProperty("jmx-manager-update-rate", annotationAttributes.get("jmxManagerUpdateRate"));

			boolean jmxManagerSslEnabled = Boolean.valueOf(String.valueOf(annotationAttributes.get("jmxManagerSslEnabled")));

			if (jmxManagerSslEnabled) {
				gemfireProperties.setProperty("jmx-manager-ssl-enabled", Boolean.TRUE.toString());
				gemfireProperties.setProperty("jmx-manager-ssl-ciphers", annotationAttributes.get("jmxManagerSslCiphers"));
				gemfireProperties.setProperty("jmx-manager-ssl-protocols", annotationAttributes.get("jmxManagerSslProtocols"));
				gemfireProperties.setProperty("jmx-manager-ssl-require-authentication", annotationAttributes.get("jmxManagerSslRequireAuthentication"));
			}
		}

		return gemfireProperties.build();
	}
}
