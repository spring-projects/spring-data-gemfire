/*
 * Copyright 2012-2018 the original author or authors.
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

import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The {@link ManagerConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that applies
 * additional configuration using Pivotal GemFire/Apache Geode {@link Properties} to configure an embedded Manager
 * in this cluster member.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.config.annotation.EnableManager
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class ManagerConfiguration extends EmbeddedServiceConfigurationSupport {

	protected static final int DEFAULT_JMX_MANAGER_PORT = 1099;

	/**
	 * Returns the {@link EnableManager} {@link java.lang.annotation.Annotation} {@link Class} type.
	 *
	 * @return the {@link EnableManager} {@link java.lang.annotation.Annotation} {@link Class} type.
	 * @see org.springframework.data.gemfire.config.annotation.EnableManager
	 */
	@Override
	protected Class getAnnotationType() {
		return EnableManager.class;
	}

	/* (non-Javadoc) */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {

		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		gemfireProperties.setProperty("jmx-manager",
			resolveProperty(managerProperty("enabled"), Boolean.TRUE));

		gemfireProperties.setProperty("jmx-manager-access-file",
			resolveProperty(managerProperty("access-file"),
				(String) annotationAttributes.get("accessFile")));

		gemfireProperties.setProperty("jmx-manager-bind-address",
			resolveProperty(managerProperty("bind-address"),
				(String) annotationAttributes.get("bindAddress")));

		gemfireProperties.setProperty("jmx-manager-hostname-for-clients",
			resolveProperty(managerProperty("hostname-for-clients"),
				(String) annotationAttributes.get("hostnameForClients")));

		gemfireProperties.setProperty("jmx-manager-password-file",
			resolveProperty(managerProperty("password-file"),
				(String) annotationAttributes.get("passwordFile")));

		gemfireProperties.setProperty("jmx-manager-port",
			resolvePort(resolveProperty(managerProperty("port"),
				(Integer) annotationAttributes.get("port")), DEFAULT_JMX_MANAGER_PORT));

		gemfireProperties.setProperty("jmx-manager-start",
			resolveProperty(managerProperty("start"), (Boolean) annotationAttributes.get("start")));

		gemfireProperties.setProperty("jmx-manager-update-rate",
			resolveProperty(managerProperty("update-rate"),
				(Integer) annotationAttributes.get("updateRate")));

		return gemfireProperties.build();
	}
}
