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
 * that applies additional GemFire configuration using GemFire System {@link Properties} to configure
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
		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		gemfireProperties.setProperty("jmx-manager", Boolean.TRUE.toString());
		gemfireProperties.setProperty("jmx-manager-access-file", annotationAttributes.get("accessFile"));
		gemfireProperties.setProperty("jmx-manager-bind-address", annotationAttributes.get("bindAddress"));
		gemfireProperties.setProperty("jmx-manager-hostname-for-clients", annotationAttributes.get("hostnameForClients"));
		gemfireProperties.setProperty("jmx-manager-password-file", annotationAttributes.get("passwordFile"));
		gemfireProperties.setProperty("jmx-manager-port",
			resolvePort((Integer) annotationAttributes.get("port"), DEFAULT_JMX_MANAGER_PORT));
		gemfireProperties.setProperty("jmx-manager-start", annotationAttributes.get("start"));
		gemfireProperties.setProperty("jmx-manager-update-rate", annotationAttributes.get("updateRate"));

		return gemfireProperties.build();
	}
}
