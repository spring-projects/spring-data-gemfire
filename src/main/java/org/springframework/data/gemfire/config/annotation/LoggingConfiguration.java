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

import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The {@link LoggingConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that applies
 * additional configuration using Pivotal GemFire/Apache Geode {@link Properties} to configure
 * Pivotal GemFire/Apache Geode logging.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.config.annotation.EnableLogging
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class LoggingConfiguration extends EmbeddedServiceConfigurationSupport {

	public static final int DEFAULT_LOG_DISK_SPACE_LIMIT = 0;
	public static final int DEFAULT_LOG_FILE_SIZE_LIMIT = 0;

	public static final String DEFAULT_LOG_LEVEL = "config";

	/**
	 * Returns the {@link EnableLogging} {@link java.lang.annotation.Annotation} {@link Class} type.
	 *
	 * @return the {@link EnableLogging} {@link java.lang.annotation.Annotation} {@link Class} type.
	 * @see org.springframework.data.gemfire.config.annotation.EnableLogging
	 */
	@Override
	protected Class getAnnotationType() {
		return EnableLogging.class;
	}

	/* (non-Javadoc) */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {

		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		gemfireProperties.setPropertyIfNotDefault("log-disk-space-limit",
			resolveProperty(loggingProperty("log-disk-space-limit"),
				(Integer) annotationAttributes.get("logDiskSpaceLimit")), DEFAULT_LOG_DISK_SPACE_LIMIT);

		gemfireProperties.setProperty("log-file",
			resolveProperty(loggingProperty("log-file"),
				(String) annotationAttributes.get("logFile")));

		gemfireProperties.setPropertyIfNotDefault("log-file-size-limit",
			resolveProperty(loggingProperty("log-file-size-limit"),
				(Integer) annotationAttributes.get("logFileSizeLimit")), DEFAULT_LOG_FILE_SIZE_LIMIT);

		gemfireProperties.setPropertyIfNotDefault("log-level",
			resolveProperty(loggingProperty("level"),
				(String) annotationAttributes.get("logLevel")), DEFAULT_LOG_LEVEL);

		return gemfireProperties.build();
	}
}
