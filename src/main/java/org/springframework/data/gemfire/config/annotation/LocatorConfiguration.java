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
import java.util.Optional;
import java.util.Properties;

import org.apache.geode.distributed.Locator;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The {@link LocatorConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that applies
 * additional configuration by way of Pivotal GemFire/Apache Geode {@link Properties} to configure
 * an embedded {@link Locator}.
 *
 * @author John Blum
 * @see org.apache.geode.distributed.Locator
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.config.annotation.EnableLocator
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class LocatorConfiguration extends EmbeddedServiceConfigurationSupport {

	protected static final int DEFAULT_LOCATOR_PORT = GemfireUtils.DEFAULT_LOCATOR_PORT;

	protected static final String START_LOCATOR_GEMFIRE_PROPERTY_NAME = "start-locator";

	/**
	 * Returns the {@link EnableLocator} {@link java.lang.annotation.Annotation} {@link Class} type.
	 *
	 * @return the {@link EnableLocator} {@link java.lang.annotation.Annotation} {@link Class} type.
	 * @see org.springframework.data.gemfire.config.annotation.EnableLocator
	 */
	@Override
	protected Class getAnnotationType() {
		return EnableLocator.class;
	}

	/* (non-Javadoc) */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {

		return Optional.of(resolveProperty(locatorProperty("enabled"), Boolean.TRUE))
			.filter(Boolean.TRUE::equals)
			.map(enabled -> {

				String host = resolveHost(resolveProperty(locatorProperty("host"),
					(String) annotationAttributes.get("host")));

				int port = resolvePort(resolveProperty(locatorProperty("port"),
					(Integer) annotationAttributes.get("port")), DEFAULT_LOCATOR_PORT);

				return PropertiesBuilder.create()
					.setProperty(START_LOCATOR_GEMFIRE_PROPERTY_NAME, String.format("%s[%d]", host, port))
					.build();

			}).orElseGet(Properties::new);
	}
}
