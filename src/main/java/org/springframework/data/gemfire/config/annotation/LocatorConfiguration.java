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

import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The LocatorConfiguration class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * that applies additional GemFire configuration by way of GemFire System properties to configure
 * an embedded GemFire Locator.
 *
 * @author John Blum
 * @see EnableLocator
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class LocatorConfiguration extends EmbeddedServiceConfigurationSupport {

	protected static final int DEFAULT_LOCATOR_PORT = GemfireUtils.DEFAULT_LOCATOR_PORT;

	protected static final String START_LOCATOR_GEMFIRE_SYSTEM_PROPERTY_NAME = "start-locator";

	@Override
	protected Class getAnnotationType() {
		return EnableLocator.class;
	}

	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		String host = resolveHost((String) annotationAttributes.get("host"));
		int port = resolvePort((Integer) annotationAttributes.get("port"), DEFAULT_LOCATOR_PORT);

		return PropertiesBuilder.create()
			.setProperty(START_LOCATOR_GEMFIRE_SYSTEM_PROPERTY_NAME, String.format("%s[%d]", host, port))
			.build();
	}
}
