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

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.util.Arrays;
import java.util.Map;
import java.util.Properties;

import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;
import org.springframework.util.ObjectUtils;

/**
 * The {@link SslConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that applies
 * additional configuration using Pivotal GemFire/Apache Geode {@link Properties} to configure SSL.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.config.annotation.EnableSsl
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class SslConfiguration extends EmbeddedServiceConfigurationSupport {

	/**
	 * Returns the {@link EnableSsl} {@link java.lang.annotation.Annotation} {@link Class} type.
	 *
	 * @return the {@link EnableSsl} {@link java.lang.annotation.Annotation} {@link Class} type.
	 * @see org.springframework.data.gemfire.config.annotation.EnableSsl
	 */
	@Override
	protected Class getAnnotationType() {
		return EnableSsl.class;
	}

	/* (non-Javadoc) */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {

		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		EnableSsl.Component[] components = (EnableSsl.Component[]) annotationAttributes.get("components");

		if (ObjectUtils.isEmpty(components)) {
			logWarning("SSL will not be configured; No SSL enabled Components %s were specified",
				Arrays.toString(EnableSsl.Component.values()));
		}

		stream(nullSafeArray(components, EnableSsl.Component.class)).forEach(component ->

			gemfireProperties.setProperty(String.format("%s-ssl-ciphers", component),
				resolveProperty(componentSslProperty(component.toString(), "ciphers"),
					resolveProperty(sslProperty("ciphers"),
						(String) annotationAttributes.get("ciphers"))))

				.setProperty(String.format("%s-ssl-enabled", component),
					resolveProperty(componentSslProperty(component.toString(), "enabled"),
						resolveProperty(sslProperty("enabled"), true)))

				.setProperty(String.format("%s-ssl-keystore", component),
					resolveProperty(componentSslProperty(component.toString(), "keystore"),
						resolveProperty(sslProperty("keystore"),
							(String) annotationAttributes.get("keystore"))))

				.setProperty(String.format("%s-ssl-keystore-password", component),
					resolveProperty(componentSslProperty(component.toString(), "keystore-password"),
						resolveProperty(sslProperty("keystore-password"),
							(String) annotationAttributes.get("keystorePassword"))))

				.setProperty(String.format("%s-ssl-keystore-type", component),
					resolveProperty(componentSslProperty(component.toString(), "keystore-type"),
						resolveProperty(sslProperty("keystore-type"),
							(String) annotationAttributes.get("keystoreType"))))

				.setProperty(String.format("%s-ssl-protocols", component),
					resolveProperty(componentSslProperty(component.toString(), "protocols"),
						resolveProperty(sslProperty("protocols"),
							(String) annotationAttributes.get("protocols"))))

				.setProperty(String.format("%s-ssl-require-authentication", component),
					resolveProperty(componentSslProperty(component.toString(), "require-authentication"),
						resolveProperty(sslProperty("require-authentication"),
							Boolean.TRUE.equals(annotationAttributes.get("requireAuthentication")))))

				.setProperty(String.format("%s-ssl-truststore", component),
					resolveProperty(componentSslProperty(component.toString(), "truststore"),
						resolveProperty(sslProperty("truststore"),
							(String) annotationAttributes.get("truststore"))))

				.setProperty(String.format("%s-ssl-truststore-password", component),
					resolveProperty(componentSslProperty(component.toString(), "truststore-password"),
						resolveProperty(sslProperty("truststore-password"),
							(String) annotationAttributes.get("truststorePassword"))))

		);

		return gemfireProperties.build();
	}
}
