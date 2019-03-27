/*
 * Copyright 2012-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Annotation;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The {@link HttpServiceConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that applies
 * additional configuration by way of Pivotal GemFire/Apache Geode {@link Properties} to configure
 * Pivotal GemFire/Apache Geode's embedded HTTP service and dependent services (e.g. Pulse).
 *
 * @author John Blum
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.config.annotation.EnableHttpService
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @see <a href="https://geode.docs.pivotal.io/docs/rest_apps/book_intro.html">Developing REST Applications for Apache Geode</a>
 * @since 1.9.0
 */
public class HttpServiceConfiguration extends EmbeddedServiceConfigurationSupport {

	public static final boolean DEFAULT_HTTP_SERVICE_SSL_REQUIRE_AUTHENTICATION = false;
	public static final boolean DEFAULT_HTTP_SERVICE_START_DEVELOPER_REST_API = false;

	public static final int DEFAULT_HTTP_SERVICE_PORT = 7070;

	/**
	 * Returns the {@link EnableHttpService} {@link java.lang.annotation.Annotation} {@link Class} type.
	 *
	 * @return the {@link EnableHttpService} {@link java.lang.annotation.Annotation} {@link Class} type.
	 * @see org.springframework.data.gemfire.config.annotation.EnableHttpService
	 */
	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableHttpService.class;
	}

	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {

		return Optional.of(resolveProperty(httpServiceProperty("enabled"), Boolean.TRUE))
			.filter(Boolean.TRUE::equals)
			.map(enabled -> {

				PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

				gemfireProperties.setProperty("http-service-bind-address",
					resolveProperty(httpServiceProperty("bind-address"),
						(String) annotationAttributes.get("bindAddress")));

				gemfireProperties.setPropertyIfNotDefault("http-service-port",
					resolveProperty(httpServiceProperty("port"),
						(Integer) annotationAttributes.get("port")),
							DEFAULT_HTTP_SERVICE_PORT);

				gemfireProperties.setPropertyIfNotDefault("http-service-ssl-require-authentication",
					resolveProperty(httpServiceProperty("ssl-require-authentication"),
						(Boolean) annotationAttributes.get("sslRequireAuthentication")),
							DEFAULT_HTTP_SERVICE_SSL_REQUIRE_AUTHENTICATION);

				gemfireProperties.setPropertyIfNotDefault("start-dev-rest-api",
					resolveProperty(httpServiceProperty("dev-rest-api.start"),
						(Boolean) annotationAttributes.get("startDeveloperRestApi")),
							DEFAULT_HTTP_SERVICE_START_DEVELOPER_REST_API);

				return gemfireProperties.build();

			})
			.orElseGet(Properties::new);
	}
}
