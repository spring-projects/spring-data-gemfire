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
 * The {@link MemcachedServerConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that applies
 * additional configuration using Pivotal GemFire/Apache Geode {@link Properties} to configure
 * an embedded Memcached server in this cluster member.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.config.annotation.EnableMemcachedServer
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class MemcachedServerConfiguration extends EmbeddedServiceConfigurationSupport {

	protected static final int DEFAULT_MEMCACHED_SERVER_PORT = 11211;

	/**
	 * Returns the {@link EnableMemcachedServer} {@link java.lang.annotation.Annotation} {@link Class} type.
	 *
	 * @return the {@link EnableMemcachedServer} {@link java.lang.annotation.Annotation} {@link Class} type.
	 * @see org.springframework.data.gemfire.config.annotation.EnableMemcachedServer
	 */
	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableMemcachedServer.class;
	}

	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {

		return Optional.of(resolveProperty(memcachedServiceProperty("enabled"), Boolean.TRUE))
			.filter(Boolean.TRUE::equals)
			.map(enabled ->

				PropertiesBuilder.create()
					.setProperty("memcached-port",
						resolvePort(resolveProperty(memcachedServiceProperty("port"),
							(Integer) annotationAttributes.get("port")), DEFAULT_MEMCACHED_SERVER_PORT))
					.setProperty("memcached-protocol",
						resolveProperty(memcachedServiceProperty("protocol"),
							EnableMemcachedServer.MemcachedProtocol.class,
								(EnableMemcachedServer.MemcachedProtocol) annotationAttributes.get("protocol")))
					.build()

			).orElseGet(Properties::new);
	}
}
