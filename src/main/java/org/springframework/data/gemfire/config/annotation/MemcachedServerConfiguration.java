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
 * The MemcachedServerConfiguration class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * that applies additional GemFire configuration by way of GemFire System properties to configure
 * an embedded Memcached server.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.EnableMemcachedServer
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class MemcachedServerConfiguration extends EmbeddedServiceConfigurationSupport {

	protected static final int DEFAULT_MEMCACHED_SERVER_PORT = 11211;

	@Override
	protected Class getAnnotationType() {
		return EnableMemcachedServer.class;
	}

	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		return PropertiesBuilder.create()
			.setProperty("memcached-port", resolvePort((Integer) annotationAttributes.get("port"),
				DEFAULT_MEMCACHED_SERVER_PORT))
			.setProperty("memcached-protocol", annotationAttributes.get("protocol"))
			.build();
	}
}
