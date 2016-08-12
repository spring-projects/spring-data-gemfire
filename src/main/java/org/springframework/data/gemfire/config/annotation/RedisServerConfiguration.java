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
 * The RedisServerConfiguration class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * that applies additional GemFire configuration by way of GemFire System properties to configure
 * an embedded Redis server.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.EnableRedisServer
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class RedisServerConfiguration extends EmbeddedServiceConfigurationSupport {

	protected static final int DEFAULT_REDIS_PORT = 6379;

	@Override
	protected Class getAnnotationType() {
		return EnableRedisServer.class;
	}

	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		return new PropertiesBuilder()
			.setProperty("redis-bind-address", annotationAttributes.get("bindAddress"))
			.setProperty("redis-port", resolvePort((Integer)annotationAttributes.get("port"), DEFAULT_REDIS_PORT))
			.build();
	}
}
