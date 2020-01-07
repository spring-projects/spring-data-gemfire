/*
 * Copyright 2016-2020 the original author or authors.
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
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * The {@link EnableRedisServer} annotation marks a Spring {@link Configuration @Configuration} annotated {@link Class}
 * to embed the Redis service in this cluster member.
 *
 * The Redis service implements the Redis server protocol enabling Redis clients to connect to and inter-operate with
 * Pivotal GemFire or Apache Geode.
 *
 * However, the embedded Pivotal GemFire/Apache Geode Redis Service can be enabled/disabled externally
 * in {@literal application.properties} by using the {@literal spring.data.gemfire.service.redis.enabled} property
 * even when this {@link Annotation} is present, thereby serving as a toggle.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.MemcachedServerConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
@Import(RedisServerConfiguration.class)
@UsesGemFireProperties
@SuppressWarnings("unused")
public @interface EnableRedisServer {

	/**
	 * Configures the Network bind-address on which the Redis server will accept connections.
	 *
	 * Defaults to {@literal localhost}.
	 *
	 * Use the {@literal spring.data.gemfire.service.redis.bind-address} property in {@literal application.properties}.
	 */
	String bindAddress() default "";

	/**
	 * Configures the Network port on which the Redis server will listen for Redis client connections.
	 *
	 * Defaults to {@literal 6379}.
	 *
	 * Use the {@literal spring.data.gemfire.service.redis.port} property in {@literal application.properties}.
	 */
	int port() default RedisServerConfiguration.DEFAULT_REDIS_PORT;

}
