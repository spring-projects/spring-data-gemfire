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
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * The {@link EnableMemcachedServer} annotation marks a Spring {@link Configuration @Configuration}
 * annotated {@link Class} to start an embedded Memcached Server (Gemcached) service in this cluster member.
 *
 * The Gemcached service implements the Memcached Server protocol enabling Memcached clients to connect to
 * and communicate with Pivotal GemFire or Apache Geode servers.
 *
 * However, the embedded Pivotal GemFire/Apache Geode Memcached Service can be enabled/disabled externally
 * in {@literal application.properties} by using the {@literal spring.data.gemfire.service.memcached.enabled} property
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
@Inherited
@Documented
@Import(MemcachedServerConfiguration.class)
@UsesGemFireProperties
@SuppressWarnings("unused")
public @interface EnableMemcachedServer {

	/**
	 * If specified and is non-zero, sets the port number for an embedded Gemcached server
	 * and starts the Gemcached server.
	 *
	 * Default to {@literal 11211}.
	 *
	 * Use the {@literal spring.data.gemfire.service.memcached.port} property in {@literal application.properties}.
	 */
	int port() default MemcachedServerConfiguration.DEFAULT_MEMCACHED_SERVER_PORT;

	/**
	 * Sets the protocol used by an embedded Gemcached server. Valid values are BINARY and ASCII.
	 * If you omit this property, the ASCII protocol is used.
	 *
	 * Default to {@link MemcachedProtocol#ASCII}.
	 *
	 * Use the {@literal spring.data.gemfire.service.memcached.protocol} property in {@literal application.properties}.
	 */
	MemcachedProtocol protocol() default MemcachedProtocol.ASCII;

	/**
	 * Valid values for the Memcached Network Protocol on-the-wire transport.
	 */
	enum MemcachedProtocol {
		ASCII,
		BINARY
	}
}
