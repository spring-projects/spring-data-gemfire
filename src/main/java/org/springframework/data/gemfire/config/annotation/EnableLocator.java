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

import org.apache.geode.distributed.Locator;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * The {@link EnableLocator} annotation configures a Spring {@link Configuration @Configuration} annotated {@link Class}
 * to start an embedded Pivotal GemFire/Apache Geode {@link Locator} service in this cluster member.
 *
 * However, the embedded Pivotal GemFire/Apache Geode Locator service can be enabled/disabled externally
 * in {@literal application.properties} with the {@literal spring.data.gemfire.service.http.enabled} property
 * even when this {@link Annotation} is present, thereby serving as a toggle.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.LocatorConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(LocatorConfiguration.class)
@UsesGemFireProperties
@SuppressWarnings("unused")
public @interface EnableLocator {

	/**
	 * Configures the host/IP address on which the embedded {@link Locator} service will bind to
	 * for accepting connections from clients sending {@link Locator} requests.
	 *
	 * Defaults to {@literal localhost}.
	 *
	 * Use the {@literal spring.data.gemfire.locator.host} property
	 * in Spring Boot {@literal application.properties}.
	 */
	String host() default LocatorConfiguration.DEFAULT_HOST;

	/**
	 * Configures the port on which the embedded {@link Locator} service will bind to
	 * listening for client connections sending {@link Locator} requests.
	 *
	 * Defaults to {@literal 10334}.
	 *
	 * Use the {@literal spring.data.gemfire.locator.port} property
	 * in Spring Boot {@literal application.properties}.
	 */
	int port() default LocatorConfiguration.DEFAULT_LOCATOR_PORT;

}
