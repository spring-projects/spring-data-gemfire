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

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.annotation.Import;

/**
 * The EnableEmbeddedLocator annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to start an embedded GemFire Locator service in this GemFire server/data node.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.EmbeddedLocatorConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(EmbeddedLocatorConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableEmbeddedLocator {

	/**
	 * Configures the host/IP address on which the embedded Locator service will bind to for accepting connections
	 * from clients sending Locator requests.
	 *
	 * Default is {@literal localhost}.
	 */
	String host() default EmbeddedLocatorConfiguration.DEFAULT_HOST;

	/**
	 * Configures the port on which the embedded Locator service will bind to listening for client connections
	 * sending Locator requests.
	 *
	 * Default is {@literal 10334}.
	 */
	int port() default EmbeddedLocatorConfiguration.DEFAULT_LOCATOR_PORT;

}
