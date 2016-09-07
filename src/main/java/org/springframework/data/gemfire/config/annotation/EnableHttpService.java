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
 * The EnableHttpService annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to configure and enable GemFire/Geode's embedded HTTP service.
 *
 * By using this annotation, this allows GemFire's embedded HTTP services, like Pulse, the Management REST API
 * and the Developer REST API to be enabled.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.HttpServiceConfiguration
 * @see <a href="http://geode.docs.pivotal.io/docs/rest_apps/book_intro.html">Developing REST Applications for Apache Geode</a>
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(HttpServiceConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableHttpService {

	/**
	 * If set, then the GemFire member binds the embedded HTTP service to the specified address.
	 * If this property is not set but the HTTP service is enabled using {@literal http-service-port},
	 * then GemFire binds the HTTP service to the member’s local address. Used by the GemFire Pulse Web application
	 * and the Developer REST API service.
	 *
	 * Defaults to unset.
	 */
	String bindAddress() default "";

	/**
	 * If non-zero, then GemFire starts an embedded HTTP service that listens on this port. The HTTP service
	 * is used to host the GemFire Pulse Web application and the development REST API service. If you are hosting
	 * the Pulse web app on your own Web server and are not using the Development REST API service, then disable
	 * this embedded HTTP service by setting this property to zero. Ignored if {@literal jmx-manager}
	 * and {@literal start-dev-rest-api} are both set to {@literal false}.
	 *
	 * Defaults to {@literal 7070}.
	 */
	int port() default HttpServiceConfiguration.DEFAULT_HTTP_SERVICE_PORT;

	/**
	 * Boolean indicating whether to require authentication for HTTP service connections. If this property is not set,
	 * then GemFire uses the value of {@literal cluster-ssl-require-authentication} to determine whether HTTP service
	 * connections require authentication.
	 *
	 * To enable SSL communications for the HTTP service, use the {@link EnableSsl} annotation and set the
	 * {@link EnableSsl#components()} to contain
	 * {@link org.springframework.data.gemfire.config.annotation.EnableSsl.Component#HTTP}.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean sslRequireAuthentication() default false;

	/**
	 * If set to true, then the developer REST API service will be started when cache is created.
	 * The REST service can be configured using {@literal http-service-port} and {@literal http-service-bind-address}
	 * GemFire System Properties.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean startDeveloperRestApi() default false;

}
