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

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * The {@link EnableSsl} annotation marks a Spring {@link Configuration @Configuration} annotated {@link Class}
 * to configure and enable Pivotal GemFire/Apache Geode's TCP/IP Socket SSL.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.SslConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(SslConfiguration.class)
@UsesGemFireProperties
@SuppressWarnings("unused")
public @interface EnableSsl {

	/**
	 * A space-separated list of the valid SSL ciphers for secure Socket connections. A setting of 'any'
	 * uses any ciphers that are enabled by default in the configured JSSE provider.
	 *
	 * Defaults to {@literal any}.
	 *
	 * Use either the {@literal spring.data.gemfire.security.ssl.<component>.ciphers} property
	 * or the {@literal spring.data.gemfire.security.ssl.ciphers} property
	 * in {@literal application.properties}.
	 */
	String ciphers() default "any";

	/**
	 * An array of Pivotal GemFire/Apache Geode Components in which SSL can be enabled.
	 *
	 * Defaults to {@link EnableSsl.Component#CLUSTER}.
	 *
	 * The value(s) for this attribute are used to configure cluster-wide
	 * (e.g. {@literal spring.data.gemfire.security.ssl.cluster.keystore} SSL properties
	 * or individual component {e.g. {@literal spring.data.gemfire.security.ssl.locator.keystore}} SSL properties.
	 */
	Component[] components() default { Component.CLUSTER };

	/**
	 * Pathname to the keystore used for SSL communications.
	 *
	 * Defaults to unset.
	 *
	 * Use either the {@literal spring.data.gemfire.security.ssl.<component>.keystore} property
	 * or the {@literal spring.data.gemfire.security.ssl.keystore} property
	 * in {@literal application.properties}.
	 */
	String keystore() default "";

	/**
	 * Password to access the keys in the keystore used in SSL communications.
	 *
	 * Defaults to unset.
	 *
	 * Use either the {@literal spring.data.gemfire.security.ssl.<component>.keystore-password} property
	 * or the {@literal spring.data.gemfire.security.ssl.keystore-password} property
	 * in {@literal application.properties}.
	 */
	String keystorePassword() default "";

	/**
	 * TODO change to an enum?
	 *
	 * Identifies the type of keystore used in SSL communications.  For example, JKS, PKCS11, etc.
	 *
	 * Defaults to unset.
	 *
	 * Use either the {@literal spring.data.gemfire.security.ssl.<component>.keystore-type} property
	 * or the {@literal spring.data.gemfire.security.ssl.keystore-type} property
	 * in {@literal application.properties}.
	 */
	String keystoreType() default "";

	/**
	 * A space-separated list of the valid SSL protocols used in secure Socket connections. A setting of 'any'
	 * uses any protocol that is enabled by default in the configured JSSE provider.
	 *
	 * Defaults to {@literal any}.
	 *
	 * Use either the {@literal spring.data.gemfire.security.ssl.<component>.protocols} property
	 * or the {@literal spring.data.gemfire.security.ssl.protocols} property
	 * in {@literal application.properties}.
	 */
	String protocols() default "any";

	/**
	 * Boolean value indicating whether to require authentication for SSL communication between peers,
	 * clients and servers, gateways, etc.
	 *
	 * Defaults to {@literal true}.
	 *
	 * Use either the {@literal spring.data.gemfire.security.ssl.<component>.require-authentication} property
	 * or the {@literal spring.data.gemfire.security.ssl.require-authentication} property
	 * in {@literal application.properties}.
	 */
	boolean requireAuthentication() default true;

	/**
	 * Pathname to the truststore used in SSL communications.
	 *
	 * Defaults to unset.
	 *
	 * Use either the {@literal spring.data.gemfire.security.ssl.<component>.truststore} property
	 * or the {@literal spring.data.gemfire.security.ssl.truststore} property
	 * in {@literal application.properties}.
	 */
	String truststore() default "";

	/**
	 * Password to access the keys in the truststore used in SSL communications.
	 *
	 * Defaults to unset.
	 *
	 * Use either the {@literal spring.data.gemfire.security.ssl.<component>.truststore-password} property
	 * or the {@literal spring.data.gemfire.security.ssl.truststore-password} property
	 * in {@literal application.properties}.
	 */
	String truststorePassword() default "";

	enum Component {

		CLUSTER("cluster"),
		GATEWAY("gateway"),
		HTTP("http-service"),
		JMX("jmx-manager"),
		LOCATOR("locator"),
		SERVER("server");

		private final String prefix;

		/* (non-Javadoc) */
		Component(String prefix) {
			this.prefix = prefix;
		}

		/**
		 * Returns a {@link String} representation of this enumerated value.
		 *
		 * @return a {@link String} describing this enumerated value.
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return prefix;
		}
	}
}
