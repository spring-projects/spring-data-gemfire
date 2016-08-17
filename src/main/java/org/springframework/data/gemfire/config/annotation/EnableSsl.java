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
 * The EnableSsl class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(SslConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableSsl {

	/**
	 * A space-separated list of the valid SSL ciphers for secure Socket connections. A setting of 'any'
	 * uses any ciphers that are enabled by default in the configured JSSE provider.
	 *
	 * Defaults to {@literal any}.
	 */
	String ciphers() default "any";

	/**
	 * A list of GemFire components in which SSL will be enabled.
	 *
	 * Defaults to {@link org.springframework.data.gemfire.config.annotation.EnableSsl.Component#CLUSTER}
	 */
	Component[] components() default { Component.CLUSTER };

	/**
	 * Pathname to the keystore used for SSL communications.
	 *
	 * Defaults to unset.
	 */
	String keystore() default "";

	/**
	 * Password to access the keys in the keystore used in SSL communications.
	 *
	 * Defaults to unset.
	 */
	String keystorePassword() default "";

	/**
	 * Identifies the type of keystore used in SSL communications.  For example, JKS, PKCS11, etc.
	 *
	 * Defaults to unset.
	 */
	// TODO change to a enum?
	String keystoreType() default "";

	/**
	 * A space-separated list of the valid SSL protocols used in secure Socket connections. A setting of 'any'
	 * uses any protocol that is enabled by default in the configured JSSE provider.
	 *
	 * Defaults to {@literal any}.
	 */
	String protocols() default "any";

	/**
	 * Boolean value indicating whether to require authentication for SSL communication between peers,
	 * clients and servers, gateways, etc.
	 *
	 * Defaults to {@literal true}.
	 */
	boolean requireAuthentication() default true;

	/**
	 * Pathname to the truststore used in SSL communications.
	 *
	 * Defaults to unset.
	 */
	String truststore() default "";

	/**
	 * Password to access the keys in the truststore used in SSL communications.
	 *
	 * Defaults to unset.
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

		Component(String prefix) {
			this.prefix = prefix;
		}

		@Override
		public String toString() {
			return prefix;
		}
	}
}
