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
 * The EnableManager annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to embed and start a GemFire Manager service in the GemFire server/data node.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Import
 * @see ManagerConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(ManagerConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableManager {

	/**
	 * If {@literal true} then this member is willing to be a JMX Manager. All the other JMX Manager properties will be
	 * used when it does become a manager. If this property is {@literal false} then all other jmx-manager-* properties
	 * are ignored.
	 *
	 * Defaults to {@literal true}.
	 */
	boolean jmxManager() default true;

	/**
	 * By default, the JMX Manager will allow full access to all mbeans by any client. If this property is set to
	 * the name of a file then it can restrict clients to only being able to read MBeans; they will not be able
	 * to modify MBeans. The access level can be configured differently in this file for each user name defined
	 * in the password file. For more information about the format of this file see Oracle's documentation
	 * of the {@code com.sun.management.jmxremote.access.file} System property. Ignored if {@literal jmx-manager}
	 * is false or if {@literal jmx-manager-port} is zero.
	 */
	String jmxManagerAccessFile() default "";

	/**
	 * By default, the JMX Manager (when configured with a port) will listen on all the local host's addresses.
	 * You can use this property to configure what IP address or host name the JMX Manager will listen on for
	 * non-HTTP connections. Ignored if JMX Manager is {@literal false} or {@literal jmx-manager-port} is zero.
	 *
	 * Defaults to {@literal localhost}.
	 */
	String jmxManagerBindAddress() default "";

	/**
	 * Lets you control what hostname will be given to clients that ask the Locator for the location of a JMX Manager.
	 * By default, the IP address that the JMX Manager reports is used. But for clients on a different network
	 * this property allows you to configure a different hostname that will be given to clients. Ignored if
	 * {@literal jmx-manager} is {@literal false} or {@literal jmx-manager-port} is zero.
	 *
	 * Defaults to {@literal localhost}.
	 */
	String jmxManagerHostnameForClients() default "";

	/**
	 * By default, the JMX Manager will allow clients without credentials to connect. If this property is set to
	 * the name of a file then only clients that connect with credentials that match an entry in this file will
	 * be allowed. Most JVMs require that the file is only readable by the owner. For more information about the
	 * format of this file see Oracle's documentation of the {@literal com.sun.management.jmxremote.password.file}
	 * System property. Ignored if {@literal jmx-manager} is {@literal false} or {@literal jmx-manager-port} is zero.
	 */
	String jmxManagerPasswordFile() default "";

	/**
	 * The port this JMX Manager will listen to for client connections. If this property is set to zero then GemFire
	 * will not allow remote client connections but you can alternatively use the standard System properties supported
	 * by the JVM for configuring access from remote JMX clients. Ignored if {@literal jmx-manager} is {@literal false}.
	 *
	 * Defaults to {@literal 1099}.
	 */
	int jmxManagerPort() default ManagerConfiguration.DEFAULT_JMX_MANAGER_PORT;

	/**
	 * Enables or disables SSL for connections to the JMX Manager. If {@literal true} and {@literal jmx-manager-port}
	 * is not zero, then the JMX Manager will only accept SSL connections. If this property is not set, then GemFire
	 * uses the value of {@literal cluster-ssl-enabled} to determine whether JMX connections should use SSL.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean jmxManagerSslEnabled() default false;

	/**
	 * A space-separated list of the valid SSL ciphers for JMX Manager connections. A setting of 'any' uses any ciphers
	 * that are enabled by default in the configured JSSE provider. If this property is not set, then GemFire uses
	 * the value of {@literal cluster-ssl-ciphers} to determine which SSL ciphers are used for JMX connections.
	 *
	 * Defaults to {@literal any}.
	 */
	String jmxManagerSslCiphers() default "any";

	/**
	 * A space-separated list of the valid SSL protocols for JMX Manager connections. A setting of 'any' uses any
	 * protocol that is enabled by default in the configured JSSE provider. If this property is not set, then GemFire
	 * uses the value of {@literal cluster-ssl-protocols} to determine which SSL protocols are used for JMX connections.
	 *
	 * Defaults to {@literal any}.
	 */
	String jmxManagerSslProtocols() default "any";

	/**
	 * Boolean indicating whether to require authentication for JMX Manager connections. If this property is not set,
	 * then GemFire uses the value of {@literal cluster-ssl-require-authentication} to determine whether JMX connections
	 * require authentication.
	 *
	 * Defaults to {@literal true}.
	 */
	boolean jmxManagerSslRequireAuthentication() default true;

	/**
	 * If true then this member will start a JMX Manager when it creates a cache. Management tools like Gfsh can be
	 * configured to connect to the JMX Manager. In most cases you should not set this because a JMX Manager will
	 * automatically be started when needed on a member that sets {@literal jmx-manager} to {@literal true}. Ignored if
	 * {@literal jmx-manager} is {@literal false}.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean jmxManagerStart() default false;

	/**
	 * The rate, in milliseconds, at which this member will push updates to any JMX Managers. Currently this value
	 * should be greater than or equal to the {@literal statistic-sample-rate}. Setting this value too high will
	 * cause stale values to be seen by Gfsh and GemFire Pulse.
	 *
	 * Defaults to {@literal 2000}.
	 */
	int jmxManagerUpdateRate() default 2000;

}
