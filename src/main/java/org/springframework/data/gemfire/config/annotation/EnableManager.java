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
 * The {@link EnableManager} annotation marks a Spring {@link Configuration @Configuration} annotated {@link Class}
 * to configure, embed and start a Pivotal GemFire/Apache Geode Manager service in this cluster member.
 *
 * Automatically sets {@literal jmx-manager} to {@literal true} just by specifying this {@link Annotation}
 * on your Spring application {@link Configuration @Configuration} annotated {@link Class}.
 *
 * However, the embedded Pivotal GemFire/Apache Geode Manager can be enabled/disabled externally
 * in {@literal application.properties} by using the {@literal spring.data.gemfire.manager.enabled} property
 * even when this {@link Annotation} is present, thereby serving as a toggle.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.ManagerConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(ManagerConfiguration.class)
@UsesGemFireProperties
@SuppressWarnings("unused")
public @interface EnableManager {

	/**
	 * By default, the JMX Manager will allow full access to all mbeans by any client. If this property is set to
	 * the name of a file then it can restrict clients to only being able to read MBeans; they will not be able
	 * to modify MBeans. The access level can be configured differently in this file for each user name defined
	 * in the password file. For more information about the format of this file see Oracle's documentation
	 * of the {@code com.sun.management.jmxremote.access.file} System property. Ignored if {@literal jmx-manager}
	 * is false or if {@literal jmx-manager-port} is zero.
	 *
	 * Defaults to unset.
	 *
	 * Use the {@literal spring.data.gemfire.manager.access-file} property in {@literal application.properties}.
	 */
	String accessFile() default "";

	/**
	 * By default, the JMX Manager (when configured with a port) will listen on all the local host's addresses.
	 * You can use this property to configure what IP address or host name the JMX Manager will listen on for
	 * non-HTTP connections. Ignored if JMX Manager is {@literal false} or {@literal jmx-manager-port} is zero.
	 *
	 * Defaults to unset.
	 *
	 * Use the {@literal spring.data.gemfire.manager.bind-address} property in {@literal application.properties}.
	 */
	String bindAddress() default "";

	/**
	 * Lets you control what hostname will be given to clients that ask the Locator for the location of a JMX Manager.
	 * By default, the IP address that the JMX Manager reports is used. But for clients on a different network
	 * this property allows you to configure a different hostname that will be given to clients. Ignored if
	 * {@literal jmx-manager} is {@literal false} or {@literal jmx-manager-port} is zero.
	 *
	 * Defaults to unset.
	 *
	 * Use the {@literal spring.data.gemfire.manager.hostname-for-clients} property
	 * in {@literal application.properties}.
	 */
	String hostnameForClients() default "";

	/**
	 * By default, the JMX Manager will allow clients without credentials to connect. If this property is set to
	 * the name of a file then only clients that connect with credentials that match an entry in this file will
	 * be allowed. Most JVMs require that the file is only readable by the owner. For more information about the
	 * format of this file see Oracle's documentation of the {@literal com.sun.management.jmxremote.password.file}
	 * System property. Ignored if {@literal jmx-manager} is {@literal false} or {@literal jmx-manager-port} is zero.
	 *
	 * Defaults to unset.
	 *
	 * Use the {@literal spring.data.gemfire.manager.password-file} property in {@literal application.properties}.
	 */
	String passwordFile() default "";

	/**
	 * The port this JMX Manager will listen to for client connections. If this property is set to zero then GemFire
	 * will not allow remote client connections but you can alternatively use the standard System properties supported
	 * by the JVM for configuring access from remote JMX clients. Ignored if {@literal jmx-manager} is {@literal false}.
	 *
	 * Defaults to {@literal 1099}.
	 *
	 * Use the {@literal spring.data.gemfire.manager.port} property in {@literal application.properties}.
	 */
	int port() default ManagerConfiguration.DEFAULT_JMX_MANAGER_PORT;

	/**
	 * If {@literal true} then this member will start a JMX Manager when it creates a cache. Management tools
	 * like Gfsh can be configured to connect to the JMX Manager. In most cases you should not set this
	 * because a JMX Manager will automatically be started when needed on a member that sets {@literal jmx-manager}
	 * to {@literal true}. Ignored if {@literal jmx-manager} is {@literal false}.
	 *
	 * Defaults to {@literal false}.
	 *
	 * Use the {@literal spring.data.gemfire.manager.start} property in {@literal application.properties}.
	 */
	boolean start() default false;

	/**
	 * The rate, in milliseconds, at which this member will push updates to any JMX Managers. Currently this value
	 * should be greater than or equal to the {@literal statistic-sample-rate}. Setting this value too high will
	 * cause stale values to be seen by Gfsh and GemFire Pulse.
	 *
	 * Defaults to {@literal 2000} milliseconds.
	 *
	 * Use the {@literal spring.data.gemfire.manager.update-rate} property in {@literal application.properties}.
	 */
	int updateRate() default 2000;

}
