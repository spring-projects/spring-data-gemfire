/*
 * Copyright 2016 the original author or authors.
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

import org.apache.geode.security.AuthInitialize;
import org.springframework.context.annotation.Import;

/**
 * The {@link EnableSecurity} annotation marks a Spring {@link org.springframework.context.annotation.Configuration}
 * annotated class to configure and enable Apache Geode's Security features for authentication, authorization
 * and post processing.
 *
 * @author John Blum
 * @see GeodeIntegratedSecurityConfiguration
 * @see org.apache.geode.security.AuthInitialize
 * @see org.apache.geode.security.SecurityManager
 * @see org.apache.geode.security.PostProcessor
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.ApacheShiroSecurityConfiguration
 * @see org.springframework.data.gemfire.config.annotation.GeodeIntegratedSecurityConfiguration
 * @since 1.0.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import({ ApacheShiroSecurityConfiguration.class, GeodeIntegratedSecurityConfiguration.class })
@SuppressWarnings({ "unused" })
public @interface EnableSecurity {

	/**
	 * Used for authentication. Static creation method returning an {@link AuthInitialize} object,
	 * which obtains credentials for clients.
	 *
	 * Defaults to unset.
	 */
	String clientAuthenticationInitializer() default "";

	/**
	 * Used with authentication. Static creation method returning an {@link AuthInitialize} object, which obtains
	 * credentials for peers in a distributed system.
	 *
	 * Defaults to unset.
	 */
	String peerAuthenticationInitializer() default "";

	/**
	 * Specifies the application {@link Class} type implementing the Apache Geode
	 * {@link org.apache.geode.security.SecurityManager} interface to enable security in Apache Geode.
	 *
	 * Defaults to {@link Void}.
	 */
	Class<?> securityManagerClass() default Void.class;

	/**
	 * Specifies the fully-qualified class name of the application {@link Class} implementing the Apache Geode
	 * {@link org.apache.geode.security.SecurityManager} interface to enable security in Apache Geode.
	 *
	 * Use this Annotation attribute if you are uncertain whether the application class is on the classpath or not.
	 *
	 * Default is unset.
	 */
	String securityManagerClassName() default "";

	/**
	 * Specifies the application {@link Class} type implementing the Apache Geode
	 * {@link org.apache.geode.security.PostProcessor} interface, which used to transform sensitive data
	 * returned from secure data access operations.
	 *
	 * Defaults to {@link Void}.
	 */
	Class<?> securityPostProcessorClass() default Void.class;

	/**
	 * Specifies the fully-qualified class name of the application {@link Class} implementing the Apache Geode
	 * {@link org.apache.geode.security.PostProcessor} interface, which used to transform sensitive data
	 * returned from secure data access operations.
	 *
	 * Use this Annotation attribute if you are uncertain whether the application class is on the classpath or not.
	 *
	 * Default is unset.
	 */
	String securityPostProcessorClassName() default "";

	/**
	 * Sets the Geode System Property referring to the location of an Apache Shiro INI file used to configure
	 * the Apache Shiro Security Framework to secure Apache Geode.
	 *
	 * Default is unset.
	 */
	String shiroIniResourcePath() default "";

}
