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

import com.gemstone.gemfire.security.AccessControl;
import com.gemstone.gemfire.security.AuthInitialize;
import com.gemstone.gemfire.security.Authenticator;

import org.springframework.context.annotation.Import;

/**
 * The EnableAuth annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to configure and enable GemFire/Geode's Authentication and Authorization framework services.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.AuthConfiguration
 * @see com.gemstone.gemfire.security.AccessControl
 * @see com.gemstone.gemfire.security.AuthInitialize
 * @see com.gemstone.gemfire.security.Authenticator
 * @see <a href="http://gemfire.docs.pivotal.io/docs-gemfire/latest/managing/security/authentication_overview.html">Authentication</a>
 * @see <a href="http://gemfire.docs.pivotal.io/docs-gemfire/latest/managing/security/authorization_overview.html">Authorization</a>
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(AuthConfiguration.class)
@SuppressWarnings({ "deprecation", "unused" })
public @interface EnableAuth {

	/**
	 * Used for authorization. Static creation method returning an {@link AccessControl} object, which determines
	 * authorization of client-server cache operations. This specifies the callback that should be invoked
	 * in the pre-operation phase, which is when the request for the operation is received from the client.
	 *
	 * Defaults to unset.
	 */
	String clientAccessor() default "";

	/**
	 * Used for authorization. The callback that should be invoked in the post-operation phase, which is
	 * when the operation has completed on the server but before the result is sent to the client.
	 * The post-operation callback is also invoked for the updates that are sent from server to client
	 * through the notification channel.
	 *
	 * Defaults to unset.
	 */
	String clientAccessPostOperation() default "";

	/**
	 * Used for authentication. Static creation method returning an {@link AuthInitialize} object,
	 * which obtains credentials for clients. The obtained credentials should be acceptable
	 * to the {@link Authenticator} specified through the {@literal security-client-authenticator} property
	 * on the clients.
	 *
	 * Defaults to unset.
	 */
	String clientAuthenticationInitializer() default "";

	/**
	 * Used for authentication. Static creation method returning an {@link Authenticator} object,
	 * which is used by a server to verify the credentials of the connecting client.
	 *
	 * Defaults to unset.
	 */
	String clientAuthenticator() default "";

	/**
	 * Used for authentication. For secure transmission of sensitive credentials like passwords, you can encrypt
	 * the credentials using the Diffie-Hellman key exchange algorithm. Do this by setting the
	 * {@literal security-client-dhalgo} system property on the clients to the name of a valid symmetric key cipher
	 * supported by the JDK.
	 *
	 * Defaults to unset.
	 */
	String clientDiffieHellmanAlgorithm() default "";

	/**
	 * Used with authentication. Static creation method returning an {@link AuthInitialize} object, which obtains
	 * credentials for peers in a distributed system. The obtained credentials should be acceptable to the
	 * {@link Authenticator} specified through the {@literal security-peer-authenticator} property on the peers.
	 *
	 * Defaults to unset.
	 */
	String peerAuthenticationInitializer() default "";

	/**
	 * Used with authentication. Static creation method returning an {@link Authenticator} object, which is used
	 * by a peer to verify the credentials of the connecting peer.
	 *
	 * Defaults to unset.
	 */
	String peerAuthenticator() default "";

	/**
	 * Used with authentication. Timeout in milliseconds used by a peer to verify membership of an unknown
	 * authenticated peer requesting a secure connection.
	 *
	 * Defaults to {@literal 1000} milliseconds.
	 */
	long peerVerifyMemberTimeout() default AuthConfiguration.DEFAULT_PEER_VERIFY_MEMBER_TIMEOUT;

	/**
	 * Used with authentication. The log file for security log messages. If not specified, the member’s
	 * regular log file is used.
	 *
	 * Defaults to unset.
	 */
	String securityLogFile() default "";

	/**
	 * Used with authentication. Logging level detail for security log messages.
	 *
	 * Valid values from lowest to highest are {@literal fine}, {@literal config}, {@literal info}, {@literal warning},
	 * {@literal error}, {@literal severe}, and {@literal none}.
	 *
	 * Defaults to {@literal config}.
	 */
	String securityLogLevel() default AuthConfiguration.DEFAULT_SECURITY_LOG_LEVEL;

	/**
	 * Used for authentication. Any custom properties needed by your {@link AuthInitialize}
	 * or {@link Authenticator} callbacks store in an external {@link java.util.Properties} file.
	 *
	 * Any security-related (properties that begin with {@literal security-*}) configuration properties
	 * that are normally configured in {@literal gemfire.properties} can be moved to a separate
	 * {@literal gfsecurity.properties} file. Placing these configuration settings in a separate file
	 * allows you to restrict access to security configuration data. This way, you can still allow read
	 * or write access for your {@literal gemfire.properties} file.
	 *
	 * Defaults to unset.
	 */
	String securityPropertiesFile() default "";

}
