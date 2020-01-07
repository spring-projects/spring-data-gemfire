/*
 * Copyright 2017-2020 the original author or authors.
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
 */
package org.springframework.data.gemfire.config.annotation;

import java.io.IOException;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URI;
import java.util.Optional;
import java.util.Properties;

import org.apache.geode.management.internal.security.ResourceConstants;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.core.env.Environment;
import org.springframework.core.type.AnnotatedTypeMetadata;
import org.springframework.data.gemfire.config.annotation.support.AutoConfiguredAuthenticationInitializer;
import org.springframework.data.gemfire.config.support.RestTemplateConfigurer;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.util.StringUtils;

/**
 * The {@link AutoConfiguredAuthenticationConfiguration} class is a Spring {@link Configuration @Configuration} class
 * that auto-configures Pivotal GemFire / Apache Geode Authentication by providing a implementation
 * of the {@link org.apache.geode.security.AuthInitialize} interface along with setting the necessary Pivotal GemFire / Geode
 * properties.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.apache.geode.security.AuthInitialize
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Condition
 * @see org.springframework.context.annotation.Conditional
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.core.env.Environment
 * @see org.springframework.data.gemfire.config.annotation.EnableBeanFactoryLocator
 * @see org.springframework.data.gemfire.config.annotation.support.AutoConfiguredAuthenticationInitializer
 * @see org.springframework.data.gemfire.util.PropertiesBuilder
 * @since 2.0.0
 */
@Configuration
@Import(BeanFactoryLocatorConfiguration.class)
@Conditional(AutoConfiguredAuthenticationConfiguration.AutoConfiguredAuthenticationCondition.class)
@SuppressWarnings("unused")
public class AutoConfiguredAuthenticationConfiguration {

	protected static final String AUTO_CONFIGURED_AUTH_INIT_STATIC_FACTORY_METHOD =
		AutoConfiguredAuthenticationInitializer.class.getName().concat(".newAuthenticationInitializer");

	protected static final String DEFAULT_USERNAME = "test";
	protected static final String DEFAULT_PASSWORD = DEFAULT_USERNAME;
	protected static final String HTTP_PROTOCOL = "HTTP";
	protected static final String SECURITY_CLIENT_AUTH_INIT = "security-client-auth-init";
	protected static final String SECURITY_PEER_AUTH_INIT = "security-peer-auth-init";

	private Logger logger = LoggerFactory.getLogger(getClass());

	@Bean("GemFireSecurityAuthenticator")
	public Authenticator authenticator(Environment environment) {

		Authenticator authenticator = new Authenticator() {

			@Override
			protected PasswordAuthentication getPasswordAuthentication() {

				String username =
					environment.getProperty(AutoConfiguredAuthenticationInitializer.SDG_SECURITY_USERNAME_PROPERTY,
						DEFAULT_USERNAME);

				String password =
					environment.getProperty(AutoConfiguredAuthenticationInitializer.SDG_SECURITY_PASSWORD_PROPERTY,
						DEFAULT_PASSWORD);

				return new PasswordAuthentication(username, password.toCharArray());
			}
		};

		Authenticator.setDefault(authenticator);

		return authenticator;
	}

	ClientHttpRequestInterceptor loggingAwareClientHttpRequestInterceptor() {

		return (request, body, execution) -> {

			logger.debug("HTTP Request URI [{}]", request.getURI());

			Optional.ofNullable(request.getHeaders())
				.ifPresent(httpHeaders -> {

					CollectionUtils.nullSafeSet(httpHeaders.keySet()).forEach(httpHeaderName -> {
						logger.debug("HTTP Request Header Name [{}] Value [{}]",
							httpHeaderName, httpHeaders.get(httpHeaderName));
					});
				});

			ClientHttpResponse response = execution.execute(request, body);

			Optional.ofNullable(response)
				.ifPresent(it -> {

					try {
						logger.debug("HTTP Response Status Code [{}] Message [{}]",
							it.getRawStatusCode(), it.getStatusText());
					}
					catch (IOException cause) {
						logger.debug("Error occurred getting HTTP Response Status Code and Message", cause);
					}
				});

			return response;
		};
	}

	@Bean
	@Order(Ordered.LOWEST_PRECEDENCE)
	public RestTemplateConfigurer loggingAwareRestTemplateConfigurer() {
		return restTemplate ->  restTemplate.getInterceptors().add(loggingAwareClientHttpRequestInterceptor());
	}

	ClientHttpRequestInterceptor securityAwareClientHttpRequestInterceptor() {

		return (request, body, execution) -> {

			URI uri = request.getURI();

			PasswordAuthentication passwordAuthentication =
				Authenticator.requestPasswordAuthentication(uri.getHost(), null, uri.getPort(),
					HTTP_PROTOCOL, null, uri.getScheme());

			String username = passwordAuthentication.getUserName();
			char[] password = passwordAuthentication.getPassword();

			if (isAuthenticationEnabled(username, password)) {

				HttpHeaders requestHeaders = request.getHeaders();

				requestHeaders.add(ResourceConstants.USER_NAME, username);
				requestHeaders.add(ResourceConstants.PASSWORD, String.valueOf(password));
			}

			return execution.execute(request, body);
		};
	}

	@Bean
	@Order(Ordered.HIGHEST_PRECEDENCE)
	public RestTemplateConfigurer securityAwareRestTemplateConfigurer(Authenticator authenticator) {
		return restTemplate -> restTemplate.getInterceptors().add(securityAwareClientHttpRequestInterceptor());
	}

	private boolean isAuthenticationEnabled(String username, char[] password) {
		return StringUtils.hasText(username) && password != null && password.length > 0;
	}

	@Bean
	public ClientCacheConfigurer authenticationCredentialsSettingClientCacheConfigurer(Environment environment) {
		return (beanName, beanFactory) -> setAuthenticationCredentials(beanFactory.getProperties(), environment);
	}

	@Bean
	public PeerCacheConfigurer authenticationCredentialsSettingPeerCacheConfigurer(Environment environment) {
		return (beanName, beanFactory) -> setAuthenticationCredentials(beanFactory.getProperties(), environment);
	}

	private void setAuthenticationCredentials(Properties gemfireProperties, Environment environment) {

		Optional.ofNullable(gemfireProperties)
			.filter(properties -> isMatch(environment))
			.ifPresent(properties -> {
				properties.setProperty(SECURITY_CLIENT_AUTH_INIT, AUTO_CONFIGURED_AUTH_INIT_STATIC_FACTORY_METHOD);
				properties.setProperty(SECURITY_PEER_AUTH_INIT, AUTO_CONFIGURED_AUTH_INIT_STATIC_FACTORY_METHOD);
			});
	}

	private static boolean isMatch(Environment environment) {

		return Optional.ofNullable(environment)
			.map(env -> env.getProperty(AutoConfiguredAuthenticationInitializer.SDG_SECURITY_USERNAME_PROPERTY))
			.map(StringUtils::hasText)
			.isPresent();
	}

	public static class AutoConfiguredAuthenticationCondition implements Condition {

		@Override
		public boolean matches(ConditionContext conditionContext, AnnotatedTypeMetadata annotatedTypeMetadata) {
			return isMatch(conditionContext.getEnvironment());
		}
	}
}
