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

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;

import javax.annotation.PostConstruct;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.internal.security.SecurityService;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.realm.Realm;
import org.apache.shiro.spring.LifecycleBeanPostProcessor;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ConfigurationCondition;
import org.springframework.core.type.AnnotatedTypeMetadata;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

/**
 * The {@link ApacheShiroSecurityConfiguration} class is a Spring {@link Configuration @Configuration} component
 * responsible for configuring and initializing the Apache Shiro security framework in order to secure Apache Geode
 * administrative and data access operations.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.internal.security.SecurityService
 * @see org.apache.shiro.mgt.SecurityManager
 * @see org.apache.shiro.spring.LifecycleBeanPostProcessor
 * @see org.springframework.context.annotation.Condition
 * @see org.springframework.context.annotation.Conditional
 * @since 1.0.0
 */
@Configuration
@Conditional(ApacheShiroSecurityConfiguration.ApacheShiroPresentCondition.class)
@SuppressWarnings("unused")
public class ApacheShiroSecurityConfiguration {

	@Autowired(required = false)
	private List<Realm> realms = Collections.emptyList();

	@Autowired(required = false)
	private org.apache.shiro.mgt.SecurityManager shiroSecurityManager;

	/**
	 * {@link Bean} definition to configure and register an Apache Shiro, Spring {@link LifecycleBeanPostProcessor}
	 * used to automatically call lifecycle callback methods on Shiro security components during Spring container
	 * initialization and destruction phases.
	 *
	 * The registration of this {@link Bean} definition is dependent upon whether the user is using Apache Shiro
	 * to secure Apache Geode, which is determined by the presence of Apache Shiro {@link Realm Realms}
	 * declared in the Spring {@link org.springframework.context.ApplicationContext}.
	 *
	 * @return an Apache Shiro, Spring {@link LifecycleBeanPostProcessor} bean.
	 * @see org.apache.shiro.spring.LifecycleBeanPostProcessor
	 */
	@Bean
	//@Conditional(ShiroRealmsConfigured.class)
	public BeanPostProcessor shiroLifecycleBeanPostProcessor() {
		return new LifecycleBeanPostProcessor();
	}

	/**
	 * {@link Bean} definition used to configure and register an Apache Shiro
	 * {@link org.apache.shiro.mgt.SecurityManager} implementation to secure Apache Geode.
	 *
	 * The registration of this {@link Bean} definition is dependent upon whether the user is using Apache Shiro
	 * to secure Apache Geode, which is determined by the presence of Apache Shiro {@link Realm Realms}
	 * declared in the Spring {@link org.springframework.context.ApplicationContext}.
	 *
	 * This {@link Bean} definition declares a dependency on the Apache Geode {@link GemFireCache} instance
	 * in order to ensure the Geode cache is created and initialized first, thus evaluating any security configuration
	 * logic internally in Apache Geode that may potentially overwrite the Spring configuration.
	 *
	 * @return an Apache Shiro {@link org.apache.shiro.mgt.SecurityManager} implementation used to secure Apache Geode.
	 * @see org.apache.shiro.mgt.SecurityManager
	 * @see #isRealmsPresent()
	 * @see #getRealms()
	 */
	@Bean
	//@Conditional(ShiroRealmsConfigured.class)
	public org.apache.shiro.mgt.SecurityManager shiroSecurityManager(GemFireCache gemfireCache) {
		return (isRealmsPresent() ? new DefaultSecurityManager(getRealms()) : null);
	}

	/**
	 * Post processes the Apache Shiro security components by registering the Apach Shiro
	 * {@link org.apache.shiro.mgt.SecurityManager} if present with the Apache Shiro security framework
	 * and proceeds to enable Apache Geode security.
	 *
	 * @throws IllegalStateException if an Apache Shiro {@link org.apache.shiro.mgt.SecurityManager} is present
	 * and Apache Geode security could not be enabled.
	 * @see #registerSecurityManager(org.apache.shiro.mgt.SecurityManager)
	 * @see #enableApacheGeodeSecurity()
	 */
	@PostConstruct
	public void postProcess() {
		if (this.shiroSecurityManager != null) {
			registerSecurityManager(this.shiroSecurityManager);

			if (!enableApacheGeodeSecurity()) {
				throw new IllegalStateException("Failed to enable security services in Apache Geode");
			}
		}
	}

	/**
	 * Registers the given Apache Shiro {@link org.apache.shiro.mgt.SecurityManager} with the Apache Shiro
	 * security framework.
	 *
	 * @param securityManager {@link org.apache.shiro.mgt.SecurityManager} to register.
	 * @return the given {@link org.apache.shiro.mgt.SecurityManager} reference.
	 * @throws IllegalArgumentException if {@link org.apache.shiro.mgt.SecurityManager} is {@literal null}.
	 * @see org.apache.shiro.SecurityUtils#setSecurityManager(org.apache.shiro.mgt.SecurityManager)
	 * @see org.apache.shiro.mgt.SecurityManager
	 */
	protected org.apache.shiro.mgt.SecurityManager registerSecurityManager(
			org.apache.shiro.mgt.SecurityManager securityManager) {

		Assert.notNull(securityManager, "The Apache Shiro SecurityManager to register must not be null");

		SecurityUtils.setSecurityManager(securityManager);

		return securityManager;
	}

	/**
	 * Sets the Apache Geode, Integrated Security {@link SecurityService} property {@literal isIntegratedSecurity}
	 * to {@literal true} to indicate that Apache Geode security is enabled.
	 *
	 * @return a boolean value indicating whether Apache Geode's Integrated Security framework services
	 * were successfully enabled.
	 * @see org.apache.geode.internal.security.SecurityService#getSecurityService()
	 */
	protected boolean enableApacheGeodeSecurity() {
		SecurityService securityService = SecurityService.getSecurityService();

		if (securityService != null) {
			Field isIntegratedSecurity = ReflectionUtils.findField(securityService.getClass(),
				"isIntegratedSecurity", Boolean.TYPE);

			if (isIntegratedSecurity != null) {
				ReflectionUtils.makeAccessible(isIntegratedSecurity);
				ReflectionUtils.setField(isIntegratedSecurity, securityService, true);

				return true;
			}
		}

		return false;
	}

	/**
	 * Returns the {@link List} of Apache Shiro {@link Realm Realms} configured in
	 * this Spring {@link org.springframework.context.ApplicationContext}.
	 *
	 * @return a {@link List} of configured/declared Apache Shiro {@link Realm Realms}.
	 * @see org.apache.shiro.realm.Realm
	 */
	protected List<Realm> getRealms() {
		return this.realms;
	}

	/**
	 * Determines whether any Apache Shiro {@link Realm Realms} were configured in
	 * this Spring {@link org.springframework.context.ApplicationContext}.
	 *
	 * @return a boolean value indicating whether any Apache Shiro {@link Realm Realms} were declared and configured
	 * in this Spring {@link org.springframework.context.ApplicationContext}.
	 * @see #getRealms()
	 */
	protected boolean isRealmsPresent() {
		return !CollectionUtils.isEmpty(getRealms());
	}

	/**
	 * A Spring {@link Condition} to determine whether the user has included (declared) the 'shiro-spring' dependency
	 * on their application's classpath, which is necessary for configuring Apache Shiro to secure Apache Geode
	 * in a Spring context.
	 *
	 * @see org.springframework.context.annotation.Condition
	 */
	public static class ApacheShiroPresentCondition implements Condition {

		protected static final String APACHE_SHIRO_LIFECYCLE_BEAN_POST_PROCESOR_CLASS_NAME =
			"org.apache.shiro.spring.LifecycleBeanPostProcessor";

		/**
		 * @inheritDoc
		 */
		@Override
		public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
			return ClassUtils.isPresent(APACHE_SHIRO_LIFECYCLE_BEAN_POST_PROCESOR_CLASS_NAME,
				context.getClassLoader());
		}
	}

	/**
	 * A Spring {@link Condition} implementation that determines whether the user declared and configured
	 * any Apache Shiro {@link Realm Realms}, which are necessary to configure the Apache Shiro security framework
	 * with security meta-data used to secure Apache Geode.
	 *
	 * @see org.springframework.context.annotation.ConfigurationCondition
	 */
	public static class ShiroRealmsConfigured implements ConfigurationCondition {

		/**
		 * @inheritDoc
		 */
		@Override
		public ConfigurationPhase getConfigurationPhase() {
			return ConfigurationPhase.REGISTER_BEAN;
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
			ListableBeanFactory beanFactory = context.getBeanFactory();

			ApacheShiroSecurityConfiguration securityConfiguration =
				beanFactory.getBean(ApacheShiroSecurityConfiguration.class);

			return (securityConfiguration.isRealmsPresent() || !beanFactory.getBeansOfType(Realm.class).isEmpty());
		}
	}
}
