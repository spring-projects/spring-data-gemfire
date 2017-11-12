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

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.realm.Realm;
import org.apache.shiro.spring.LifecycleBeanPostProcessor;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.OrderComparator;
import org.springframework.core.type.AnnotatedTypeMetadata;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;

/**
 * The {@link ApacheShiroSecurityConfiguration} class is a Spring {@link Configuration @Configuration} component
 * responsible for configuring and initializing the Apache Shiro security framework in order to secure Apache Geode
 * administrative and data access operations.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.internal.security.SecurityService
 * @see org.apache.shiro.mgt.DefaultSecurityManager
 * @see org.apache.shiro.realm.Realm
 * @see org.apache.shiro.spring.LifecycleBeanPostProcessor
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.ListableBeanFactory
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Condition
 * @see org.springframework.context.annotation.Conditional
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.annotation.ApacheShiroSecurityConfiguration.ApacheShiroPresentCondition
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @since 1.9.0
 */
@Configuration
@Conditional(ApacheShiroSecurityConfiguration.ApacheShiroPresentCondition.class)
@SuppressWarnings("unused")
public class ApacheShiroSecurityConfiguration extends AbstractAnnotationConfigSupport {

	/**
	 * Returns the {@link EnableSecurity} {@link java.lang.annotation.Annotation} {@link Class} type.
	 *
	 * @return the {@link EnableSecurity} {@link java.lang.annotation.Annotation} {@link Class} type.
	 * @see org.springframework.data.gemfire.config.annotation.EnableSecurity
	 */
	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableSecurity.class;
	}

	/**
	 * Sets a reference to the Spring {@link BeanFactory}.
	 *
	 * @param beanFactory reference to the Spring {@link BeanFactory}.
	 * @throws IllegalArgumentException if the Spring {@link BeanFactory} is not
	 * an instance of {@link ListableBeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	@Override
	@SuppressWarnings("all")
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {

		super.setBeanFactory(Optional.ofNullable(beanFactory)
			.filter(it -> it instanceof ListableBeanFactory)
			.orElseThrow(() -> newIllegalArgumentException(
				"BeanFactory [%s] must be an instance of ListableBeanFactory",
					ObjectUtils.nullSafeClassName(beanFactory))));
	}

	/**
	 * Returns a reference to the Spring {@link BeanFactory}.
	 *
	 * @return a reference to the Spring {@link BeanFactory}.
	 * @throws IllegalStateException if the Spring {@link BeanFactory} was not set.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected ListableBeanFactory getListableBeanFactory() {
		return (ListableBeanFactory) getBeanFactory();
	}

	@Bean
	public BeanFactoryPostProcessor shiroGemFireBeanFactoryPostProcessor() {

		return configurableListableBeanFactory ->
			SpringUtils.addDependsOn(configurableListableBeanFactory.getBeanDefinition("gemfireCache"),
				"shiroSecurityManager");
	}

	/**
	 * {@link Bean} definition to define, configure and register an Apache Shiro Spring
	 * {@link LifecycleBeanPostProcessor} to automatically call lifecycle callback methods
	 * on Shiro security components during Spring container initialization and destruction phases.
	 *
	 * @return an instance of the Apache Shiro Spring {@link LifecycleBeanPostProcessor} to handle the lifecycle
	 * of Apache Shiro security framework components.
	 * @see org.apache.shiro.spring.LifecycleBeanPostProcessor
	 */
	@Bean
	public BeanPostProcessor shiroLifecycleBeanPostProcessor() {
		return new LifecycleBeanPostProcessor();
	}

	/**
	 * {@link Bean} definition to define, configure and register an Apache Shiro
	 * {@link org.apache.shiro.mgt.SecurityManager} implementation to secure Apache Geode.
	 *
	 * The registration of this {@link Bean} definition is dependent upon whether the user is using Apache Shiro
	 * to secure Apache Geode, which is determined by the presence of Apache Shiro {@link Realm Realms}
	 * declared in the Spring {@link org.springframework.context.ApplicationContext}.
	 *
	 * This {@link Bean} definition declares a dependency on the Apache Geode {@link GemFireCache} instance
	 * in order to ensure the Geode cache is created and initialized first.  This ensures that any internal Geode
	 * security configuration logic is evaluated and processed before SDG attempts to configure Apache Shiro
	 * as Apache Geode's security provider.
	 *
	 * Additionally, this {@link Bean} definition will register the Apache Shiro
	 * {@link org.apache.geode.security.SecurityManager} with the Apache Shiro security framework
	 *
	 * Finally, this method proceeds to enable Apache Geode security.

	 * @return an Apache Shiro {@link org.apache.shiro.mgt.SecurityManager} implementation used to secure Apache Geode.
	 * @throws IllegalStateException if an Apache Shiro {@link org.apache.shiro.mgt.SecurityManager} was registered
	 * with the Apache Shiro security framework but Apache Geode security could not be enabled.
	 * @see org.apache.shiro.mgt.SecurityManager
	 * @see #registerSecurityManager(org.apache.shiro.mgt.SecurityManager)
	 * @see #resolveRealms()
	 */
	@Bean
	public org.apache.shiro.mgt.SecurityManager shiroSecurityManager() {

		return Optional.ofNullable(resolveRealms())
			.filter(realms -> !realms.isEmpty())
			.map(realms -> new DefaultSecurityManager(realms))
			.map(this::registerSecurityManager)
			.orElse(null);
	}

	/**
	 * Resolves all the Apache Shiro {@link Realm Realms} declared and configured as Spring managed beans
	 * in the Spring {@link org.springframework.context.ApplicationContext}.
	 *
	 * This method will order the Realms according to priority order to ensure that the Apache Shiro Realms
	 * are applied in the correct sequence, as declared/configured.
	 *
	 * @return a {@link List} of all Apache Shiro {@link Realm Realms} declared and configured as Spring managed beans
	 * in the Spring {@link org.springframework.context.ApplicationContext}.
	 * @see org.springframework.beans.factory.ListableBeanFactory#getBeansOfType(Class, boolean, boolean)
	 * @see org.springframework.core.OrderComparator
	 * @see org.apache.shiro.realm.Realm
	 */
	protected List<Realm> resolveRealms() {

		try {

			Map<String, Realm> realmBeans = getListableBeanFactory().getBeansOfType(Realm.class,
				false, false);

			List<Realm> realms = new ArrayList<>(CollectionUtils.nullSafeMap(realmBeans).values());

			realms.sort(OrderComparator.INSTANCE);

			return realms;
		}
		catch (Exception ignore) {
			return Collections.emptyList();
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
	 * A Spring {@link Condition} to determine whether the user has included (declared) the 'shiro-spring' dependency
	 * on their application's classpath, which is necessary for configuring Apache Shiro to secure Apache Geode
	 * in a Spring context.
	 *
	 * @see org.springframework.context.annotation.Condition
	 */
	public static class ApacheShiroPresentCondition implements Condition {

		protected static final String APACHE_SHIRO_LIFECYCLE_BEAN_POST_PROCESSOR_CLASS_NAME =
			"org.apache.shiro.spring.LifecycleBeanPostProcessor";

		/**
		 * @inheritDoc
		 */
		@Override
		@SuppressWarnings("all")
		public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
			return ClassUtils.isPresent(APACHE_SHIRO_LIFECYCLE_BEAN_POST_PROCESSOR_CLASS_NAME,
				context.getClassLoader());
		}
	}
}
