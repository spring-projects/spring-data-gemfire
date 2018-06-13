/*
 * Copyright 2017-2018 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.annotation;

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeMap;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Executor;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.query.CqQuery;
import org.apache.geode.cache.query.QueryService;
import org.springframework.aop.framework.AopProxyUtils;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.listener.ContinuousQueryDefinition;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.data.gemfire.listener.annotation.ContinuousQuery;
import org.springframework.data.gemfire.util.CacheUtils;
import org.springframework.util.Assert;
import org.springframework.util.ErrorHandler;
import org.springframework.util.StringUtils;

/**
 * The {@link ContinuousQueryConfiguration} class is a Spring {@link Configuration @Configuration} class enabling
 * Continuous Query (CQ) Pivotal GemFire/Apache Geode capabilities in this cache client application.
 *
 * @author John Blum
 * @see java.util.concurrent.Executor
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.query.CqQuery
 * @see org.apache.geode.cache.query.QueryService
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.core.annotation.AnnotationAttributes
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @see org.springframework.data.gemfire.listener.ContinuousQueryDefinition
 * @see org.springframework.data.gemfire.listener.ContinuousQueryListener
 * @see org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer
 * @see org.springframework.data.gemfire.listener.annotation.ContinuousQuery
 * @see org.springframework.util.ErrorHandler
 * @since 2.0.0
 */
@Configuration
@SuppressWarnings("unused")
public class ContinuousQueryConfiguration extends AbstractAnnotationConfigSupport implements ImportAware {

	protected static final String ORG_SPRINGFRAMEWORK_DATA_GEMFIRE_PACKAGE_NAME = "org.springframework.data.gemfire";
	protected static final String ORG_SPRINGFRAMEWORK_PACKAGE_NAME = "org.springframework";

	private int phase;

	@Autowired(required = false)
	private List<ContinuousQueryListenerContainerConfigurer> configurers = Collections.emptyList();

	private String errorHandlerBeanName;
	private String poolName;
	private String queryServiceBeanName;
	private String taskExecutorBeanName;

	/**
	 * Returns the {@link Annotation} {@link Class type} that configures and creates {@link CqQuery Continuous Queries}
	 * for application {@link ContinuousQuery} annotated POJO service methods.
	 *
	 * @return the {@link Annotation} {@link Class type} that configures and creates {@link CqQuery Continuous Queries}
	 * for application {@link ContinuousQuery} annotated POJO service methods.
	 * @see org.springframework.data.gemfire.config.annotation.EnableContinuousQueries
	 * @see org.springframework.data.gemfire.listener.annotation.ContinuousQuery
	 * @see java.lang.annotation.Annotation
	 * @see java.lang.Class
	 */
	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableContinuousQueries.class;
	}

	@Override
	public void setImportMetadata(AnnotationMetadata importingClassMetadata) {

		if (isAnnotationPresent(importingClassMetadata)) {

			AnnotationAttributes enableContinuousQueriesAttributes = getAnnotationAttributes(importingClassMetadata);

			setErrorHandlerBeanName(enableContinuousQueriesAttributes.getString("errorHandlerBeanName"));
			setPhase(enableContinuousQueriesAttributes.<Integer>getNumber("phase"));
			setPoolName(enableContinuousQueriesAttributes.getString("poolName"));
			setQueryServiceBeanName(enableContinuousQueriesAttributes.getString("queryServiceBeanName"));
			setTaskExecutorBeanName(enableContinuousQueriesAttributes.getString("taskExecutorBeanName"));
		}
	}

	@Bean
	public BeanPostProcessor continuousQueryBeanPostProcessor() {

		return new BeanPostProcessor() {

			private ContinuousQueryListenerContainer container;

			private List<ContinuousQueryDefinition> continuousQueryDefinitions = new ArrayList<>();

			@Override
			public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {

				if (bean instanceof ContinuousQueryListenerContainer) {
					this.container = (ContinuousQueryListenerContainer) bean;
					this.continuousQueryDefinitions.forEach(this.container::addListener);
					this.continuousQueryDefinitions.clear();
				}
				else if (isApplicationBean(bean, beanName)) {

					Object resolvedBean = resolveTargetObject(bean);

					List<ContinuousQueryDefinition> definitions = stream(resolvedBean.getClass().getMethods())
						.filter(method -> method.isAnnotationPresent(ContinuousQuery.class))
						.map(method -> ContinuousQueryDefinition.from(resolvedBean, method))
						.collect(Collectors.toList());

					Optional.ofNullable(this.container).map(container -> {
						definitions.forEach(container::addListener);
						return container;
					}).orElseGet(() -> {
						this.continuousQueryDefinitions.addAll(definitions);
						return null;
					});

				}

				return bean;
			}
		};
	}

	private boolean isApplicationBean(Object bean, String beanName) {

		return Optional.ofNullable(bean)
			//.filter(this::isNotProxy)
			.map(this::resolveTargetObject)
			.map(Object::getClass)
			.map(Class::getPackage)
			.map(Package::getName)
			.filter(StringUtils::hasText)
			.filter(packageName -> packageName.startsWith(ORG_SPRINGFRAMEWORK_DATA_GEMFIRE_PACKAGE_NAME)
				|| !packageName.startsWith(ORG_SPRINGFRAMEWORK_PACKAGE_NAME))
			.isPresent();
	}

	private boolean isNotProxy(Object bean) {
		return !isProxy(bean);
	}

	private boolean isProxy(Object bean) {
		return AopUtils.isAopProxy(bean);
	}

	private Object resolveTargetObject(Object bean) {

		return Optional.ofNullable(bean)
			.filter(this::isProxy)
			.map(proxy -> AopProxyUtils.getSingletonTarget(proxy))
			.orElse(bean);
	}

	@Bean
	public ContinuousQueryListenerContainer continuousQueryListenerContainer(GemFireCache gemfireCache) {

		Assert.state(CacheUtils.isClient(gemfireCache),
			"Continuous Queries (CQ) may only be used in a ClientCache application");

		ContinuousQueryListenerContainer container = new ContinuousQueryListenerContainer();

		container.setCache(gemfireCache);
		container.setContinuousQueryListenerContainerConfigurers(resolveContinuousQueryListenerContainerConfigurers());

		resolveErrorHandler().ifPresent(container::setErrorHandler);
		resolvePhase().ifPresent(container::setPhase);
		resolvePoolName().ifPresent(container::setPoolName);
		resolveQueryService().ifPresent(container::setQueryService);
		resolveTaskExecutor().ifPresent(container::setTaskExecutor);

		return container;
	}

	protected List<ContinuousQueryListenerContainerConfigurer> resolveContinuousQueryListenerContainerConfigurers() {

		return Optional.ofNullable(this.configurers)
			.filter(configurers -> !configurers.isEmpty())
			.orElseGet(() ->
				Optional.of(this.getBeanFactory())
					.filter(beanFactory -> beanFactory instanceof ListableBeanFactory)
					.map(beanFactory -> {

						Map<String, ContinuousQueryListenerContainerConfigurer> beansOfType =
							((ListableBeanFactory) beanFactory).getBeansOfType(ContinuousQueryListenerContainerConfigurer.class,
								true, false);

						return nullSafeMap(beansOfType).values().stream().collect(Collectors.toList());

					})
					.orElseGet(Collections::emptyList)
			);
	}

	protected Optional<ErrorHandler> resolveErrorHandler() {

		return Optional.ofNullable(getErrorHandlerBeanName())
			.filter(StringUtils::hasText)
			.map(errorHandlerBeanName -> getBeanFactory().getBean(errorHandlerBeanName, ErrorHandler.class));
	}

	protected Optional<Integer> resolvePhase() {
		return Optional.of(getPhase()).filter(phase -> phase != 0);
	}

	protected Optional<String> resolvePoolName() {
		return Optional.ofNullable(getPoolName()).filter(StringUtils::hasText);
	}

	protected Optional<QueryService> resolveQueryService() {

		return Optional.ofNullable(getQueryServiceBeanName())
			.filter(StringUtils::hasText)
			.map(queryServiceBeanName -> getBeanFactory().getBean(queryServiceBeanName, QueryService.class));
	}

	protected Optional<Executor> resolveTaskExecutor() {

		return Optional.ofNullable(getTaskExecutorBeanName())
			.filter(StringUtils::hasText)
			.map(taskExecutorBeanName -> getBeanFactory().getBean(taskExecutorBeanName, Executor.class));
	}

	public void setErrorHandlerBeanName(String errorHandlerBeanName) {
		this.errorHandlerBeanName = errorHandlerBeanName;
	}

	protected String getErrorHandlerBeanName() {
		return this.errorHandlerBeanName;
	}

	public void setPhase(int phase) {
		this.phase = phase;
	}

	protected int getPhase() {
		return this.phase;
	}

	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	protected String getPoolName() {
		return this.poolName;
	}

	public void setQueryServiceBeanName(String queryServiceBeanName) {
		this.queryServiceBeanName = queryServiceBeanName;
	}

	protected String getQueryServiceBeanName() {
		return this.queryServiceBeanName;
	}

	public void setTaskExecutorBeanName(String taskExecutorBeanName) {
		this.taskExecutorBeanName = taskExecutorBeanName;
	}

	protected String getTaskExecutorBeanName() {
		return this.taskExecutorBeanName;
	}
}
