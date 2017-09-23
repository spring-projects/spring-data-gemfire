/*
 * Copyright 2017 the original author or authors.
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Executor;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.apache.shiro.util.Assert;
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
import org.springframework.lang.Nullable;
import org.springframework.util.ErrorHandler;
import org.springframework.util.StringUtils;

/**
 * The {@link ContinuousQueryConfiguration} class is a Spring {@link Configuration @Configuration} class enabling
 * Continuous Query (CQ) GemFire/Geode capabilities in this cache client application.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
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
	private String taskExecutorBeanName;

	@Override
	protected Class getAnnotationType() {
		return EnableContinuousQueries.class;
	}

	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {

		if (importMetadata.hasAnnotation(getAnnotationType().getName())) {

			AnnotationAttributes enableContinuousQueriesAttributes =
				AnnotationAttributes.fromMap(importMetadata.getAnnotationAttributes(getAnnotationType().getName()));

			setErrorHandlerBeanName(enableContinuousQueriesAttributes.getString("errorHandlerBeanName"));
			setPhase(enableContinuousQueriesAttributes.<Integer>getNumber("phase"));
			setPoolName(enableContinuousQueriesAttributes.getString("poolName"));
			setTaskExecutorBeanName(enableContinuousQueriesAttributes.getString("taskExecutorBeanName"));
		}
	}

	@Bean
	public BeanPostProcessor continuousQueryBeanPostProcessor() {

		return new BeanPostProcessor() {

			private ContinuousQueryListenerContainer container;

			private List<ContinuousQueryDefinition> continuousQueryDefinitions = new ArrayList<>();

			@Nullable @Override
			public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

				if (bean instanceof ContinuousQueryListenerContainer) {
					this.container = (ContinuousQueryListenerContainer) bean;
					this.continuousQueryDefinitions.forEach(definition -> this.container.addListener(definition));
					this.continuousQueryDefinitions.clear();
				}
				else if (isApplicationBean(bean)) {

					List<ContinuousQueryDefinition> definitions = stream(bean.getClass().getMethods())
						.filter(method -> method.isAnnotationPresent(ContinuousQuery.class))
						.map(method -> ContinuousQueryDefinition.from(bean, method))
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

	private boolean isApplicationBean(Object bean) {

		return Optional.ofNullable(bean)
			.map(Object::getClass)
			.filter(type -> type.getPackage().getName().startsWith(ORG_SPRINGFRAMEWORK_DATA_GEMFIRE_PACKAGE_NAME)
				|| !type.getPackage().getName().startsWith(ORG_SPRINGFRAMEWORK_PACKAGE_NAME))
			.isPresent();
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
								true, true);

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

	protected Optional<Executor> resolveTaskExecutor() {

		return Optional.ofNullable(getTaskExecutorBeanName())
			.filter(StringUtils::hasText)
			.map(taskExecutorBeanName -> getBeanFactory().getBean(taskExecutorBeanName, Executor.class));
	}

	public void setErrorHandlerBeanName(String errorHandlerBeanName) {
		this.errorHandlerBeanName = errorHandlerBeanName;
	}

	protected String getErrorHandlerBeanName() {
		return errorHandlerBeanName;
	}

	public void setPhase(int phase) {
		this.phase = phase;
	}

	protected int getPhase() {
		return phase;
	}

	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	protected String getPoolName() {
		return poolName;
	}

	public void setTaskExecutorBeanName(String taskExecutorBeanName) {
		this.taskExecutorBeanName = taskExecutorBeanName;
	}

	protected String getTaskExecutorBeanName() {
		return taskExecutorBeanName;
	}
}
