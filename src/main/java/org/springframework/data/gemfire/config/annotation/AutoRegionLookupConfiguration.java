/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.EnvironmentAware;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.env.Environment;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.support.AutoRegionLookupBeanPostProcessor;
import org.springframework.expression.EvaluationException;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.ParseException;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.expression.spel.support.StandardTypeConverter;
import org.springframework.expression.spel.support.StandardTypeLocator;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The {@link AutoRegionLookupConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that enables
 * the automatic lookup of GemFire Regions, which may have been defined else where, such as in {@literal cache.xml}
 * or using GemFire's Cluster Configuration Service.
 *
 * This registrar works by registering the {@link AutoRegionLookupBeanPostProcessor} in the Spring application context,
 * which is enabled when a Spring {@link org.springframework.context.annotation.Configuration @Configuration} annotated
 * GemFire cache application class is annotated with {@link EnableAutoRegionLookup}.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.context.EnvironmentAware
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.core.env.Environment
 * @see org.springframework.data.gemfire.config.annotation.EnableAutoRegionLookup
 * @see org.springframework.data.gemfire.config.support.AutoRegionLookupBeanPostProcessor
 * @since 1.9.0
 */
public class AutoRegionLookupConfiguration implements BeanFactoryAware, EnvironmentAware,
		ImportBeanDefinitionRegistrar {

	private static final AtomicBoolean AUTO_REGION_LOOKUP_BEAN_POST_PROCESSOR_REGISTERED = new AtomicBoolean(false);

	private Environment environment;

	private ExpressionParser spelParser = new SpelExpressionParser();

	private StandardEvaluationContext evaluationContext = new StandardEvaluationContext();

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		evaluationContext.setBeanResolver(new BeanFactoryResolver(beanFactory));

		if (beanFactory instanceof ConfigurableBeanFactory) {
			ConfigurableBeanFactory configurableBeanFactory = (ConfigurableBeanFactory) beanFactory;

			evaluationContext.setTypeLocator(new StandardTypeLocator(configurableBeanFactory.getBeanClassLoader()));

			ConversionService conversionService = configurableBeanFactory.getConversionService();

			if (conversionService != null) {
				evaluationContext.setTypeConverter(new StandardTypeConverter(conversionService));
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}

	/**
	 * Returns a reference to the configured {@link Environment} in the Spring application context.
	 *
	 * @return a reference to the configured {@link Environment}.
	 * @throws IllegalStateException if the {@link Environment} reference was not properly configured.
	 * @see org.springframework.core.env.Environment
	 */
	protected Environment getEnvironment() {
		Assert.state(environment != null, "The Environment was not properly configured");
		return this.environment;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {
		Map<String, Object> enableAutoRegionLookupAttributes =
			importingClassMetadata.getAnnotationAttributes(EnableAutoRegionLookup.class.getName());

		String enabled = (String) enableAutoRegionLookupAttributes.get("enabled");

		if (isEnabled(enabled)) {
			registerAutoRegionLookupBeanPostProcessor(registry);
		}
	}

	/* (non-Javadoc) */
	private boolean isEnabled(String enabled) {
		enabled = StringUtils.trimWhitespace(enabled);

		if (!Boolean.parseBoolean(enabled)) {
			try {
				// try parsing as a SpEL expression...
				return spelParser.parseExpression(enabled).getValue(evaluationContext, Boolean.TYPE);
			}
			catch (EvaluationException ignore) {
				return false;
			}
			catch (ParseException ignore) {
				// try resolving as a Spring property placeholder expression...
				return getEnvironment().getProperty(enabled, Boolean.TYPE, false);
			}
		}

		return true;
	}

	/* (non-Javadoc) */
	private void registerAutoRegionLookupBeanPostProcessor(BeanDefinitionRegistry registry) {
		if (AUTO_REGION_LOOKUP_BEAN_POST_PROCESSOR_REGISTERED.compareAndSet(false, true)) {
			AbstractBeanDefinition autoRegionLookupBeanPostProcessor = BeanDefinitionBuilder
				.rootBeanDefinition(AutoRegionLookupBeanPostProcessor.class)
				.setRole(AbstractBeanDefinition.ROLE_INFRASTRUCTURE)
				.getBeanDefinition();

			BeanDefinitionReaderUtils.registerWithGeneratedName(autoRegionLookupBeanPostProcessor, registry);
		}
	}
}
