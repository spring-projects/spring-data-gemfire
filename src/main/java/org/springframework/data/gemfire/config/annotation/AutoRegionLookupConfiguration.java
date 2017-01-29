/*
 * Copyright 2016-2018 the original author or authors.
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

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.core.env.Environment;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.config.support.AutoRegionLookupBeanPostProcessor;
import org.springframework.expression.EvaluationException;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.ParseException;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.expression.spel.support.StandardTypeConverter;
import org.springframework.expression.spel.support.StandardTypeLocator;
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
 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.core.env.Environment
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.annotation.EnableAutoRegionLookup
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @see org.springframework.data.gemfire.config.support.AutoRegionLookupBeanPostProcessor
 * @see org.springframework.expression.ExpressionParser
 * @since 1.9.0
 */
public class AutoRegionLookupConfiguration extends AbstractAnnotationConfigSupport
		implements ImportBeanDefinitionRegistrar {

	private static final boolean DEFAULT_ENABLED = true;

	private static final AtomicBoolean AUTO_REGION_LOOKUP_BEAN_POST_PROCESSOR_REGISTERED = new AtomicBoolean(false);

	private Environment environment;

	private ExpressionParser spelParser = new SpelExpressionParser();

	private StandardEvaluationContext evaluationContext = new StandardEvaluationContext();

	/* (non-Javadoc) */
	@Override
	protected Class getAnnotationType() {
		return EnableAutoRegionLookup.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {

		super.setBeanFactory(beanFactory);

		this.evaluationContext.setBeanResolver(new BeanFactoryResolver(beanFactory));

		if (beanFactory instanceof ConfigurableBeanFactory) {

			ConfigurableBeanFactory configurableBeanFactory = (ConfigurableBeanFactory) beanFactory;

			this.evaluationContext.setTypeLocator(new StandardTypeLocator(
				configurableBeanFactory.getBeanClassLoader()));

			Optional.ofNullable(configurableBeanFactory.getConversionService())
				.ifPresent(conversionService ->
					this.evaluationContext.setTypeConverter(new StandardTypeConverter(conversionService)));
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

		Map<String, Object> enableAutoRegionLookupAttributes =
			importingClassMetadata.getAnnotationAttributes(EnableAutoRegionLookup.class.getName());

		Optional.ofNullable(resolveProperty(propertyName("enable-auto-region-lookup"),
				(Boolean) enableAutoRegionLookupAttributes.get("enabled")))
			.filter(Boolean.TRUE::equals)
			.ifPresent(enabled -> registerAutoRegionLookupBeanPostProcessor(registry));
	}

	/**
	 * (non-Javadoc)
	 *
	 * This method was used to support Spring property placeholders and SpEL Expressions
	 * in the {@link EnableAutoRegionLookup#enabled()} attribute.  However, this required the attribute to be
	 * of type {@link String}, which violates type-safety.  We are favoring type-safety over configuration
	 * flexibility and offering alternative means to achieve flexible and dynamic configuration, e.g. properties
	 * from an {@literal application.properties} file.
	 */
	private boolean isEnabled(String enabled) {

		enabled = StringUtils.trimWhitespace(enabled);

		if (!Boolean.parseBoolean(enabled)) {
			try {
				// try parsing as a SpEL expression...
				return this.spelParser.parseExpression(enabled).getValue(this.evaluationContext, Boolean.TYPE);
			}
			catch (EvaluationException ignore) {
				return false;
			}
			catch (ParseException ignore) {
				// try resolving as a Spring property placeholder expression...
				return getEnvironment().getProperty(enabled, Boolean.TYPE, false);
			}
		}

		return DEFAULT_ENABLED;
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
