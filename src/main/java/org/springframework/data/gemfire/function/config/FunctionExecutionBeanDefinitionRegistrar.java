/*
 * Copyright 2002-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.config;

import java.util.Set;

import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.context.annotation.ScannedGenericBeanDefinition;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * {@link ImportBeanDefinitionRegistrar} for {code} @EnableGemfireFunctionExecutions {code}
 * Scans for interfaces annotated with one of {code} @OnRegion, @OnServer, @OnServers, @OnMember, @OnMembers {code}
 * @author David Turanski
 *
 */
class FunctionExecutionBeanDefinitionRegistrar implements ImportBeanDefinitionRegistrar {

	/* (non-Javadoc)
	 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar#registerBeanDefinitions(org.springframework.core.type.AnnotationMetadata, org.springframework.beans.factory.support.BeanDefinitionRegistry)
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata annotationMetadata, BeanDefinitionRegistry registry) {
		AbstractFunctionExecutionConfigurationSource configurationSource = new AnnotationFunctionExecutionConfigurationSource(
				annotationMetadata);

		registerBeanDefinitions(configurationSource, registry);

	}

	/*
	 * This registers bean definitions from any function execution configuration source
	 */
	void registerBeanDefinitions(AbstractFunctionExecutionConfigurationSource functionExecutionConfigurationSource,
			BeanDefinitionRegistry registry) {

		for (ScannedGenericBeanDefinition beanDefinition : functionExecutionConfigurationSource.getCandidates(
				new DefaultResourceLoader())) {

			String functionExecutionAnnotation = getFunctionExecutionAnnotation(beanDefinition,
				AnnotationFunctionExecutionConfigurationSource.getFunctionExecutionAnnotationTypeNames());

			Assert.notNull(functionExecutionAnnotation);

			String beanName = (String) beanDefinition.getMetadata().getAnnotationAttributes(
				functionExecutionAnnotation).get("id");

			if (!StringUtils.hasText(beanName)) {
				beanName = BeanDefinitionReaderUtils.generateBeanName(beanDefinition, registry);
			}

			AbstractFunctionExecutionBeanDefinitionBuilder builder = FunctionExecutionBeanDefinitionBuilderFactory
				.newInstance(new FunctionExecutionConfiguration(beanDefinition, functionExecutionAnnotation));

			registry.registerBeanDefinition(beanName, builder.build(registry));
		}
	}

	private String getFunctionExecutionAnnotation(ScannedGenericBeanDefinition beanDefinition,
			Set<String> functionExecutionAnnotationTypeNames) {

		Set<String> annotationTypes = beanDefinition.getMetadata().getAnnotationTypes();

		String functionExecutionAnnotation = null;

		for (String annotationType : annotationTypes) {
			if (functionExecutionAnnotationTypeNames.contains(annotationType)) {
				Assert.isNull(functionExecutionAnnotation, String.format(
					"interface %1$s contains multiple Function Execution Annotations: %2$s, %3$s",
						beanDefinition.getBeanClassName(), functionExecutionAnnotation, annotationType));
				functionExecutionAnnotation = annotationType;
			}
		}

		return functionExecutionAnnotation;
	}

}
