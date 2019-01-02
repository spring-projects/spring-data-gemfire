/*
<<<<<<< Updated upstream
 * Copyright 2002-2019 the original author or authors.
=======
 * Copyright 2002-2019 the original author or authors.
>>>>>>> Stashed changes
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

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.util.Optional;
import java.util.Set;

import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.context.annotation.ScannedGenericBeanDefinition;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.function.annotation.OnMember;
import org.springframework.data.gemfire.function.annotation.OnMembers;
import org.springframework.data.gemfire.function.annotation.OnRegion;
import org.springframework.data.gemfire.function.annotation.OnServer;
import org.springframework.data.gemfire.function.annotation.OnServers;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

/**
 * {@link ImportBeanDefinitionRegistrar} for {@link EnableGemfireFunctionExecutions}, which scans for interfaces
 * annotated with one of {@link OnRegion}, {@link OnServer}, {@link OnServers}, {@link OnMember}, {@link OnMembers}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.context.annotation.ScannedGenericBeanDefinition
 */
public class FunctionExecutionBeanDefinitionRegistrar implements ImportBeanDefinitionRegistrar {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
	 * 	#registerBeanDefinitions(org.springframework.core.type.AnnotationMetadata, org.springframework.beans.factory.support.BeanDefinitionRegistry)
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata annotationMetadata, BeanDefinitionRegistry registry) {

		AbstractFunctionExecutionConfigurationSource configurationSource =
			newAnnotationBasedFunctionExecutionConfigurationSource(annotationMetadata);

		registerBeanDefinitions(configurationSource, registry);

	}

	protected AbstractFunctionExecutionConfigurationSource newAnnotationBasedFunctionExecutionConfigurationSource(
			AnnotationMetadata annotationMetadata) {

		return new AnnotationFunctionExecutionConfigurationSource(annotationMetadata);
	}

	protected void registerBeanDefinitions(Element element, ParserContext parserContext) {

		AbstractFunctionExecutionConfigurationSource configurationSource =
			newXmlBasedFunctionExecutionConfigurationSource(element, parserContext);

		registerBeanDefinitions(configurationSource, parserContext.getRegistry());
	}

	protected AbstractFunctionExecutionConfigurationSource newXmlBasedFunctionExecutionConfigurationSource(
			Element element, ParserContext parserContext) {

		return new XmlFunctionExecutionConfigurationSource(element, parserContext);
	}

	/**
	 * Registers bean definitions from any {@link FunctionExecutionConfigurationSource}.
	 */
	void registerBeanDefinitions(AbstractFunctionExecutionConfigurationSource functionExecutionConfigurationSource,
			BeanDefinitionRegistry registry) {

		Set<String> functionExecutionAnnotationTypeNames =
			AbstractFunctionExecutionConfigurationSource.getFunctionExecutionAnnotationTypeNames();

		for (ScannedGenericBeanDefinition beanDefinition : functionExecutionConfigurationSource
				.getCandidates(new DefaultResourceLoader())) {

			String functionExecutionAnnotation =
				Optional.ofNullable(getFunctionExecutionAnnotation(beanDefinition, functionExecutionAnnotationTypeNames))
					.orElseThrow(() -> newIllegalStateException(String.format("No Function Execution Annotation [%1$s] found for type [%2$s]",
						functionExecutionAnnotationTypeNames, beanDefinition.getBeanClassName())));

			String beanName = Optional.of(beanDefinition.getMetadata())
				.map(annotationMetadata -> annotationMetadata.getAnnotationAttributes(functionExecutionAnnotation))
				.map(AnnotationAttributes::fromMap)
				.map(annotationAttributes -> annotationAttributes.getString("id"))
				.filter(StringUtils::hasText)
				.orElseGet(() -> BeanDefinitionReaderUtils.generateBeanName(beanDefinition, registry));

			AbstractFunctionExecutionBeanDefinitionBuilder builder = FunctionExecutionBeanDefinitionBuilderFactory
				.newInstance(new FunctionExecutionConfiguration(beanDefinition, functionExecutionAnnotation));

			registry.registerBeanDefinition(beanName, builder.build(registry));
		}
	}

	private String getFunctionExecutionAnnotation(ScannedGenericBeanDefinition beanDefinition,
			Set<String> functionExecutionAnnotationTypeNames) {

		String existingFunctionExecutionAnnotation = null;

		for (String annotationType : beanDefinition.getMetadata().getAnnotationTypes()) {
			if (functionExecutionAnnotationTypeNames.contains(annotationType)) {

				Assert.isNull(existingFunctionExecutionAnnotation,
					String.format("Interface [%1$s] contains multiple Function Execution Annotations: %2$s, %3$s",
						beanDefinition.getBeanClassName(), existingFunctionExecutionAnnotation, annotationType));

				existingFunctionExecutionAnnotation = annotationType;
			}
		}

		return existingFunctionExecutionAnnotation;
	}
}
