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
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.config;

import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Base class for Function Execution bean definition builders.
 *
 * @author David Turanski
 * @author John Blum
 */
abstract class AbstractFunctionExecutionBeanDefinitionBuilder {

	protected final FunctionExecutionConfiguration configuration;

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	 AbstractFunctionExecutionBeanDefinitionBuilder(FunctionExecutionConfiguration configuration) {

		Assert.notNull(configuration, "FunctionExecutionConfiguration must not be null");

		this.configuration = configuration;
	}

	 BeanDefinition build(BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder functionProxyFactoryBeanBuilder =
			BeanDefinitionBuilder.rootBeanDefinition(getFunctionProxyFactoryBeanClass());

		functionProxyFactoryBeanBuilder.addConstructorArgValue(this.configuration.getFunctionExecutionInterface());
		functionProxyFactoryBeanBuilder.addConstructorArgReference(BeanDefinitionReaderUtils
			.registerWithGeneratedName(buildGemfireFunctionOperations(registry), registry));

		return functionProxyFactoryBeanBuilder.getBeanDefinition();
	}

	protected AbstractBeanDefinition buildGemfireFunctionOperations(BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder functionTemplateBuilder = getGemfireFunctionOperationsBeanDefinitionBuilder(registry);

		functionTemplateBuilder.setLazyInit(true);

		Optional.ofNullable(this.configuration.getAttribute("resultCollector"))
			.map(String::valueOf)
			.filter(StringUtils::hasText)
			.ifPresent(reference -> functionTemplateBuilder.addPropertyReference("resultCollector", reference));

		return functionTemplateBuilder.getBeanDefinition();
	}

	protected abstract Class<?> getFunctionProxyFactoryBeanClass();

	protected abstract BeanDefinitionBuilder getGemfireFunctionOperationsBeanDefinitionBuilder(BeanDefinitionRegistry registry);

}
