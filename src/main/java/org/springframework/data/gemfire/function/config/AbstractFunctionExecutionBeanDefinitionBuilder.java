/*
 * Copyright 2002-2012 the original author or authors.
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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.util.Assert;
/**
 * Base class for function execution bean definition builders
 * @author David Turanski
 *
 */
abstract class AbstractFunctionExecutionBeanDefinitionBuilder {
	
	protected final Log log = LogFactory.getLog(this.getClass());
	protected final FunctionExecutionConfiguration configuration;
	

	/**
	 * 
	 * @param configuration the configuration values
	 */
	 AbstractFunctionExecutionBeanDefinitionBuilder(FunctionExecutionConfiguration configuration) {
		Assert.notNull(configuration);
		this.configuration = configuration;
	}
	
	/**
	 * Build the bean definition
	 * @param registry
	 * @return
	 */
	 BeanDefinition build(BeanDefinitionRegistry registry) {
		
		BeanDefinitionBuilder builder = BeanDefinitionBuilder.rootBeanDefinition(getFunctionProxyFactoryBeanClass());
	
		builder.addConstructorArgValue(configuration.getFunctionExecutionInterface());
		
		BeanDefinitionBuilder functionTemplateBuilder = getGemfireOperationsBeanDefinitionBuilder(registry);
		functionTemplateBuilder.setLazyInit(true);
		
		AbstractBeanDefinition functionTemplate = functionTemplateBuilder
				.getBeanDefinition();
		
		String functionTemplateName = BeanDefinitionReaderUtils.registerWithGeneratedName(functionTemplate, registry);
		
		builder.addConstructorArgReference(functionTemplateName);
		
//		builder.addConstructorArgValue(functionTemplate);
		
		
		return builder.getBeanDefinition();
		 
	}	
	 
	/*
	 * Subclasses implement to specify the types to uses. 
	 */
	protected abstract Class<?> getFunctionProxyFactoryBeanClass();
	protected abstract BeanDefinitionBuilder getGemfireOperationsBeanDefinitionBuilder(BeanDefinitionRegistry registry);
}
