/*
 * Copyright 2002-2013 the original author or authors.
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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.data.gemfire.config.GemfireConstants;
import org.springframework.data.gemfire.function.execution.GemfireFunctionProxyFactoryBean;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * @author David Turanski
 *
 */
abstract class ServerBasedExecutionBeanDefinitionBuilder extends AbstractFunctionExecutionBeanDefinitionBuilder {

	/**
	 * @param configuration
	 */
	ServerBasedExecutionBeanDefinitionBuilder(FunctionExecutionConfiguration configuration) {
		super(configuration);
	}

	 
 
	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.AbstractFunctionExecutionBeanDefinitionBuilder#getGemfireOperationsBeanDefinitionBuilder(org.springframework.beans.factory.support.BeanDefinitionRegistry)
	 */
	@Override
	protected BeanDefinitionBuilder getGemfireOperationsBeanDefinitionBuilder(BeanDefinitionRegistry registry) {
		
		BeanDefinitionBuilder functionTemplateBuilder = BeanDefinitionBuilder.genericBeanDefinition(getGemfireOperationsClass());
		
		String pool = (String)configuration.getAttribute("pool");
		String cache = (String)configuration.getAttribute("cache");
		
		Assert.state(!(StringUtils.hasText(pool) && StringUtils.hasText(cache)),
				String.format("invalid configuration for interface %s. Cannot specify both 'pool' and 'cache'",
						configuration.getFunctionExecutionInterface().getName())); 
		
		if (StringUtils.hasText(pool)) {
			
			functionTemplateBuilder.addConstructorArgReference(pool);
		} else {
			if (!StringUtils.hasText(cache)) {
				cache = GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME;
			}
			
			functionTemplateBuilder.addConstructorArgReference(cache);
		}
		return functionTemplateBuilder;
	}


	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.AbstractFunctionExecutionBeanDefinitionBuilder#getFunctionProxyFactoryBeanClass()
	 */
	@Override
	protected Class<?> getFunctionProxyFactoryBeanClass() {
		return GemfireFunctionProxyFactoryBean.class;
	}

	protected abstract Class<?> getGemfireOperationsClass();
}
