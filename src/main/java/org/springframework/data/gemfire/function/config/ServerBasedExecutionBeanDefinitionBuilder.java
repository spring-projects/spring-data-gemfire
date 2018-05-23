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
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.function.annotation.OnServer;
import org.springframework.data.gemfire.function.annotation.OnServers;
import org.springframework.data.gemfire.function.execution.GemfireFunctionProxyFactoryBean;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Base class for {@link OnServer} and {@link OnServers} Function execution
 * {@link BeanDefinitionBuilder BeanDefinitionBuilders}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.function.config.AbstractFunctionExecutionBeanDefinitionBuilder
 */
abstract class ServerBasedExecutionBeanDefinitionBuilder extends AbstractFunctionExecutionBeanDefinitionBuilder {

	ServerBasedExecutionBeanDefinitionBuilder(FunctionExecutionConfiguration configuration) {
		super(configuration);
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.AbstractFunctionExecutionBeanDefinitionBuilder
	 * 	#getGemfireFunctionOperationsBeanDefinitionBuilder(org.springframework.beans.factory.support.BeanDefinitionRegistry)
	 */
	@Override
	protected BeanDefinitionBuilder getGemfireFunctionOperationsBeanDefinitionBuilder(BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder functionTemplateBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(getGemfireFunctionOperationsClass());

		String cache = (String) this.configuration.getAttribute("cache");
		String pool = (String) this.configuration.getAttribute("pool");

		Assert.state(!(StringUtils.hasText(cache) && StringUtils.hasText(pool)),
			String.format("Invalid configuration for interface [%s]; cannot specify both 'pool' and 'cache'",
				this.configuration.getFunctionExecutionInterface().getName()));

		functionTemplateBuilder.addConstructorArgReference(StringUtils.hasText(pool)
			? pool : (StringUtils.hasText(cache) ? cache : GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));

		return functionTemplateBuilder;
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.AbstractFunctionExecutionBeanDefinitionBuilder
	 * 	#getFunctionProxyFactoryBeanClass()
	 */
	@Override
	protected Class<?> getFunctionProxyFactoryBeanClass() {
		return GemfireFunctionProxyFactoryBean.class;
	}

	protected abstract Class<?> getGemfireFunctionOperationsClass();

}
