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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.data.gemfire.function.execution.GemfireOnRegionFunctionTemplate;
import org.springframework.data.gemfire.function.execution.OnRegionFunctionProxyFactoryBean;

/**
 * @author David Turanski
 * @author John Blum
 */
class OnRegionFunctionExecutionBeanDefinitionBuilder extends AbstractFunctionExecutionBeanDefinitionBuilder {

    OnRegionFunctionExecutionBeanDefinitionBuilder(FunctionExecutionConfiguration configuration) {
		super(configuration);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.AbstractFunctionExecutionBeanDefinitionBuilder
	 * 	#getGemfireFunctionOperationsBeanDefinitionBuilder(org.springframework.beans.factory.support.BeanDefinitionRegistry)
	 */
	@Override
	protected BeanDefinitionBuilder getGemfireFunctionOperationsBeanDefinitionBuilder(BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder functionTemplateBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(GemfireOnRegionFunctionTemplate.class);

		String regionBeanName = String.valueOf(this.configuration.getAttribute("region"));

		functionTemplateBuilder.addConstructorArgReference(regionBeanName);

		return functionTemplateBuilder;
	}


	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.AbstractFunctionExecutionBeanDefinitionBuilder
	 * 	#getFunctionProxyFactoryBeanClass()
	 */
	@Override
	protected Class<?> getFunctionProxyFactoryBeanClass() {
		return OnRegionFunctionProxyFactoryBean.class;
	}
}
