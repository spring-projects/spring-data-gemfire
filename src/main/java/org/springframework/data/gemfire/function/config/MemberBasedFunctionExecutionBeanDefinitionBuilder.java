/*
 * Copyright 2002-2019 the original author or authors.
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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.data.gemfire.function.annotation.OnMember;
import org.springframework.data.gemfire.function.annotation.OnMembers;
import org.springframework.data.gemfire.function.execution.GemfireFunctionProxyFactoryBean;
import org.springframework.util.StringUtils;

/**
 * Base class for {@link OnMember} and {@link OnMembers} Function execution
 * {@link BeanDefinitionBuilder BeanDefinitionBuilders}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.function.config.AbstractFunctionExecutionBeanDefinitionBuilder
 */
abstract class MemberBasedFunctionExecutionBeanDefinitionBuilder
		extends AbstractFunctionExecutionBeanDefinitionBuilder {

	MemberBasedFunctionExecutionBeanDefinitionBuilder(FunctionExecutionConfiguration configuration) {
		super(configuration);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.AbstractFunctionExecutionBeanDefinitionBuilder#getGemfireFunctionOperationsBeanDefinitionBuilder(org.springframework.beans.factory.support.BeanDefinitionRegistry)
	 */
	@Override
	protected BeanDefinitionBuilder getGemfireFunctionOperationsBeanDefinitionBuilder(BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder functionTemplateBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(getGemfireOperationsClass());

		Optional.ofNullable(this.configuration.getAttribute("groups"))
			.map(String::valueOf)
			.map(StringUtils::trimAllWhitespace)
			.filter(StringUtils::hasText)
			.map(StringUtils::commaDelimitedListToStringArray)
			.ifPresent(functionTemplateBuilder::addConstructorArgValue);

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
