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

import org.springframework.data.gemfire.function.execution.GemfireOnMembersFunctionTemplate;

/**
 * @author David Turanski
 *
 */
class OnMembersExecutionBeanDefinitionBuilder extends MemberBasedExecutionBeanDefinitionBuilder {

	/**
	 * @param configuration
	 */
	OnMembersExecutionBeanDefinitionBuilder(FunctionExecutionConfiguration configuration) {
		super(configuration);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.MemberBasedExecutionBeanDefinitionBuilder#getGemfireFunctionOperationsClass()
	 */
	@Override
	protected Class<?> getGemfireOperationsClass() {
		return GemfireOnMembersFunctionTemplate.class;
	}

}
