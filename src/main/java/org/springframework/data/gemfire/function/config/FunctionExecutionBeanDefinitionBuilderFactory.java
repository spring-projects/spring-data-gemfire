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

import org.springframework.data.gemfire.function.annotation.OnMember;
import org.springframework.data.gemfire.function.annotation.OnMembers;
import org.springframework.data.gemfire.function.annotation.OnRegion;
import org.springframework.data.gemfire.function.annotation.OnServer;
import org.springframework.data.gemfire.function.annotation.OnServers;

/**
 * Maps the Function Execution Annotation type to the corresponding Function Execution bean definition builder.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.function.annotation.OnMember
 * @see org.springframework.data.gemfire.function.annotation.OnMembers
 * @see org.springframework.data.gemfire.function.annotation.OnRegion
 * @see org.springframework.data.gemfire.function.annotation.OnServer
 * @see org.springframework.data.gemfire.function.annotation.OnServers
 * @see org.springframework.data.gemfire.function.config.OnMemberExecutionBeanDefinitionBuilder
 * @see org.springframework.data.gemfire.function.config.OnMembersExecutionBeanDefinitionBuilder
 * @see org.springframework.data.gemfire.function.config.OnRegionExecutionBeanDefinitionBuilder
 * @see org.springframework.data.gemfire.function.config.OnServerExecutionBeanDefinitionBuilder
 * @see org.springframework.data.gemfire.function.config.OnServersExecutionBeanDefinitionBuilder
 */
abstract class FunctionExecutionBeanDefinitionBuilderFactory {

	static AbstractFunctionExecutionBeanDefinitionBuilder newInstance(FunctionExecutionConfiguration configuration) {
		String functionExecutionAnnotation = configuration.getAnnotationType();

		if (OnMember.class.getName().equals(functionExecutionAnnotation)) {
			return new OnMemberExecutionBeanDefinitionBuilder(configuration);
		}
		else if (OnMembers.class.getName().equals(functionExecutionAnnotation)) {
			return new OnMembersExecutionBeanDefinitionBuilder(configuration);
		}
		else if (OnRegion.class.getName().equals(functionExecutionAnnotation)) {
			return new OnRegionExecutionBeanDefinitionBuilder(configuration);
		}
		else if (OnServer.class.getName().equals(functionExecutionAnnotation)) {
			return new OnServerExecutionBeanDefinitionBuilder(configuration);
		}
		else if (OnServers.class.getName().equals(functionExecutionAnnotation)) {
			return new OnServersExecutionBeanDefinitionBuilder(configuration);
		}

		return null;
	}

}
