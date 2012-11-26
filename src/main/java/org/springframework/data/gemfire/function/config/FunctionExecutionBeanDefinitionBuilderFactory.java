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

/**
 * 
 * Maps the annotation type to the corresponding function execution bean definition builder
 * @author David Turanski
 *
 */
abstract class  FunctionExecutionBeanDefinitionBuilderFactory {

	static AbstractFunctionExecutionBeanDefinitionBuilder newInstance(FunctionExecutionConfiguration configuration) {
		String functionExectionAnnotation = configuration.getAnnotationType();
		if (functionExectionAnnotation.equals(OnRegion.class.getName())) {
			return new OnRegionExecutionBeanDefinitionBuilder(configuration);
		} 
		if (functionExectionAnnotation.equals(OnServer.class.getName())) {
			return new OnServerExecutionBeanDefinitionBuilder(configuration);
		} 
		if (functionExectionAnnotation.equals(OnServers.class.getName())) {
			return new OnServersExecutionBeanDefinitionBuilder(configuration);
		} 
		if (functionExectionAnnotation.equals(OnMember.class.getName())) {
			return new OnMemberExecutionBeanDefinitionBuilder(configuration);
		} 
		if (functionExectionAnnotation.equals(OnMembers.class.getName())) {
			return new OnMembersExecutionBeanDefinitionBuilder(configuration);
		} 
		return null;	
	}
}
