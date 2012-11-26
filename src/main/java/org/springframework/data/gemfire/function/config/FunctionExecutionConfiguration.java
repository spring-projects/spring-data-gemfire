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

import java.util.Map;

import org.springframework.context.annotation.ScannedGenericBeanDefinition;
import org.springframework.util.Assert;

/**
 * Function execution configuration used by bean definition builders
 * 
 * @author David Turanski
 *
 */
class FunctionExecutionConfiguration  {
	
	private final Map<String,Object> attributes;
	private Class<?> functionExecutionInterface;
	private final String annotationType;


	FunctionExecutionConfiguration(ScannedGenericBeanDefinition beanDefinition, String annotationType) {
		this.attributes = beanDefinition.getMetadata().getAnnotationAttributes(annotationType,true);
		
		try {
			this.functionExecutionInterface = beanDefinition.resolveBeanClass(beanDefinition.getClass().getClassLoader());
			Assert.isTrue(functionExecutionInterface.isInterface(),
					String.format("The annotation %s only applies to an interface. It is not valid for the type %s",
							annotationType, functionExecutionInterface.getName()));
			
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		}
		this.annotationType = annotationType;
	}
 
	Class<?> getFunctionExecutionInterface() {
		return this.functionExecutionInterface;
	}
	 
	Map<String, Object> getAttributes() {
		return this.attributes;
	}

 
	Object getAttribute(String name) {
		return attributes.get(name);
	}
	
	String getAnnotationType() {
		return this.annotationType;
	}
}
