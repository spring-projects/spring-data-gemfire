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

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Map;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.util.Assert;
import org.springframework.util.ReflectionUtils;

/**
 * @author David Turanski
 *
 */
public class GemfireFunctionBeanPostProcessor implements BeanPostProcessor {
 
	private static final String GEMFIRE_FUNCTION_ANNOTATION_NAME = GemfireFunction.class.getName();
	private static final String REGION_DATA_ANNOTATION_NAME = RegionData.class.getName();

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanPostProcessor#postProcessBeforeInitialization(java.lang.Object, java.lang.String)
	 */
	@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
		return bean;
	}

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanPostProcessor#postProcessAfterInitialization(java.lang.Object, java.lang.String)
	 */
	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {

		Method[] methods = ReflectionUtils.getAllDeclaredMethods(bean.getClass());
		
		for (Method method: methods) {
			GemfireFunction annotation = AnnotationUtils.getAnnotation(method, GemfireFunction.class);
			if (annotation != null) {
				Assert.isTrue(Modifier.isPublic(method.getModifiers()),"The method " + method.getName()+ " annotated with" + GEMFIRE_FUNCTION_ANNOTATION_NAME+ " must be public");
				Map<String,Object> attributes = AnnotationUtils.getAnnotationAttributes(annotation,false,true);
				
				processParameterAnnotations(method);
				
				GemfireFunctionUtils.registerFunctionForPojoMethod(bean, method, attributes, false);
			}	
		}
		
		return bean;
	}

	private void processParameterAnnotations(Method method) {
		Annotation[][] parameterAnnotations = method.getParameterAnnotations();
		if (parameterAnnotations.length == 0) {
			return;
		}
		
		Class<?>[] paramTypes = method.getParameterTypes();
		
		for (int i=0; i< parameterAnnotations.length; i++) {
			Annotation[] annotations  = parameterAnnotations[i];
			if (annotations.length > 0) {
				System.out.println("found annotations for parameter in position " + i + " " + paramTypes[i] );
				for (Annotation annotation:annotations) {
					System.out.println("Annotation type:" + annotation.annotationType().getName());
				}
			}
			
		}
		
	}
}
