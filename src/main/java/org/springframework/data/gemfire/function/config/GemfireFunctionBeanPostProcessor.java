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

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.gemfire.function.GemfireFunctionUtils;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;
import org.springframework.util.Assert;
import org.springframework.util.ReflectionUtils;

/**
 * Spring {@link BeanPostProcessor} that discovers bean components wired as Function implementations,
 * i.e. beans containing methods annotated with {@link GemfireFunction}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.data.gemfire.function.annotation.GemfireFunction
 */
public class GemfireFunctionBeanPostProcessor implements BeanPostProcessor {

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanPostProcessor#postProcessAfterInitialization(java.lang.Object, java.lang.String)
	 */
	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {

		registerAnyDeclaredGemfireFunctionAnnotatedMethods(bean);

		return bean;
	}

	private void registerAnyDeclaredGemfireFunctionAnnotatedMethods(Object bean) {

		stream(nullSafeArray(ReflectionUtils.getAllDeclaredMethods(bean.getClass()), Method.class)).forEach(method -> {

			GemfireFunction gemfireFunctionAnnotation = AnnotationUtils.getAnnotation(method, GemfireFunction.class);

			if (gemfireFunctionAnnotation != null) {

				Assert.isTrue(Modifier.isPublic(method.getModifiers()),
					String.format("The bean [%s] method [%s] annotated with [%s] must be public",
						bean.getClass().getName(), method.getName(), GemfireFunction.class.getName()));

				AnnotationAttributes annotationAttributes = AnnotationAttributes.fromMap(
					AnnotationUtils.getAnnotationAttributes(gemfireFunctionAnnotation,false,true));

				GemfireFunctionUtils.registerFunctionForPojoMethod(bean, method, annotationAttributes, false);
			}
		});
	}
}
