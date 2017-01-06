/*
 * Copyright 2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.util;

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.util.StringUtils;

/**
 * SpringUtils is a utility class encapsulating common functionality on objects and other class types.
 *
 * @author John Blum
 * @since 1.8.0
 */
@SuppressWarnings("unused")
// TODO rename this utiltiy class using a more descriptive, intuitive and meaningful name
public abstract class SpringUtils {

	/* (non-Javadoc) */
	public static BeanDefinition addDependsOn(BeanDefinition bean, String beanName) {
		List<String> dependsOnList = new ArrayList<>();

		Collections.addAll(dependsOnList, nullSafeArray(bean.getDependsOn(), String.class));
		dependsOnList.add(beanName);
		bean.setDependsOn(dependsOnList.toArray(new String[dependsOnList.size()]));

		return bean;
	}

	/* (non-Javadoc) */
	public static String defaultIfEmpty(String value, String defaultValue) {
		return (StringUtils.hasText(value) ? value : defaultValue);
	}

	/* (non-Javadoc) */
	public static <T> T defaultIfNull(T value, T defaultValue) {
		return (value != null ? value : defaultValue);
	}

	/* (non-Javadoc) */
	public static boolean equalsIgnoreNull(Object obj1, Object obj2) {
		return (obj1 == null ? obj2 == null : obj1.equals(obj2));
	}

	/* (non-Javadoc) */
	public static String dereferenceBean(String beanName) {
		return String.format("%1$s%2$s", BeanFactory.FACTORY_BEAN_PREFIX, beanName);
	}
}
