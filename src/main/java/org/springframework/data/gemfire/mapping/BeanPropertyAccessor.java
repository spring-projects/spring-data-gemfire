/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.mapping;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import org.springframework.core.convert.ConversionService;
import org.springframework.data.mapping.PersistentProperty;
import org.springframework.data.mapping.PersistentPropertyAccessor;
import org.springframework.data.mapping.model.ConvertingPropertyAccessor;
import org.springframework.data.mapping.model.MappingException;
import org.springframework.util.Assert;
import org.springframework.util.ReflectionUtils;

/**
 * The BeanPropertyAccessor class is an implementation of Spring Data Common's PersistentPropertyAccessor for accessing
 * persistent properties on beans.
 *
 * @author John Blum
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Method
 * @see org.springframework.data.mapping.PersistentProperty
 * @see org.springframework.data.mapping.PersistentPropertyAccessor
 * @see org.springframework.data.mapping.model.ConvertingPropertyAccessor
 * @since 1.7.0
 */
class BeanPropertyAccessor implements PersistentPropertyAccessor {

	private final Object bean;

	private BeanPropertyAccessor(final Object bean) {
		Assert.notNull(bean, "Bean must not be null");
		this.bean = bean;
	}

	public static PersistentPropertyAccessor create(Object bean) {
		return new BeanPropertyAccessor(bean);
	}

	public static PersistentPropertyAccessor create(Object bean, ConversionService conversionService) {
		return new ConvertingPropertyAccessor(new BeanPropertyAccessor(bean), conversionService);
	}

	@Override
	public void setProperty(PersistentProperty<?> property, Object value) {
		try {
			if (property.usePropertyAccess() && property.getSetter() != null) {
				Method setter = property.getSetter();
				ReflectionUtils.makeAccessible(setter);
				ReflectionUtils.invokeMethod(setter, getBean(), value);
			}
			else {
				Field field = property.getField();
				ReflectionUtils.makeAccessible(field);
				ReflectionUtils.setField(field, getBean(), value);
			}
		}
		catch (Throwable t) {
			throw new MappingException(String.format("Failed to set property (%1$s) to value (%2$s)!",
				property, value), t);
		}
	}

	@Override
	public Object getProperty(PersistentProperty<?> property) {
		try {
			if (property.usePropertyAccess() && property.getGetter() != null) {
				Method getter = property.getGetter();
				ReflectionUtils.makeAccessible(getter);
				return ReflectionUtils.invokeMethod(getter, getBean());
			}
			else {
				Field field = property.getField();
				ReflectionUtils.makeAccessible(field);
				return ReflectionUtils.getField(field, getBean());
			}
		}
		catch (Throwable t) {
			throw new MappingException(String.format("Failed to get value of property (%1$s)!", property), t);
		}
	}

	@Override
	public Object getBean() {
		return bean;
	}

}
