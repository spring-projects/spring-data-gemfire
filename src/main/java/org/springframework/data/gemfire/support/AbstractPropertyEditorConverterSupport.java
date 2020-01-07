/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.support;

import java.beans.PropertyEditorSupport;

import org.springframework.core.convert.converter.Converter;
import org.springframework.util.Assert;

/**
 * The AbstractPropertyEditorConverterSupport class is an abstract base class for Spring Converter implementations
 * that also implement the JavaBeans PropertyEditor interface.
 *
 * @author John Blum
 * @see java.beans.PropertyEditorSupport
 * @see org.springframework.core.convert.converter.Converter
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public abstract class AbstractPropertyEditorConverterSupport<T> extends PropertyEditorSupport
		implements Converter<String, T> {

	/**
	 * Asserts that the given {@link String} was converted into an instance of {@link Class type} T.
	 *
	 * @param source {@link String} to convert.
	 * @param convertedValue converted value of {@link Class type} T.
	 * @param type {@link Class type} of the converted value.
	 * @return the converted value.
	 * @throws java.lang.IllegalArgumentException if the {@link String} could not be converted into
	 * an instance of {@link Class type} T.
	 */
	protected T assertConverted(String source, T convertedValue, Class<T> type) {
		Assert.notNull(convertedValue, String.format("[%1$s] is not a valid %2$s", source, type.getSimpleName()));
		return convertedValue;
	}

	/**
	 * Sets the value of this {@link java.beans.PropertyEditor} to the given {@link String}
	 * converted to the appropriate {@link Class type}.
	 *
	 * @param text {@link String} to convert.
	 * @throws java.lang.IllegalArgumentException if the {@link String} could not be converted into
	 * an instance of {@link Class type} T.
	 * @see #convert(Object)
	 * @see #setValue(Object)
	 */
	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		setValue(convert(text));
	}
}
