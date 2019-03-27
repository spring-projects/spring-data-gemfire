/*
 * Copyright 2010-2013 the original author or authors.
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
	 * Asserts that the given String was successfully converted into an instance of Class type T.
	 *
	 * @param source the String to convert.
	 * @param convertedValue the value of the String converted into instance of Class type T.
	 * @param type the target Class type of the converted value.
	 * @return the converted value of Class type T if not null.
	 * @throws java.lang.IllegalArgumentException if the String could not be converted into
	 * an instance of Class type T.
	 */
	protected T assertConverted(final String source, final T convertedValue, final Class<T> type) {
		Assert.notNull(convertedValue, String.format("(%1$s) is not a valid %2$s!", source, type.getSimpleName()));
		return convertedValue;
	}

	/**
	 * Sets the value of this PropertyEditor with the given String converted to the appropriate Class type.
	 *
	 * @param text the String to convert.
	 * @throws java.lang.IllegalArgumentException if the String could not be converted into
	 * an instance of Class type T.
	 * @see #convert(Object)
	 * @see #setValue(Object)
	 */
	@Override
	public void setAsText(final String text) throws IllegalArgumentException {
		setValue(convert(text));
	}

}
