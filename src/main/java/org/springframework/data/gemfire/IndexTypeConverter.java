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

package org.springframework.data.gemfire;

import java.beans.PropertyEditorSupport;

import org.springframework.core.convert.converter.Converter;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The IndexTypeConverter class is a Spring Converter implementation as well as a JavaBeans PropertyEditor
 * that converts a given String value into a proper IndexType.
 *
 * @author John Blum
 * @see java.beans.PropertyEditorSupport
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.gemfire.IndexType
 * @since 1.5.2
 */
@SuppressWarnings("unused")
public class IndexTypeConverter extends PropertyEditorSupport implements Converter<String, IndexType> {

	/**
	 * Asserts that the given String value was successfully converted into a IndexType.
	 *
	 * @param value the String value to convert into an appropriate IndexType.
	 * @param indexType the converted IndexType.
	 * @return the IndexType is non-null.
	 * @throws java.lang.IllegalArgumentException if the IndexType argument was null, indicating that
	 * the given String value could not be converted into an appropriate IndexType.
	 * @see org.springframework.data.gemfire.IndexType
	 */
	private IndexType assertConverted(final String value, final IndexType indexType) {
		Assert.notNull(indexType, String.format("Failed to convert String (%1$s) into an IndexType!", value));
		return indexType;
	}

	/**
	 * Converts the given String value into an appropriate IndexType
	 *
	 * @param value the String to convert into a corresponding IndexType enumerated value.
	 * @return an IndexType converted from the given String value.
	 * @throws java.lang.IllegalArgumentException if the given String could not be converted into
	 * an appropriate IndexType enumerated value.
	 * @see #assertConverted(String, IndexType)
	 * @see org.springframework.data.gemfire.IndexType#valueOfIgnoreCase(String)
	 * @see org.springframework.util.StringUtils#trimWhitespace(String)
	 */
	@Override
	public IndexType convert(final String value) {
		return assertConverted(value, IndexType.valueOfIgnoreCase(StringUtils.trimWhitespace(value)));
	}

	/**
	 * Sets the value of this PropertyEditor as a IndexType enumerated value converted from the given String text.
	 *
	 * @param text the String to convert into a corresponding IndexType enumerated value.
	 * @throws java.lang.IllegalArgumentException if the given String could not be converted into
	 * an appropriate IndexType enumerated value.
	 * @see #convert(String)
	 * @see #setValue(Object)
	 */
	@Override
	public void setAsText(final String text) throws IllegalArgumentException {
		setValue(convert(text));
	}

}
