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

/**
 * The ExpirationActionTypeConverter class is a Spring Converter used to convert a String value into
 * a corresponding ExpirationActionType enumerated value.
 *
 * @author John Blum
 * @see java.beans.PropertyEditorSupport
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.gemfire.ExpirationActionType
 * @since 1.6.0
 */
public class ExpirationActionTypeConverter extends PropertyEditorSupport implements Converter<String, ExpirationActionType> {

	/* non-Javadoc */
	private ExpirationActionType assertConverted(final String source, final ExpirationActionType expirationActionType) {
		if (expirationActionType == null) {
			throw new IllegalArgumentException(String.format("Source (%1$s) is not a valid ExpirationActionType!",
				source));
		}

		return expirationActionType;
	}

	/**
	 * Sets the ExpirationActionType by parsing a given String. May raise a java.lang.IllegalArgumentException
	 * if either the String is badly formatted or the text cannot be expressed as a ExpirationActionType.
	 *
	 * @param text the String value to express as (convert to) a ExpirationActionType.
	 * @throws java.lang.IllegalArgumentException if the String value does not represent a valid ExpirationActionType.
	 * @see #convert(String)
	 * @see #setValue(Object)
	 */
	@Override
	public void setAsText(final String text) throws IllegalArgumentException {
		setValue(convert(text));
	}

	/**
	 * Converts the given String value into an appropriate ExpirationActionType.
	 *
	 * @param source the String value to convert into an ExpirationActionType.
	 * @return an ExpirationActionType for the given String value.
	 * @throws java.lang.IllegalArgumentException if the String value does not represent a valid ExpirationActionType.
	 * @see org.springframework.data.gemfire.ExpirationActionType#valueOfIgnoreCase(String)
	 */
	@Override
	public ExpirationActionType convert(final String source) {
		return assertConverted(source, ExpirationActionType.valueOfIgnoreCase(source));
	}

}
