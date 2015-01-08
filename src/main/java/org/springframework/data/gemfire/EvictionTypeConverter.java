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

/**
 * The EvictionTypeConverter class is a Spring Converter used to convert a String value into
 * a corresponding EvictionType enumerated value.
 *
 * @author John Blum
 * @see java.beans.PropertyEditorSupport
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.gemfire.EvictionType
 * @since 1.6.0
 */
public class EvictionTypeConverter extends PropertyEditorSupport implements Converter<String, EvictionType> {

	/* non-javadoc */
	private EvictionType assertConverted(final String source, final EvictionType evictionType) {
		Assert.notNull(evictionType, String.format("Source (%1$s) is not a valid EvictionType!", source));
		return evictionType;
	}

	/**
	 * Converts the given String value into an appropriate ExpirationActionType.
	 *
	 * @param source the String value to convert into an ExpirationActionType.
	 * @return an ExpirationActionType for the given String value.
	 * @throws java.lang.IllegalArgumentException if the String value does not represent a valid ExpirationActionType.
	 * @see org.springframework.data.gemfire.EvictionType#valueOfIgnoreCase(String)
	 */
	@Override
	public EvictionType convert(final String source) {
		return assertConverted(source, EvictionType.valueOfIgnoreCase(source));
	}

	/**
	 * Sets the EvictionType by parsing the given text String. May throw a java.lang.IllegalArgumentException
	 * if either the text is badly formatted or the text cannot be expressed as an EvictionType.
	 *
	 * @param text the String value to express as (convert to) an EvictionType.
	 * @throws java.lang.IllegalArgumentException if the String value does not represent a valid EvictionType.
	 * @see #convert(String)
	 * @see #setValue(Object)
	 */
	@Override
	public void setAsText(final String text) throws IllegalArgumentException {
		setValue(convert(text));
	}

}
