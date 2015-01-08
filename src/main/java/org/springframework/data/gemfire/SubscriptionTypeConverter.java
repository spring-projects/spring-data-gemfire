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
 * The SubscriptionTypeConverter class is a Spring Converter implementation and Java PropertyEditor handling
 * the conversion between Strings and SubscriptionType enumerated values.
 *
 * @author John Blum
 * @see java.beans.PropertyEditorSupport
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.gemfire.SubscriptionType
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class SubscriptionTypeConverter extends PropertyEditorSupport implements Converter<String, SubscriptionType> {

	/* non-Javadoc */
	private SubscriptionType assertConverted(final String source, final SubscriptionType subscriptionType) {
		Assert.notNull(subscriptionType, String.format("Source (%1$s) is not a valid SubscriptionType!", source));
		return subscriptionType;
	}

	/**
	 * Converts the given String into a SubscriptionType enumerated value.
	 *
	 * @param source the String value to convert into a corresponding SubscriptionType enumerated value.
	 * @return a SubscriptionType enumerated value given a String representation.
	 * @throws java.lang.IllegalArgumentException if the String does not represent a valid SubscriptionType.
	 * @see #assertConverted(String, SubscriptionType)
	 * @see org.springframework.data.gemfire.SubscriptionType#valueOfIgnoreCase(String)
	 */
	@Override
	public SubscriptionType convert(final String source) {
		return assertConverted(source, SubscriptionType.valueOfIgnoreCase(source));
	}

	/**
	 * Sets the SubscriptionType by parsing the given text String. May throw a java.lang.IllegalArgumentException
	 * if either the text is badly formatted or the text cannot be expressed as a SubscriptionType.
	 *
	 * @param text the String value to express as (convert to) a SubscriptionType.
	 * @throws java.lang.IllegalArgumentException if the String value does not represent a valid SubscriptionType.
	 * @see #convert(String)
	 * @see #setValue(Object)
	 */
	@Override
	public void setAsText(final String text) throws IllegalArgumentException {
		setValue(convert(text));
	}

}
