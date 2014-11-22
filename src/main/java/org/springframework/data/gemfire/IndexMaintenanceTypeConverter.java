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
 * The IndexMaintenanceTypeConverter class...
 *
 * @author John Blum
 * @see java.beans.PropertyEditorSupport
 * @see org.springframework.core.convert.converter.Converter
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public class IndexMaintenanceTypeConverter extends PropertyEditorSupport implements Converter<String, IndexMaintenanceType> {

	/* (non-Javadoc) */
	private IndexMaintenanceType assertConverted(final String source, final IndexMaintenanceType indexMaintenanceType) {
		if (indexMaintenanceType == null) {
			throw new IllegalArgumentException(String.format("Source (%1$s) is not a valid IndexMaintenanceType!",
				source));
		}

		return indexMaintenanceType;
	}

	/**
	 * Sets the IndexMaintenanceType by parsing a given String. May raise a java.lang.IllegalArgumentException
	 * if either the String is badly formatted or the text cannot be expressed as a IndexMaintenanceType.
	 *
	 * @param text the String value to express (convert) as a IndexMaintenanceType.
	 * @throws java.lang.IllegalArgumentException if the String value does not represent a valid IndexMaintenanceType.
	 * @see #convert(String)
	 * @see #setValue(Object)
	 */
	@Override
	public void setAsText(final String text) throws IllegalArgumentException {
		setValue(convert(text));
	}

	/**
	 * Converts the given String value into an appropriate IndexMaintenanceType.
	 *
	 * @param source the String value to convert into a IndexMaintenanceType.
	 * @return a IndexMaintenanceType for the given String value.
	 * @throws java.lang.IllegalArgumentException if the String value does not represent a valid IndexMaintenanceType.
	 * @see org.springframework.data.gemfire.IndexMaintenanceType#valueOfIgnoreCase(String)
	 */
	@Override
	public IndexMaintenanceType convert(final String source) {
		return assertConverted(source, IndexMaintenanceType.valueOfIgnoreCase(source));
	}

}
