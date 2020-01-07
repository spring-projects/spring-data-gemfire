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

package org.springframework.data.gemfire;

import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The IndexMaintenanceTypeConverter class is a Spring Converter and JavaBeans PropertyEditor capable of converting
 * a String into a specific SDG IndexMaintenancePolicyType.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.IndexMaintenancePolicyType
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class IndexMaintenancePolicyConverter extends AbstractPropertyEditorConverterSupport<IndexMaintenancePolicyType> {

	/**
	 * Converts the given String value into an appropriate IndexMaintenancePolicyType.
	 *
	 * @param source the String value to convert into a IndexMaintenancePolicyType.
	 * @return an IndexMaintenancePolicyType converted from the given String value.
	 * @throws java.lang.IllegalArgumentException if the String is not a valid IndexMaintenancePolicyType.
	 * @see org.springframework.data.gemfire.IndexMaintenancePolicyType#valueOfIgnoreCase(String)
	 * @see #assertConverted(String, Object, Class)
	 */
	@Override
	public IndexMaintenancePolicyType convert(final String source) {
		return assertConverted(source, IndexMaintenancePolicyType.valueOfIgnoreCase(source),
			IndexMaintenancePolicyType.class);
	}

}
