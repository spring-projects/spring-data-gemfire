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

package org.springframework.data.gemfire.eviction;

import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The EvictionTypeConverter class is a Spring Converter used to convert a String value into
 * a corresponding EvictionType enumerated value.
 *
 * @author John Blum
 * @see EvictionPolicyType
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @since 1.6.0
 */
public class EvictionPolicyConverter extends AbstractPropertyEditorConverterSupport<EvictionPolicyType> {

	/**
	 * Converts the given String into a matching EvictionType.
	 *
	 * @param source the String value to convert into an EvictionType.
	 * @return the EvictionType matching the given String.
	 * @throws java.lang.IllegalArgumentException if the String value does not represent a valid EvictionType.
	 * @see EvictionPolicyType#valueOfIgnoreCase(String)
	 * @see #assertConverted(String, Object, Class)
	 */
	@Override
	public EvictionPolicyType convert(final String source) {
		return assertConverted(source, EvictionPolicyType.valueOfIgnoreCase(source), EvictionPolicyType.class);
	}

}
