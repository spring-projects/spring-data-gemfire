/*
 * Copyright 2010-2018 the original author or authors.
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

import org.apache.geode.cache.InterestPolicy;
import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The InterestPolicyConverter class is a Spring Converter implementation and Java PropertyEditor handling
 * the conversion between Strings and GemFire InterestPolicy values.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @see org.apache.geode.cache.InterestPolicy
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class InterestPolicyConverter extends AbstractPropertyEditorConverterSupport<InterestPolicy> {

	/**
	 * Converts the given String into a GemFire InterestPolicy value.
	 *
	 * @param source the String value to convert into a GemFire InterestPolicy value.
	 * @return a GemFire InterestPolicy value for the given String description of the GemFire InterestPolicy
	 * @throws java.lang.IllegalArgumentException if the String is not a valid GemFire InterestPolicy.
	 * @see org.springframework.data.gemfire.InterestPolicyType#getInterestPolicy(InterestPolicyType)
	 * @see org.springframework.data.gemfire.InterestPolicyType#valueOfIgnoreCase(String)
	 * @see #assertConverted(String, Object, Class)
	 */
	@Override
	public InterestPolicy convert(final String source) {
		return assertConverted(source, InterestPolicyType.getInterestPolicy(
			InterestPolicyType.valueOfIgnoreCase(source)), InterestPolicy.class);
	}

}
