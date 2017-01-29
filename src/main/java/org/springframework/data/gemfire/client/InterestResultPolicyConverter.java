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

package org.springframework.data.gemfire.client;

import org.apache.geode.cache.InterestResultPolicy;
import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The InterestResultPolicyConverter class is a Spring Converter and JavaBeans PropertyEditor capable of converting
 * a String into a GemFire InterestResultPolicyConverter.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @see org.apache.geode.cache.InterestResultPolicy
 * @since 1.6.0
 */
public class InterestResultPolicyConverter extends AbstractPropertyEditorConverterSupport<InterestResultPolicy> {

	/**
	 * Converts the given String into an instance of GemFire InterestResultPolicy.
	 *
	 * @param source the String to convert into an InterestResultPolicy value.
	 * @return a GemFire InterestResultPolicy value for the given String.
	 * @throws java.lang.IllegalArgumentException if the String is not a valid GemFire InterestResultPolicy.
	 * @see org.springframework.data.gemfire.client.InterestResultPolicyType#getInterestResultPolicy(InterestResultPolicyType)
	 * @see org.springframework.data.gemfire.client.InterestResultPolicyType#valueOfIgnoreCase(String)
	 * @see #assertConverted(String, Object, Class)
	 * @see org.apache.geode.cache.InterestResultPolicy
	 */
	@Override
	public InterestResultPolicy convert(final String source) {
		return assertConverted(source, InterestResultPolicyType.getInterestResultPolicy(
			InterestResultPolicyType.valueOfIgnoreCase(source)), InterestResultPolicy.class);
	}

}
