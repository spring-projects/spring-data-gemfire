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

package org.springframework.data.gemfire.server;

import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The SubscriptionEvictionPolicyConverter class is a Spring Converter and JavaBeans PropertyEditor for converting
 * Strings into a SubscriptionEvictionPolicy enumerated value.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.server.SubscriptionEvictionPolicy
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class SubscriptionEvictionPolicyConverter extends AbstractPropertyEditorConverterSupport<SubscriptionEvictionPolicy> {

	/**
	 * Converts the given String into a SubscriptionEvictionPolicy enumerated value.
	 *
	 * @param source the String to convert into a SubscriptionEvictionPolicy enum.
	 * @return a SubscriptionEvictionPolicy enumerated value for the given String.
	 * @throws java.lang.IllegalArgumentException if the String is a valid SubscriptionEvictionPolicy
	 * enumerated value.
	 * @see org.springframework.data.gemfire.server.SubscriptionEvictionPolicy#valueOfIgnoreCase(String)
	 * @see #assertConverted(String, Object, Class)
	 */
	@Override
	public SubscriptionEvictionPolicy convert(final String source) {
		return assertConverted(source, SubscriptionEvictionPolicy.valueOfIgnoreCase(source),
			SubscriptionEvictionPolicy.class);
	}

}
