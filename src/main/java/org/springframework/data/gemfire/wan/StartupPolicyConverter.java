/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.wan;

import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The StartupPolicyConverter class is a Spring Converter and JavaBeans PropertyEditor responsible for
 * converting String values into appropriate StartupPolicyType enumerated values.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @see org.springframework.data.gemfire.wan.StartupPolicyType
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class StartupPolicyConverter extends AbstractPropertyEditorConverterSupport<StartupPolicyType> {

	/**
	 * Converts the given String value into an appropriate StartupPolicyType enumerated value.
	 *
	 * @param source the String to convert.
	 * @return a StartupPolicyType enumerated value for the given String.
	 * @throws java.lang.IllegalArgumentException if the String is not a valid GatewayHub Startup Policy.
	 * @see StartupPolicyType#valueOfIgnoreCase(String)
	 * @see #assertConverted(String, Object, Class)
	 */
	@Override
	public StartupPolicyType convert(final String source) {
		return assertConverted(source, StartupPolicyType.valueOfIgnoreCase(source),
			StartupPolicyType.class);
	}

}
