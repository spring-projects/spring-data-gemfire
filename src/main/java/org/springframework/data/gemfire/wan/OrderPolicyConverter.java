/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.wan;

import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

import com.gemstone.gemfire.cache.util.Gateway;

/**
 * The OrderPolicyConverter class is a Spring Converter and JavaBeans PropertyEditor used to convert a String value
 * into an appropriate GemFire Gateway.OrderPolicy enum.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @see org.springframework.data.gemfire.wan.OrderPolicyType
 * @see com.gemstone.gemfire.cache.util.Gateway.OrderPolicy
 * @since 1.7.0
 */
@SuppressWarnings({ "deprecation", "unused" })
public class OrderPolicyConverter extends AbstractPropertyEditorConverterSupport<Gateway.OrderPolicy> {

	/**
	 * Converts the given String into a GemFire Gateway.OrderPolicy enum.
	 *
	 * @param source the String to convert.
	 * @return a GemFire Gateway.OrderPolicy enum for the given String.
	 * @throws java.lang.IllegalArgumentException if the String is not a valid GemFire Gateway.OrderPolicy.
	 * @see org.springframework.data.gemfire.wan.OrderPolicyType#getOrderPolicy()
	 * @see org.springframework.data.gemfire.wan.OrderPolicyType#valueOfIgnoreCase(String)
	 * @see com.gemstone.gemfire.cache.util.Gateway.OrderPolicy
	 */
	@Override
	public Gateway.OrderPolicy convert(final String source) {
		return assertConverted(source, OrderPolicyType.getOrderPolicy(OrderPolicyType.valueOfIgnoreCase(source)),
			Gateway.OrderPolicy.class);
	}

}
