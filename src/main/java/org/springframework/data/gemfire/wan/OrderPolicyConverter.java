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
package org.springframework.data.gemfire.wan;

import java.beans.PropertyEditor;

import org.apache.geode.cache.wan.GatewaySender;

import org.springframework.core.convert.converter.Converter;
import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The {@link OrderPolicyConverter} class is a Spring {@link Converter} and JavaBeans {@link PropertyEditor} used to
 * convert a {@link String} into an appropriate {@link GatewaySender.OrderPolicy} enum.
 *
 * @author John Blum
 * @see org.apache.geode.cache.wan.GatewaySender.OrderPolicy
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @see org.springframework.data.gemfire.wan.OrderPolicyType
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class OrderPolicyConverter extends AbstractPropertyEditorConverterSupport<GatewaySender.OrderPolicy> {

	/**
	 * Converts the given String into a Pivotal GemFire Gateway.OrderPolicy enum.
	 *
	 * @param source the String to convert.
	 * @return a Pivotal GemFire Gateway.OrderPolicy enum for the given String.
	 * @throws java.lang.IllegalArgumentException if the String is not a valid Pivotal GemFire Gateway.OrderPolicy.
	 * @see org.springframework.data.gemfire.wan.OrderPolicyType#getOrderPolicy()
	 * @see org.springframework.data.gemfire.wan.OrderPolicyType#valueOfIgnoreCase(String)
	 * @see org.apache.geode.cache.util.Gateway.OrderPolicy
	 */
	@Override
	public GatewaySender.OrderPolicy convert(String source) {

		return assertConverted(source, OrderPolicyType.getOrderPolicy(OrderPolicyType.valueOfIgnoreCase(source)),
			GatewaySender.OrderPolicy.class);
	}
}
