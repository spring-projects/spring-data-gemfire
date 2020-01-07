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

import static org.assertj.core.api.Assertions.assertThat;

import org.apache.geode.cache.wan.GatewaySender;

import org.junit.Test;

/**
 * Unit Tests for {@link OrderPolicyType}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.apache.geode.cache.wan.GatewaySender.OrderPolicy
 * @see org.springframework.data.gemfire.wan.OrderPolicyType
 * @since 1.7.0
 */
@SuppressWarnings("deprecation")
public class OrderPolicyTypeTest {

	@Test
	public void testStaticGetOrderPolicy() {

		assertThat(OrderPolicyType.getOrderPolicy(OrderPolicyType.KEY)).isEqualTo(GatewaySender.OrderPolicy.KEY);
		assertThat(OrderPolicyType.getOrderPolicy(OrderPolicyType.PARTITION)).isEqualTo(GatewaySender.OrderPolicy.PARTITION);
	}

	@Test
	@SuppressWarnings("all")
	public void testStaticGetOrderPolicyWithNull() {
		assertThat(OrderPolicyType.getOrderPolicy(null)).isNull();
	}

	@Test
	public void testValueOfGemFireOrderPolicies() {

		for (GatewaySender.OrderPolicy orderPolicy : GatewaySender.OrderPolicy.values()) {

			OrderPolicyType orderPolicyType = OrderPolicyType.valueOf(orderPolicy);

			assertThat(orderPolicyType).isNotNull();
			assertThat(orderPolicyType.getOrderPolicy()).isEqualTo(orderPolicy);
		}
	}

	@Test
	public void testValueOfNullGemFireOrderPolicy() {
		assertThat(OrderPolicyType.valueOf((GatewaySender.OrderPolicy) null)).isNull();
	}

	@Test
	public void testValueOfIgnoreCase() {

		assertThat(OrderPolicyType.valueOfIgnoreCase("KEY")).isEqualTo(OrderPolicyType.KEY);
		assertThat(OrderPolicyType.valueOfIgnoreCase("Partition")).isEqualTo(OrderPolicyType.PARTITION);
		assertThat(OrderPolicyType.valueOfIgnoreCase("PARTition")).isEqualTo(OrderPolicyType.PARTITION);
		assertThat(OrderPolicyType.valueOfIgnoreCase("PartItIon")).isEqualTo(OrderPolicyType.PARTITION);
		assertThat(OrderPolicyType.valueOfIgnoreCase("thread")).isEqualTo(OrderPolicyType.THREAD);
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {

		assertThat(OrderPolicyType.valueOfIgnoreCase("KEYZ")).isNull();
		assertThat(OrderPolicyType.valueOfIgnoreCase("Values")).isNull();
		assertThat(OrderPolicyType.valueOfIgnoreCase("invalid")).isNull();
	}
}
