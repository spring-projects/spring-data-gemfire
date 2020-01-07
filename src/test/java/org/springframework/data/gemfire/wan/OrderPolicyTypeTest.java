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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.apache.geode.cache.util.Gateway;
import org.junit.Test;

/**
 * The OrderPolicyTypeTest class is a test suite of test cases testing the contract and functionality
 * of the OrderPolicyType enum.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.wan.OrderPolicyType
 * @see org.apache.geode.cache.util.Gateway
 * @since 1.7.0
 */
@SuppressWarnings("deprecation")
public class OrderPolicyTypeTest {

	@Test
	public void testStaticGetOrderPolicy() {
		assertEquals(Gateway.OrderPolicy.KEY, OrderPolicyType.getOrderPolicy(OrderPolicyType.KEY));
		assertEquals(Gateway.OrderPolicy.PARTITION, OrderPolicyType.getOrderPolicy(OrderPolicyType.PARTITION));
	}

	@Test
	public void testStaticGetOrderPolicyWithNull() {
		assertNull(OrderPolicyType.getOrderPolicy(null));
	}

	@Test
	public void testValueOfGemFireOrderPolicies() {
		for (Gateway.OrderPolicy orderPolicy : Gateway.OrderPolicy.values()) {
			OrderPolicyType orderPolicyType = OrderPolicyType.valueOf(orderPolicy);

			assertNotNull(orderPolicyType);
			assertEquals(orderPolicy, orderPolicyType.getOrderPolicy());
		}
	}

	@Test
	public void testValueOfNullGemFireOrderPolicy() {
		assertNull(OrderPolicyType.valueOf((Gateway.OrderPolicy) null));
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(OrderPolicyType.KEY, OrderPolicyType.valueOfIgnoreCase("KEY"));
		assertEquals(OrderPolicyType.PARTITION, OrderPolicyType.valueOfIgnoreCase("Partition"));
		assertEquals(OrderPolicyType.PARTITION, OrderPolicyType.valueOfIgnoreCase("PARTition"));
		assertEquals(OrderPolicyType.PARTITION, OrderPolicyType.valueOfIgnoreCase("PartItIon"));
		assertEquals(OrderPolicyType.THREAD, OrderPolicyType.valueOfIgnoreCase("thread"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(OrderPolicyType.valueOfIgnoreCase("KEYZ"));
		assertNull(OrderPolicyType.valueOfIgnoreCase("Values"));
		assertNull(OrderPolicyType.valueOfIgnoreCase("invalid"));
	}

}
