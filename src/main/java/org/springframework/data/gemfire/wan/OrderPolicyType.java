/*
 * Copyright 2010-2013 the original author or authors.
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

import com.gemstone.gemfire.cache.wan.GatewaySender;

/**
 * The OrderPolicyType class is an enumeration of GemFire Gateway Order Policies.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.cache.wan.GatewaySender.OrderPolicy
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public enum OrderPolicyType {
	KEY(GatewaySender.OrderPolicy.KEY),
	PARTITION(GatewaySender.OrderPolicy.PARTITION),
	THREAD(GatewaySender.OrderPolicy.THREAD);

	private final GatewaySender.OrderPolicy orderPolicy;

	/**
	 * Constructs an instance of the OrderPolicyType enum initialized with the matching GemFire Gateway.OrderPolicy
	 * enumerated value.
	 *
	 * @param orderPolicy the matching GemFire Gateway.OrderPolicy enumerated value.
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender.OrderPolicy
	 */
	OrderPolicyType(final GatewaySender.OrderPolicy orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

	/**
	 * Null-safe operation to extract the matching GemFire Gateway.OrderPolicy enumerated value from
	 * the specified OrderPolicyType.
	 *
	 * @param orderPolicyType the OrderPolicyType enum from which to extract the GemFire-based
	 * Gateway.OrderPolicy enumerated value.
	 * @return the GemFire Gateway.OrderPolicy enumerated value for the given OrderPolicyType.
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender.OrderPolicy
	 * @see #getOrderPolicy()
	 */
	public static GatewaySender.OrderPolicy getOrderPolicy(final OrderPolicyType orderPolicyType) {
		return (orderPolicyType != null ? orderPolicyType.getOrderPolicy() : null);
	}

	/**
	 * Returns the matching OrderPolicyType given a GemFire Gateway.OrderPolicy enumerated value.
	 *
	 * @param orderPolicy the GemFire Gateway.OrderPolicy enumerated value used to match
	 * the desired OrderPolicyType.
	 * @return a OrderPolicyType matching the given GemFire Gateway.OrderPolicy enumerated value.
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender.OrderPolicy
	 * @see #getOrderPolicy()
	 */
	public static OrderPolicyType valueOf(final GatewaySender.OrderPolicy orderPolicy) {
		for (OrderPolicyType orderPolicyType : values()) {
			if (orderPolicyType.getOrderPolicy().equals(orderPolicy)) {
				return orderPolicyType;
			}
		}

		return null;
	}

	/**
	 * Returns a matching OrderPolicyType given the case-insensitive, name of the GemFire Gateway OrderPolicy.
	 *
	 * @param name a String name used to match the desired OrderPolicyType.
	 * @return a OrderPolicyType enumerated value for the given name.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #name()
	 */
	public static OrderPolicyType valueOfIgnoreCase(final String name) {
		for (OrderPolicyType orderPolicy : values()) {
			if (orderPolicy.name().equalsIgnoreCase(name)) {
				return orderPolicy;
			}
		}

		return null;
	}

	/**
	 * Gets the GemFire Gateway.OrderPolicy corresponding to this OrderPolicyType enum.
	 *
	 * @return a GemFire Gateway.OrderPolicy for this enum.
	 * @see com.gemstone.gemfire.cache.wan.GatewaySender.OrderPolicy
	 */
	public GatewaySender.OrderPolicy getOrderPolicy() {
		return orderPolicy;
	}

}
