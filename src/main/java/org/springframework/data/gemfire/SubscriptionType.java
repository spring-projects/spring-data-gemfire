/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire;

import com.gemstone.gemfire.cache.InterestPolicy;

/**
 * Simple enumeration for the various GemFire Subscription types.
 *
 * @author Lyndon Adams
 * @author John Blum
 * @since 1.3.0
 */
// TODO consider renaming this enum to InterestPolicyType
@SuppressWarnings("unused")
public enum SubscriptionType {
	ALL(InterestPolicy.ALL),
	DEFAULT(InterestPolicy.DEFAULT),
	CACHE_CONTENT(InterestPolicy.CACHE_CONTENT);

	private final InterestPolicy interestPolicy;

	/**
	 * Constructs an instance of the SubscriptionType enum initialized with the corresponding GemFire
	 * InterestPolicy instance.
	 *
	 * @param interestPolicy a GemFire InterestPolicy corresponding to this SubscriptionType.
	 */
	SubscriptionType(final InterestPolicy interestPolicy) {
		this.interestPolicy = interestPolicy;
	}

	/**
	 * Determines the value of the given GemFire InterestPolicy as a SubscriptionType enumerated value.
	 *
	 * @param interestPolicy the GemFire InterestPolicy to evaluate.
	 * @return a SubscriptionType enumerated value for the given GemFire InterestPolicy or null
	 * if the GemFire InterestPolicy does not correspond to a SubscriptionType enumerated value.
	 * @see #getInterestPolicy()
	 * @see com.gemstone.gemfire.cache.InterestPolicy
	 */
	public static SubscriptionType valueOf(final InterestPolicy interestPolicy) {
		for (SubscriptionType subscriptionType : values()) {
			if (subscriptionType.getInterestPolicy().equals(interestPolicy)) {
				return subscriptionType;
			}
		}

		return null;
	}

	/**
	 * Determines the value of the given String as a SubscriptionType enumerated value.  The String value's case
	 * is ignored.
	 *
	 * @param value a String value specifying the desired SubscriptionType enumerated value.
	 * @return a SubscriptionType enumerated value for the given String or null if the String value
	 * does not evaluate to a SubscriptionType enumerated value.
	 * @see java.lang.Enum#name()
	 */
	public static SubscriptionType valueOfIgnoreCase(final String value) {
		for (SubscriptionType subscriptionType : values()) {
			if (subscriptionType.name().equalsIgnoreCase(value)) {
				return subscriptionType;
			}
		}

		return null;
	}

	/**
	 * Returns the GemFire InterestPolicy corresponding to this SubscriptionType enumerated value.
	 *
	 * @return the GemFire InterestPolicy corresponding to this SubscriptionType enumerated value.
	 * @see com.gemstone.gemfire.cache.InterestPolicy
	 */
	public InterestPolicy getInterestPolicy() {
		return interestPolicy;
	}

}
