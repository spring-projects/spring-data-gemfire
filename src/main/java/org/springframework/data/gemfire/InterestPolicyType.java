/*
 * Copyright 2010-2018 the original author or authors.
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

import org.apache.geode.cache.InterestPolicy;

/**
 * The InterestPolicyType enum is an enumeration of all the GemFire Subscription, InterestPolicy values.
 *
 * @author Lyndon Adams
 * @author John Blum
 * @see org.apache.geode.cache.InterestPolicy
 * @since 1.3.0
 */
@SuppressWarnings("unused")
public enum InterestPolicyType {
	ALL(InterestPolicy.ALL),
	CACHE_CONTENT(InterestPolicy.CACHE_CONTENT);

	public static final InterestPolicyType DEFAULT = InterestPolicyType.valueOf(InterestPolicy.DEFAULT);

	private final InterestPolicy interestPolicy;

	/**
	 * Constructs an instance of the SubscriptionType enum initialized with the matching GemFire InterestPolicy.
	 *
	 * @param interestPolicy a GemFire InterestPolicy corresponding to this SubscriptionType.
	 * @see org.apache.geode.cache.InterestPolicy
	 */
	InterestPolicyType(final InterestPolicy interestPolicy) {
		this.interestPolicy = interestPolicy;
	}

	/**
	 * Null-safe operation to extract the GemFire InterestPolicy from the InterPolicyType enumerated value.
	 *
	 * @param interestPolicyType the InterestPolicyType enum from which to extract GemFire's InterestPolicy
	 * @return a GemFire InterestPolicy for the given InterestPolicyType enumerated value
	 * or null if InterestPolicyType is null.
	 * @see org.apache.geode.cache.InterestPolicy
	 */
	public static InterestPolicy getInterestPolicy(final InterestPolicyType interestPolicyType) {
		return (interestPolicyType != null ? interestPolicyType.getInterestPolicy() : null);
	}

	/**
	 * Returns a SubscriptionType enumerated value for the given GemFire InterestPolicy.
	 *
	 * @param interestPolicy the GemFire InterestPolicy used to lookup and match a SubscriptionType.
	 * @return a SubscriptionType enumerated value matching the given GemFire InterestPolicy
	 * or null if no matching value was found.
	 * @see org.apache.geode.cache.InterestPolicy
	 * @see #getInterestPolicy()
	 */
	public static InterestPolicyType valueOf(final InterestPolicy interestPolicy) {
		for (InterestPolicyType interestPolicyType : values()) {
			if (interestPolicyType.getInterestPolicy().equals(interestPolicy)) {
				return interestPolicyType;
			}
		}

		return null;
	}

	/**
	 * Returns a SubscriptionType enumerated value for the case-insensitive, named Subscription (InterestsPolicy).
	 *
	 * @param value a String name used to look and match the SubscriptionType.
	 * @return a SubscriptionType enumerated value for the given case-insensitive named Subscription
	 * or null if no match was found.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #name()
	 */
	public static InterestPolicyType valueOfIgnoreCase(final String value) {
		for (InterestPolicyType interestPolicyType : values()) {
			if (interestPolicyType.name().equalsIgnoreCase(value)) {
				return interestPolicyType;
			}
		}

		return null;
	}

	/**
	 * Returns the GemFire InterestPolicy corresponding to this SubscriptionType enumerated value.
	 *
	 * @return the GemFire InterestPolicy corresponding to this SubscriptionType.
	 * @see org.apache.geode.cache.InterestPolicy
	 */
	public InterestPolicy getInterestPolicy() {
		return interestPolicy;
	}

}
