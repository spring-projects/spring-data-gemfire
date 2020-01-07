/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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
 * The InterestPolicyType enum is an enumeration of all the Pivotal GemFire Subscription, InterestPolicy values.
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
	 * Constructs an instance of the SubscriptionType enum initialized with the matching Pivotal GemFire InterestPolicy.
	 *
	 * @param interestPolicy a Pivotal GemFire InterestPolicy corresponding to this SubscriptionType.
	 * @see org.apache.geode.cache.InterestPolicy
	 */
	InterestPolicyType(final InterestPolicy interestPolicy) {
		this.interestPolicy = interestPolicy;
	}

	/**
	 * Null-safe operation to extract the Pivotal GemFire {@link InterestPolicy}
	 * from the {@link InterestPolicyType} enumerated value.
	 *
	 * @param interestPolicyType the {@link InterestPolicyType} enum from which to extract
	 * Pivotal GemFire's {@link InterestPolicy}.
	 * @return a Pivotal GemFire {@link InterestPolicy} for the given {@link InterestPolicyType} enumerated value
	 * or {@literal null} if {@link InterestPolicyType} is {@literal null}.
	 * @see org.apache.geode.cache.InterestPolicy
	 */
	public static InterestPolicy getInterestPolicy(final InterestPolicyType interestPolicyType) {
		return (interestPolicyType != null ? interestPolicyType.getInterestPolicy() : null);
	}

	/**
	 * Returns a SubscriptionType enumerated value for the given Pivotal GemFire InterestPolicy.
	 *
	 * @param interestPolicy the Pivotal GemFire InterestPolicy used to lookup and match a SubscriptionType.
	 * @return a SubscriptionType enumerated value matching the given Pivotal GemFire InterestPolicy
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
	 * Returns the Pivotal GemFire InterestPolicy corresponding to this SubscriptionType enumerated value.
	 *
	 * @return the Pivotal GemFire InterestPolicy corresponding to this SubscriptionType.
	 * @see org.apache.geode.cache.InterestPolicy
	 */
	public InterestPolicy getInterestPolicy() {
		return interestPolicy;
	}
}
