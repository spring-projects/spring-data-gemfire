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

package org.springframework.data.gemfire.client;

import org.apache.geode.cache.InterestResultPolicy;

/**
 * The InterestResultPolicyType enum is an enumeration of all client Register Interests (result) policy values.
 *
 * @author John Blum
 * @see org.apache.geode.cache.InterestResultPolicy
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public enum InterestResultPolicyType {
	KEYS(InterestResultPolicy.KEYS),
	KEYS_VALUES(InterestResultPolicy.KEYS_VALUES),
	NONE(InterestResultPolicy.NONE);

	public static final InterestResultPolicyType DEFAULT = InterestResultPolicyType.valueOf(
		InterestResultPolicy.DEFAULT);

	private final InterestResultPolicy interestResultPolicy;

	InterestResultPolicyType(final InterestResultPolicy interestResultPolicy) {
		this.interestResultPolicy = interestResultPolicy;
	}

	public static InterestResultPolicy getInterestResultPolicy(final InterestResultPolicyType interestResultPolicyType) {
		return (interestResultPolicyType != null ? interestResultPolicyType.getInterestResultPolicy() : null);
	}

	public static InterestResultPolicyType valueOf(final InterestResultPolicy interestResultPolicy) {
		for (InterestResultPolicyType interestResultPolicyType : values()) {
			if (interestResultPolicyType.getInterestResultPolicy().equals(interestResultPolicy)) {
				return interestResultPolicyType;
			}
		}

		return null;
	}

	public static InterestResultPolicyType valueOfIgnoreCase(final String name) {
		for (InterestResultPolicyType interestResultPolicyType : values()) {
			if (interestResultPolicyType.name().equalsIgnoreCase(name)) {
				return interestResultPolicyType;
			}
		}

		return null;
	}

	public InterestResultPolicy getInterestResultPolicy() {
		return interestResultPolicy;
	}

}
