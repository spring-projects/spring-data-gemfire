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

package org.springframework.data.gemfire.eviction;

import org.apache.geode.cache.EvictionAlgorithm;

/**
 * The EvictionPolicyType enum is an enumeration of all Pivotal GemFire Eviction policies, where the Eviction 'policy'
 * is a combination of the Eviction algorithm mixed with the monitored resource (e.g. such as JVM HEAP memory).
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.apache.geode.cache.EvictionAlgorithm
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum EvictionPolicyType {

	ENTRY_COUNT(EvictionAlgorithm.LRU_ENTRY),
	HEAP_PERCENTAGE(EvictionAlgorithm.LRU_HEAP),
	MEMORY_SIZE(EvictionAlgorithm.LRU_MEMORY),
	NONE(EvictionAlgorithm.NONE);

	private final EvictionAlgorithm evictionAlgorithm;

	/**
	 * Constructs an instance of the EvictionPolicyType enum initialized with the matching Pivotal GemFire EvictionAlgorithm.
	 *
	 * @param evictionAlgorithm the Pivotal GemFire EvictionAlgorithm represented by this EvictionPolicyType enumerated value.
	 * @see org.apache.geode.cache.EvictionAlgorithm
	 */
	EvictionPolicyType(final EvictionAlgorithm evictionAlgorithm) {
		this.evictionAlgorithm = evictionAlgorithm;
	}

	/**
	 * A null-safe operation to extract the Pivotal GemFire EvictionAlgorithm from the given EvictionPolicyType.
	 *
	 * @param evictionPolicyType the EvictionPolicyType from which to extract the Pivotal GemFire EvictionAlgorithm.
	 * @return the Pivotal GemFire EvictionAlgorithm for the corresponding EvictionPolicyType or null if evictionType is null.
	 * @see org.apache.geode.cache.EvictionAlgorithm
	 * @see #getEvictionAlgorithm()
	 */
	public static EvictionAlgorithm getEvictionAlgorithm(final EvictionPolicyType evictionPolicyType) {
		return evictionPolicyType != null ? evictionPolicyType.getEvictionAlgorithm() : null;
	}

	/**
	 * Returns an EvictionPolicyType enumerated value matching the given Pivotal GemFire EvictionAlgorithm.
	 *
	 * @param evictionAlgorithm the Pivotal GemFire EvictionAlgorithm used to lookup and match the EvictionPolicyType.
	 * @return an EvictionPolicyType matching the specified Pivotal GemFire EvictionAlgorithm or null if no match was found.
	 * @see org.apache.geode.cache.EvictionAlgorithm
	 * @see #getEvictionAlgorithm()
	 */
	public static EvictionPolicyType valueOf(final EvictionAlgorithm evictionAlgorithm) {

		for (EvictionPolicyType evictionPolicyType : values()) {
			if (evictionPolicyType.getEvictionAlgorithm().equals(evictionAlgorithm)) {
				return evictionPolicyType;
			}
		}

		return null;
	}

	/**
	 * Returns an EvictionPolicyType enumerated value given the case-insensitive, named eviction policy.
	 *
	 * @param name a String indicating the name of the eviction policy used to match EvictionPolicyType.
	 * @return an EvictionPolicyType matching the given the case-insensitive, named eviction policy.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #name()
	 */
	public static EvictionPolicyType valueOfIgnoreCase(final String name) {

		for (EvictionPolicyType evictionPolicyType : values()) {
			if (evictionPolicyType.name().equalsIgnoreCase(name)) {
				return evictionPolicyType;
			}
		}

		return null;
	}

	/**
	 * Gets the Pivotal GemFire EvictionAlgorithm represented by this enumerated value.
	 *
	 * @return the Pivotal GemFire EvictionAlgorithm represented by this enum.
	 * @see org.apache.geode.cache.EvictionAlgorithm
	 */
	public EvictionAlgorithm getEvictionAlgorithm() {
		return this.evictionAlgorithm;
	}
}
