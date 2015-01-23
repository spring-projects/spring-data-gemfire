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

import com.gemstone.gemfire.cache.EvictionAlgorithm;

/**
 * The EvictionType enum is an enumeration of all GemFire Eviction policy types.
 * 
 * @author Costin Leau
 * @author John Blum
 * @see com.gemstone.gemfire.cache.EvictionAlgorithm
 */
@SuppressWarnings("unused")
public enum EvictionType {
	ENTRY_COUNT(EvictionAlgorithm.LRU_ENTRY),
	HEAP_PERCENTAGE(EvictionAlgorithm.LRU_HEAP),
	MEMORY_SIZE(EvictionAlgorithm.LRU_MEMORY),
	NONE(EvictionAlgorithm.NONE);

	private final EvictionAlgorithm evictionAlgorithm;

	/**
	 * Constructs an instance of the EvictionType enum initialized with the matching GemFire EvictionAlgorithm.
	 *
	 * @param evictionAlgorithm the GemFire EvictionAlgorithm represented by this EvictionType enumerated value.
	 * @see com.gemstone.gemfire.cache.EvictionAlgorithm
	 */
	EvictionType(final EvictionAlgorithm evictionAlgorithm) {
		this.evictionAlgorithm = evictionAlgorithm;
	}

	/**
	 * A null-safe operation to extract the GemFire EvictionAlgorithm from the given EvictionType enumerated value.
	 *
	 * @param evictionType the EvictionType from which to extract the GemFire EvictionAlgorithm.
	 * @return the GemFire EvictionAlgorithm for the corresponding EvictionType or null if evictionType is null.
	 * @see #getEvictionAlgorithm()
	 */
	public static EvictionAlgorithm getEvictionAlgorithm(final EvictionType evictionType) {
		return (evictionType != null ? evictionType.getEvictionAlgorithm() : null);
	}

	/**
	 * Returns an EvictionType enumerated value matching the given GemFire EvictionAlgorithm.
	 *
	 * @param evictionAlgorithm the GemFire EvictionAlgorithm used to lookup and match the EvictionType.
	 * @return an EvictionType matching the specified GemFire EvictionAlgorithm or null if no match was found.
	 * @see com.gemstone.gemfire.cache.EvictionAlgorithm
	 * @see #getEvictionAlgorithm()
	 */
	public static EvictionType valueOf(final EvictionAlgorithm evictionAlgorithm) {
		for (EvictionType evictionType : values()) {
			if (evictionType.getEvictionAlgorithm().equals(evictionAlgorithm)) {
				return evictionType;
			}
		}

		return null;
	}

	/**
	 * Returns an EvictionType enumerated value given the named, case-insensitive eviction policy.
	 *
	 * @param name a String indicating the name of the eviction policy used to match EvictionType.
	 * @return an EvictionType matching the given the named, case-insensitive eviction policy.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #name()
	 */
	public static EvictionType valueOfIgnoreCase(final String name) {
		for (EvictionType evictionType : values()) {
			if (evictionType.name().equalsIgnoreCase(name)) {
				return evictionType;
			}
		}

		return null;
	}

	/**
	 * Gets the GemFire EvictionAlgorithm represented by this enumerated value.
	 *
	 * @return the GemFire EvictionAlgorithm represented by this enum.
	 * @see com.gemstone.gemfire.cache.EvictionAlgorithm
	 */
	public EvictionAlgorithm getEvictionAlgorithm() {
		return evictionAlgorithm;
	}

}
