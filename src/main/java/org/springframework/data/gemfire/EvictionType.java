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
 * Simple enumeration for the various GemFire Eviction types.
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
	 * Constructs an instance (enumeration) of the EvictionType enumerated type initialized with the corresponding
	 * GemFire EvictionAlgorithm.
	 *
	 * @param evictionAlgorithm the GemFire EvictionAlgorithm represented by this EvictionType.
	 * @see com.gemstone.gemfire.cache.EvictionAlgorithm
	 */
	EvictionType(final EvictionAlgorithm evictionAlgorithm) {
		this.evictionAlgorithm = evictionAlgorithm;
	}

	/**
	 * Returns the corresponding SDG EvictionType enumerated value for the GemFire EvictionAlgorithm.
	 *
	 * @param evictionAlgorithm the GemFire EvictionAlgorithm used to lookup the corresponding EvictionType.
	 * @return a EvictionType representing the specified GemFire EvictionAlgorithm.  Returns null if no EvictionType
	 * represents the given GemFire EvictionAlgorithm.
	 * @see #getEvictionAlgorithm()
	 * @see com.gemstone.gemfire.cache.EvictionAlgorithm
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
	 * Returns an EvictionType enumerated value for the given, named EvictionType, ignoring case.
	 *
	 * @param value a String value indicating the name of the desired EvictionType enumerated value.
	 * @return an EvictionType corresponding to the given, named enumerated value.
	 * @see java.lang.Enum#name()
	 */
	public static EvictionType valueOfIgnoreCase(final String value) {
		for (EvictionType evictionType : values()) {
			if (evictionType.name().equalsIgnoreCase(value)) {
				return evictionType;
			}
		}

		return null;
	}

	/**
	 * Gets the GemFire EvictionAlgorithm represented by this enumerated value.
	 *
	 * @return the corresponding GemFire EvictionAlgorithm.
	 * @see com.gemstone.gemfire.cache.EvictionAlgorithm
	 */
	public EvictionAlgorithm getEvictionAlgorithm() {
		return evictionAlgorithm;
	}

}
