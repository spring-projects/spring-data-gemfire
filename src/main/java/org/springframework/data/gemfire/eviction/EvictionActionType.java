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

package org.springframework.data.gemfire.eviction;

import org.apache.geode.cache.EvictionAction;

/**
 * The EvictionActionType enum is an enumeration of all the Pivotal GemFire EvictionAction values.
 *
 * @author John Blum
 * @see org.apache.geode.cache.EvictionAction
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public enum EvictionActionType {

	LOCAL_DESTROY(EvictionAction.LOCAL_DESTROY),
	NONE(EvictionAction.NONE),
	OVERFLOW_TO_DISK(EvictionAction.OVERFLOW_TO_DISK);

	public static final EvictionActionType DEFAULT = EvictionActionType.valueOf(EvictionAction.DEFAULT_EVICTION_ACTION);

	private final EvictionAction evictionAction;

	/**
	 * Constructs an instance of the EvictionActionType enum initialized with the matching Pivotal GemFire EvictionAction.
	 *
	 * @param evictionAction the matching Pivotal GemFire EvictionAction value for this enumerated value.
	 * @see org.apache.geode.cache.EvictionAction
	 */
	EvictionActionType(final EvictionAction evictionAction) {
		this.evictionAction = evictionAction;
	}

	/**
	 * A null-safe operation to extract the Pivotal GemFire EvictionAction from the EvictionActionType enumerated value.
	 *
	 * @param evictionActionType the EvictionActionType enumerated value from which to extract
	 * the matching Pivotal GemFire EvictionAction value.
	 * @return a Pivotal GemFire EvictionAction given a EvictionActionType enumerated value.
	 * @see #getEvictionAction()
	 */
	public static EvictionAction getEvictionAction(final EvictionActionType evictionActionType) {
		return evictionActionType != null ? evictionActionType.getEvictionAction() : null;
	}

	/**
	 * Returns an EvictionActionType enumerated value matching the given Pivotal GemFire EvictionAction.
	 *
	 * @param evictionAction the Pivotal GemFire EvictionAction used to lookup and match the appropriate EvictionActionType.
	 * @return an EvictionActionType enumerated value matching the given Pivotal GemFire EvictionAction
	 * or null if no match was found.
	 * @see org.apache.geode.cache.EvictionAction
	 * @see #getEvictionAction()
	 */
	public static EvictionActionType valueOf(final EvictionAction evictionAction) {

		for (EvictionActionType evictionActionType : values()) {
			if (evictionActionType.getEvictionAction().equals(evictionAction)) {
				return evictionActionType;
			}
		}

		return null;
	}

	/**
	 * Returns an EvictionActionType enumerated value given the named, case-insensitive eviction action.
	 *
	 * @param name a String value indicating the name the eviction action used to match EvictionActionType.
	 * @return an EvictionActionType enumerated value matching the given named, case-insensitive eviction action
	 * or null if not match was found.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #name()
	 */
	public static EvictionActionType valueOfIgnoreCase(final String name) {

		for (EvictionActionType evictionActionType : values()) {
			if (evictionActionType.name().equalsIgnoreCase(name)) {
				return evictionActionType;
			}
		}

		return null;
	}

	/**
	 * Gets the matching Pivotal GemFire EvictionAction represented by this enumerated value.
	 *
	 * @return the Pivotal GemFire EvictionAction represented by this enum.
	 * @see org.apache.geode.cache.EvictionAction
	 */
	public EvictionAction getEvictionAction() {
		return this.evictionAction;
	}
}
