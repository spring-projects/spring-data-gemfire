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

package org.springframework.data.gemfire.expiration;

import org.apache.geode.cache.ExpirationAction;

/**
 * The ExpirationActionType enum is a enumeration of Pivotal GemFire ExpirationActions on expired Cache Region entries.
 *
 * @author John Blum
 * @see org.apache.geode.cache.ExpirationAction
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public enum ExpirationActionType {

	DESTROY(ExpirationAction.DESTROY),
	INVALIDATE(ExpirationAction.INVALIDATE),
	LOCAL_DESTROY(ExpirationAction.LOCAL_DESTROY),
	LOCAL_INVALIDATE(ExpirationAction.LOCAL_INVALIDATE);

	public static final ExpirationActionType DEFAULT = ExpirationActionType.INVALIDATE;

	private final ExpirationAction expirationAction;

	/**
	 * Constructs an instance of the ExpirationActionType enum initialized with the matching Pivotal GemFire ExpirationAction.
	 *
	 * @param expirationAction the matching Pivotal GemFire ExpirationAction for this enumerated value.
	 * @see org.apache.geode.cache.ExpirationAction
	 */
	ExpirationActionType(final ExpirationAction expirationAction) {
		this.expirationAction = expirationAction;
	}

	/**
	 * A null-safe operation to extract the corresponding Pivotal GemFire ExpirationAction for the ExpirationActionType.
	 *
	 * @param expirationActionType the ExpirationActionType enumerated value from which to extract
	 * the corresponding Pivotal GemFire ExpirationAction.
	 * @return a Pivotal GemFire ExpirationAction given the ExpirationActionType enumerated value.
	 * @see org.apache.geode.cache.ExpirationAction
	 */
	public static ExpirationAction getExpirationAction(final ExpirationActionType expirationActionType) {
		return expirationActionType != null ? expirationActionType.getExpirationAction() : null;
	}

	/**
	 * Returns the ExpirationActionType enumerated value matching the given Pivotal GemFire ExpirationAction.
	 *
	 * @param expirationAction the Pivotal GemFire ExpirationAction used to match the ExpirationActionType.
	 * @return a matching ExpirationActionType enumerated value given a Pivotal GemFire ExpirationAction
	 * or null if no match was found.
	 * @see org.apache.geode.cache.ExpirationAction
	 * @see #getExpirationAction()
	 */
	public static ExpirationActionType valueOf(final ExpirationAction expirationAction) {

		for (ExpirationActionType expirationActionType : values()) {
			if (expirationActionType.getExpirationAction().equals(expirationAction)) {
				return expirationActionType;
			}
		}

		return null;
	}

	/**
	 * Returns an ExpirationActionType enumerated value given a named, case-insensitive expiration action.
	 *
	 * @param name a String name for the expiration action matching the ExpirationActionType.
	 * @return a matching ExpirationActionType for the named, case-insensitive expiration action
	 * or null if no match could be found.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #name()
	 */
	public static ExpirationActionType valueOfIgnoreCase(final String name) {

		for (ExpirationActionType expirationActionType : values()) {
			if (expirationActionType.name().equalsIgnoreCase(name)) {
				return expirationActionType;
			}
		}

		return null;
	}

	/**
	 * Gets the matching Pivotal GemFire ExpirationAction for this enumerated value.
	 *
	 * @return the Pivotal GemFire ExpirationAction instance corresponding to this enumerated value.
	 * @see org.apache.geode.cache.ExpirationAction
	 */
	public ExpirationAction getExpirationAction() {
		return this.expirationAction;
	}
}
