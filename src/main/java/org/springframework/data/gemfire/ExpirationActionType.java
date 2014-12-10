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

package org.springframework.data.gemfire;

import com.gemstone.gemfire.cache.ExpirationAction;

/**
 * The ExpirationActionType enum is a enumeration of GemFire Expiration Actions on expired Cache Region entries.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.cache.ExpirationAction
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public enum ExpirationActionType {
	DESTROY(ExpirationAction.DESTROY),
	INVALIDATE(ExpirationAction.INVALIDATE),
	LOCAL_DESTROY(ExpirationAction.LOCAL_DESTROY),
	LOCAL_INVALIDATE(ExpirationAction.LOCAL_INVALIDATE);

	private final ExpirationAction expirationAction;

	/**
	 * Constructs an instance of the ExpirationActionType enum initialized with the corresponding GemFire
	 * ExpirationAction value.
	 *
	 * @param expirationAction the GemFire ExpirationAction value.
	 * @see com.gemstone.gemfire.cache.ExpirationAction
	 */
	ExpirationActionType(final ExpirationAction expirationAction) {
		this.expirationAction = expirationAction;
	}

	/**
	 * Returns the corresponding SDG ExpirationActionType enumerated value given a GemFire ExpirationAction.
	 *
	 * @param expirationAction the GemFire ExpirationAction instance used to determine the ExpirationActionType.
	 * @return a ExpirationActionType enumerated value given a GemFire ExpirationAction.
	 * @see com.gemstone.gemfire.cache.ExpirationAction
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
	 * Returns an ExpirationActionType for the given String value describing the enumerated value.
	 *
	 * @param value a String value describing the desired ExpirationActionType that is returned.
	 * @return an ExpirationActionType for the given String.
	 * @see java.lang.Enum#name()
	 * @see java.lang.String#equalsIgnoreCase(String)
	 */
	public static ExpirationActionType valueOfIgnoreCase(final String value) {
		for (ExpirationActionType expirationActionType : values()) {
			if (expirationActionType.name().equalsIgnoreCase(value)) {
				return expirationActionType;
			}
		}

		return null;
	}

	/**
	 * Gets the corresponding GemFire ExpirationAction for this enumerated value.
	 *
	 * @return a GemFire ExpirationAction instance corresponding to this enumerated value.
	 * @see com.gemstone.gemfire.cache.ExpirationAction
	 */
	public ExpirationAction getExpirationAction() {
		return expirationAction;
	}

}
