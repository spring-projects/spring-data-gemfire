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

package org.springframework.data.gemfire;

import org.apache.geode.cache.Scope;
import org.springframework.util.StringUtils;

/**
 * The ScopeType enum is an enumeration of GemFire Scopes.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Scope
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public enum ScopeType {
	DISTRIBUTED_ACK(Scope.DISTRIBUTED_ACK),
	DISTRIBUTED_NO_ACK(Scope.DISTRIBUTED_NO_ACK),
	GLOBAL(Scope.GLOBAL),
	LOCAL(Scope.LOCAL);

	private final Scope gemfireScope;

	/**
	 * Constructs an instance of the ScopeType initialized with a matching GemFire Scope.
	 *
	 * @param gemfireScope the GemFire Scope paired with this enumerated value.
	 * @see org.apache.geode.cache.Scope
	 */
	ScopeType(final Scope gemfireScope) {
		this.gemfireScope = gemfireScope;
	}

	/**
	 * Null-safe operation to extract the GemFire Scope from the given ScopeType enum value, or null if the provided
	 * scopeType is null.
	 *
	 * @param scopeType the ScopeType enumerated value from which to extract the GemFire Scope.
	 * @return the paired GemFire Scope from the given ScopeType or null if scopeType is null.
	 * @see org.apache.geode.cache.Scope
	 * @see #getScope()
	 */
	public static Scope getScope(final ScopeType scopeType) {
		return (scopeType != null ? scopeType.getScope() : null);
	}

	/**
	 * Returns a ScopeType enumerated value for the given a GemFire Scope.
	 *
	 * @param scope the GemFire Scope used to lookup and match the appropriate ScopeType.
	 * @return a ScopeType for the given GemFire Scope or null if no match was found.
	 * @see org.apache.geode.cache.Scope
	 * @see #getScope()
	 * @see #values()
	 */
	public static ScopeType valueOf(final Scope scope) {
		for (ScopeType scopeType : values()) {
			if (scopeType.getScope().equals(scope)) {
				return scopeType;
			}
		}

		return null;
	}

	/**
	 * Returns a ScopeType enumerated value given the case-insensitive name of the GemFire Scope.
	 *
	 * @param name a String name describing the ScopeType enum value.
	 * @return a ScopeType for the given case-insensitive, named GemFire Scope.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #values()
	 * @see #name()
	 * @see #transform(String)
	 */
	public static ScopeType valueOfIgnoreCase(String name) {
		name = transform(name);

		for (ScopeType scopeType : values()) {
			if (scopeType.name().equalsIgnoreCase(name)) {
				return scopeType;
			}
		}

		return null;
	}

	/**
	 * Null-safe operation that transforms a String name having hyphens and whitespace into a String with underscores
	 * and no whitespace.
	 *
	 * @param name the String to transform.
	 * @return a String value with underscores for hyphens and all leading/trailing whitespace trimmed, or null
	 * if the given String name is null.
	 */
	private static String transform(final String name) {
		return (StringUtils.hasText(name) ? name.trim().replaceAll("-", "_") : name);
	}

	/**
	 * Gets the matching GemFire Scope for this enumerated value.
	 *
	 * @return a GemFire Scope for this enumerated value.
	 * @see org.apache.geode.cache.Scope
	 */
	public Scope getScope() {
		return gemfireScope;
	}

}
