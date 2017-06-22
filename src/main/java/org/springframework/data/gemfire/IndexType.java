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

/**
 * The IndexType class is an enumerated type of GemFire Index Types.
 *
 * @author John Blum
 * @see org.apache.geode.cache.query.IndexType
 * @since 1.5.2
 */
@SuppressWarnings({ "deprecation", "unused" })
public enum IndexType {

	FUNCTIONAL(org.apache.geode.cache.query.IndexType.FUNCTIONAL),
	HASH(org.apache.geode.cache.query.IndexType.HASH),
	PRIMARY_KEY(org.apache.geode.cache.query.IndexType.PRIMARY_KEY),
	KEY(org.apache.geode.cache.query.IndexType.PRIMARY_KEY);

	private final org.apache.geode.cache.query.IndexType gemfireIndexType;

	/**
	 * Constructs an instance of the IndexType enum initialized with the given GemFire IndexType.
	 *
	 * @param gemfireIndexType the corresponding GemFire IndexType
	 * @see org.apache.geode.cache.query.IndexType
	 */
	IndexType(final org.apache.geode.cache.query.IndexType gemfireIndexType) {
		this.gemfireIndexType = gemfireIndexType;
	}

	/**
	 * Null-safe operation to determine if the IndexType is a "FUNCTIONAL" Index.
	 *
	 * @param indexType the IndexType to evaluate.
	 * @return a boolean value indicating whether the IndexType is a "FUNCTIONAL" Index.
	 * @see #isFunctional()
	 */
	public static boolean isFunctional(IndexType indexType) {
		return (indexType != null && indexType.isFunctional());
	}

	/**
	 * Null-safe operation to determine if the IndexType is a "HASH" Index.
	 *
	 * @param indexType the IndexType to evaluate.
	 * @return a boolean value indicating whether the IndexType is a "HASH" Index.
	 * @see #isHash()
	 */
	public static boolean isHash(IndexType indexType) {
		return (indexType != null && indexType.isHash());
	}

	/**
	 * Null-safe operation to determine if the IndexType is a "KEY" Index.
	 *
	 * @param indexType the IndexType to evaluate.
	 * @return a boolean value indicating whether the IndexType is a "KEY" Index.
	 * @see #isFunctional()
	 */
	public static boolean isKey(IndexType indexType) {
		return (indexType != null && indexType.isKey());
	}

	/**
	 * Returns an IndexType given the corresponding GemFire IndexType or null if no SDG IndexType
	 * corresponds to the GemFire IndexType.
	 *
	 * @param gemfireIndexType the GemFire IndexType.
	 * @return a IndexType matching the GemFire IndexType or null if the GemFire IndexType does not match
	 * any IndexType in this enumeration.
	 * @see org.apache.geode.cache.query.IndexType
	 */
	public static IndexType valueOf(org.apache.geode.cache.query.IndexType gemfireIndexType) {

		for (IndexType indexType : values()) {
			if (indexType.getGemfireIndexType().equals(gemfireIndexType)) {
				return indexType;
			}
		}

		return null;
	}

	/**
	 * Returns an IndexType matching the given String.
	 *
	 * @param value the String value describing the matching IndexType.
	 * @return an IndexType matching the given String.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 */
	public static IndexType valueOfIgnoreCase(String value) {

		for (IndexType indexType : values()) {
			if (indexType.name().equalsIgnoreCase(value)) {
				return indexType;
			}
		}

		return null;
	}

	/**
	 * Gets the matching GemFire IndexType for this IndexType enumerated value.
	 *
	 * @return the matching GemFire IndexType.
	 * @see org.apache.geode.cache.query.IndexType
	 */
	public org.apache.geode.cache.query.IndexType getGemfireIndexType() {
		return gemfireIndexType;
	}

	/**
	 * Determines whether this IndexType is "FUNCTIONAL".
	 *
	 * @return a boolean value indicating whether this IndexType is "FUNCTIONAL".
	 */
	public boolean isFunctional() {
		return this.equals(FUNCTIONAL);
	}

	/**
	 * Determines whether this IndexType is a "HASH" Index.
	 *
	 * @return a boolean value indicating whether this IndexType is a "HASH" Index.
	 */
	public boolean isHash() {
		return this.equals(HASH);
	}

	/**
	 * Determines whether this IndexType is a "KEY" Index.
	 *
	 * @return a boolean value indicating whether this IndexType is a "KEY" Index.
	 */
	public boolean isKey() {
		return (this.equals(KEY) || this.equals(PRIMARY_KEY));
	}
}
