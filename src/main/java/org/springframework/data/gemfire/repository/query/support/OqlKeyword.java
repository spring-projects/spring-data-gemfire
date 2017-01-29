/*
 * Copyright 2012-2018 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.repository.query.support;

import org.springframework.util.StringUtils;

/**
 * The OqlKeyword enum represents the range of keywords (Reserved Words)
 * in GemFire's Object Query Language (OQL).
 *
 * @author John Blum
 * @see <a href="http://gemfire.docs.pivotal.io/docs-gemfire/latest/developing/query_additional/supported_keywords.html">Supported Keywords</a>
 * @since 1.0.0
 */
public enum OqlKeyword {
	AND,
	AS,
	COUNT,
	DISTINCT,
	ELEMENT,
	FROM,
	HINT,
	IMPORT,
	IN,
	IS_DEFINED,
	IS_UNDEFINED,
	LIMIT,
	LIKE,
	NOT,
	NVL,
	OR,
	ORDER_BY("ORDER BY"),
	SELECT,
	SET,
	TRACE,
	TO_DATE,
	TYPE,
	WHERE;

	private final String keyword;

	/**
	 * Constructs an instance of the GemFire {@link OqlKeyword} enumerate value with an unspecified keyword.
	 * When the keyword is unspecified, it defaults to the {@link #name()} of the {@link OqlKeyword}.
	 *
	 * @see #OqlKeyword(String)
	 */
	OqlKeyword() {
		this(null);
	}

	/**
	 * Constructs an {@link OqlKeyword} enumerated value with the given GemFire OQL Keyword.
	 *
	 * @param keyword {@link String} specifying the GemFire OQL Keyword;
	 * can be {@literal null}.
	 */
	OqlKeyword(String keyword) {
		this.keyword = keyword;
	}

	/**
	 * Looks up an {@link OqlKeyword} for the given {@code keyword} {@link String}.
	 *
	 * @param keyword name of the GemFire OQL Keyword to lookup.
	 * @return an {@link OqlKeyword} enumerated value for the given {@code keyword} {@link String}.
	 * @throws IllegalArgumentException if {@code keyword} is not a valid GemFire OQL Keyword.
	 */
	public static OqlKeyword valueOfIgnoreCase(String keyword) {
		for (OqlKeyword oqlKeyword : values()) {
			if (oqlKeyword.getKeyword().equalsIgnoreCase(StringUtils.trimWhitespace(keyword))) {
				return oqlKeyword;
			}
		}

		throw new IllegalArgumentException(String.format("[%s] is not a valid GemFire OQL Keyword", keyword));
	}

	/**
	 * Returns name of this GemFire OQL Keyword enumerated value.  The keyword may have been
	 * explicitly defined when the {@link OqlKeyword} enumerated value was constructed, in which case
	 * this value is returned, otherwise this method returns {@link OqlKeyword#name()}.
	 *
	 * @return a {@link String} name for this GemFire OQL Keyword enumerated value.
	 * @see #name()
	 */
	public String getKeyword() {
		return (StringUtils.hasText(this.keyword) ? this.keyword : name());
	}

	/* (non-Javadoc) */
	@Override
	public String toString() {
		return getKeyword();
	}
}
