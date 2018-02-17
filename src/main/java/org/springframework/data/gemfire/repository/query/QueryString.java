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
 */

package org.springframework.data.gemfire.repository.query;

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIsEmpty;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.geode.cache.Region;
import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.repository.query.support.OqlKeyword;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

/**
 * {@link QueryString} is a utility class used to construct GemFire OQL query statements.
 *
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 * @see java.util.regex.Pattern
 * @see org.springframework.data.domain.Sort
 * @see org.springframework.data.gemfire.repository.query.support.OqlKeyword
 * @see org.apache.geode.cache.Region
 */
public class QueryString {

	// OQL Query Patterns
	protected static final Pattern HINT_PATTERN = Pattern.compile("<HINT '\\w+'(, '\\w+')*>");
	protected static final Pattern IMPORT_PATTERN = Pattern.compile("IMPORT .+;");
	protected static final Pattern LIMIT_PATTERN = Pattern.compile("LIMIT \\d+");
	protected static final Pattern TRACE_PATTERN = Pattern.compile("<TRACE>");

	// OQL Query Templates
	private static final String HINTS_OQL_TEMPLATE = "<HINT %1$s> %2$s";
	private static final String IMPORT_OQL_TEMPLATE = "IMPORT %1$s; %2$s";
	private static final String LIMIT_OQL_TEMPLATE = "%1$s LIMIT %2$d";
	private static final String SELECT_OQL_TEMPLATE = "SELECT %1$s FROM /%2$s";
	private static final String TRACE_OQL_TEMPLATE = "<TRACE> %1$s";

	// OQL Query Regular Expression Patterns
	private static final String IN_PATTERN = "(?<=IN (SET|LIST) )\\$\\d";
	private static final String IN_PARAMETER_PATTERN = "(?<=IN (SET|LIST) \\$)\\d";
	private static final String REGION_PATTERN = "\\/(\\/?\\w)+";

	private static final String COUNT_QUERY = "count(*)";
	private static final String STAR_QUERY = "*";

	/**
	 * Factory method used to construct an instance of {@link QueryString} initialized with the given {@link String query}.
	 *
	 * @param query {@link String} containing the OQL query.
	 * @return a new {@link QueryString} initialized with the given {@link String query}.
	 * @throws IllegalArgumentException if {@link String query} is not specified.
	 * @see #QueryString(String)
	 */
	public static QueryString of(String query) {
		return new QueryString(query);
	}

	/**
	 * Factory method used to construct an instance of {@link QueryString} initialized with
	 * the given {@link Class domain type} for which the query will be created.
	 *
	 * @param domainType {@link Class domain object type} for which the query will be created.
	 * @return a new {@link QueryString} initialized with the given {@link String query}.
	 * @throws IllegalArgumentException if {@link Class domain type} is {@literal null}.
	 * @see #QueryString(Class)
	 */
	public static QueryString from(Class<?> domainType) {
		return new QueryString(domainType);
	}

	/**
	 * Factory method used to construct an instance of {@link QueryString} initialized with
	 * the given {@link Class domain type} for which a count query will be created.
	 *
	 * @param domainType {@link Class domain object type} for which the query will be created.
	 * @return a new {@link QueryString} initialized with the given {@link String query}.
	 * @throws IllegalArgumentException if {@link Class domain type} is {@literal null}.
	 * @see #QueryString(Class)
	 */
	public static QueryString count(Class<?> domainType) {
		return new QueryString(domainType, true);
	}

	static String asQuery(Class<?> domainType, boolean isCountQuery) {
		return String.format(SELECT_OQL_TEMPLATE, isCountQuery ? COUNT_QUERY : STAR_QUERY,
			validateDomainType(domainType).getSimpleName());
	}

	static <T> Class<T> validateDomainType(Class<T> domainType) {
		Assert.notNull(domainType, "Domain type is required");
		return domainType;
	}

	static String validateQuery(String query) {
		Assert.hasText(query, String.format("Query [%s] is required", query));
		return query;
	}

	private final String query;

	/**
	 * Constructs a new instance of {@link QueryString} initialized with the given {@link String OQL query}.
	 *
	 * @param query {@link String} containing the OQL query.
	 * @throws IllegalArgumentException if {@link String query} is {@literal null} or empty.
	 * @see #validateQuery(String)
	 * @see java.lang.String
	 */
	public QueryString(String query) {
		this.query = validateQuery(query);
	}

	/**
	 * Constructs a new instance of {@link QueryString} initialized from the given {@link Class domain type},
	 * which is used to construct an OQL {@literal SELECT} query statement.
	 *
	 * @param domainType {@link Class application domain type} to query; must not be {@literal null}.
	 * @throws IllegalArgumentException if {@link Class domain type} is {@literal null}.
	 * @see #QueryString(Class, boolean)
	 */
	@SuppressWarnings("unused")
	public QueryString(Class<?> domainType) {
		this(domainType, false);
	}

	/**
	 * Constructs a new instance of {@link QueryString} initialized from the given {@link Class domain type},
	 * which is used to construct an OQL {@literal SELECT} query statement.
	 *
	 * {@code asCountQuery} is a {@link Boolean} flag indicating whether to select a count or select the contents
	 * of the objects for the given {@link Class domain type}.
	 *
	 * @param domainType {@link Class application domain type} to query; must not be {@literal null}.
	 * @param asCountQuery boolean value to indicate if this is a count query.
	 * @throws IllegalArgumentException if {@link Class domain type} is {@literal null}.
	 * @see #asQuery(Class, boolean)
	 * @see #QueryString(String)
	 */
	public QueryString(Class<?> domainType, boolean asCountQuery) {
		this(asQuery(domainType, asCountQuery));
	}

	/**
	 * Replaces the {@literal SELECT query} with a {@literal SELECT DISTINCT query} if the {@link String query}
	 * is not already distinct; i.e. does not contain the {@literal DISTINCT} keyword.
	 *
	 * @return a {@literal SELECT DISTINCT query} if {@link String query} does not contain
	 * the {@literal DISTINCT} keyword.
	 * @see java.lang.String#replaceFirst(String, String)
	 * @see #asDistinct(String)
	 */
	public QueryString asDistinct() {
		return QueryString.of(asDistinct(this.query));
	}

	/**
	 * Replaces the {@literal SELECT query} with a {@literal SELECT DISTINCT query} if the {@link String query}
	 * is not already distinct; i.e. does not contain the {@literal DISTINCT} keyword.
	 *
	 * @param query {@link String} containing the {@link String query} to evaluate.
	 * @return a {@literal SELECT DISTINCT query} if {@link String query} does not contain
	 * the {@literal DISTINCT} keyword.
	 * @see java.lang.String#replaceFirst(String, String)
	 */
	String asDistinct(String query) {

		return query.contains(OqlKeyword.DISTINCT.getKeyword()) ? query
			: query.replaceFirst(OqlKeyword.SELECT.getKeyword(),
			String.format("%1$s %2$s", OqlKeyword.SELECT.getKeyword(), OqlKeyword.DISTINCT.getKeyword()));
	}

	/**
	 * Binds the given {@link Collection} of values into the {@literal IN} parameters of the OQL Query by expanding
	 * the given values into a comma-separated {@link String}.
	 *
	 * @param values the values to bind, returns the {@link QueryString} as is if {@literal null} is given.
	 * @return a Query String having "in" parameters bound with values.
	 */
	public QueryString bindIn(Collection<?> values) {

		if (!nullSafeIsEmpty(values)) {
			return QueryString.of(this.query.replaceFirst(IN_PATTERN, String.format("(%s)",
				StringUtils.collectionToDelimitedString(values, ", ", "'", "'"))));
		}

		return this;
	}

	/**
	 * Replaces the {@link Class domain classes} referenced inside the current {@link String query}
	 * with the given {@link Region}.
	 *
	 * @param domainClass {@link Class type} of the persistent entity to query; must not be {@literal null}.
	 * @param region {@link Region} to query; must not be {@literal null}.
	 * @return a new {@link QueryString} with an OQL {@literal SELECT statement} having a {@literal FROM clause}
	 * based on the selected {@link Region}.
	 * @see org.apache.geode.cache.Region
	 * @see java.lang.Class
	 */
	@SuppressWarnings("unused")
	public QueryString fromRegion(Class<?> domainClass, Region<?, ?> region) {
		return QueryString.of(this.query.replaceAll(REGION_PATTERN, region.getFullPath()));
	}

  	/**
	 * Returns the parameter indexes used in this query.
	 *
	 * @return the parameter indexes used in this query or an empty {@link Iterable} if none are used.
	 * @see java.lang.Iterable
	 */
	public Iterable<Integer> getInParameterIndexes() {

		Pattern pattern = Pattern.compile(IN_PARAMETER_PATTERN);

		Matcher matcher = pattern.matcher(this.query);

		List<Integer> indexes = new ArrayList<>();

		while (matcher.find()) {
			indexes.add(Integer.parseInt(matcher.group()));
		}

		return indexes;
	}

	/**
	 * Appends the {@link Sort} order to this GemFire OQL Query string.
	 *
	 * @param sort {@link Sort} indicating the order of the query results.
	 * @return a new {@link QueryString} with an ORDER BY clause if {@link Sort} is not {@literal null},
	 * or this {@link QueryString} as-is if {@link Sort} is {@literal null}.
	 * @see org.springframework.data.domain.Sort
	 * @see org.springframework.data.gemfire.repository.query.QueryString
	 */
	public QueryString orderBy(Sort sort) {

		if (hasSort(sort)) {

			StringBuilder orderByClause = new StringBuilder("ORDER BY ");

			int count = 0;

			for (Sort.Order order : sort) {
				orderByClause.append(count++ > 0 ? ", " : "");
				orderByClause.append(String.format("%1$s %2$s", order.getProperty(), order.getDirection()));
			}

			return new QueryString(String.format("%1$s %2$s", asDistinct(this.query), orderByClause.toString()));
		}

		return this;
	}

	private boolean hasSort(Sort sort) {
		return sort != null && sort.iterator().hasNext();
	}

	/**
	 * Applies HINTS to the OQL Query.
	 *
	 * @param hints array of {@link String Strings} containing query hints.
	 * @return a new {@link QueryString} if hints are not null or empty, or return this {@link QueryString}.
	 */
	public QueryString withHints(String... hints) {

		if (!ObjectUtils.isEmpty(hints)) {

			StringBuilder builder = new StringBuilder();

			for (String hint : hints) {
				builder.append(builder.length() > 0 ? ", " : "");
				builder.append(String.format("'%s'", hint));
			}

			return QueryString.of(String.format(HINTS_OQL_TEMPLATE, builder.toString(), this.query));
		}

		return this;
	}

	/**
	 * Applies an IMPORT to the OQL Query.
	 *
	 * @param importExpression {@link String} containing the import clause.
	 * @return a new {@link QueryString} if an import was declared, or return this {@link QueryString}.
	 */
	public QueryString withImport(String importExpression) {
		return StringUtils.hasText(importExpression) ?
			QueryString.of(String.format(IMPORT_OQL_TEMPLATE, importExpression, this.query)) : this;
	}

	/**
	 * Applies a LIMIT to the OQL Query.
	 *
	 * @param limit {@link Integer} indicating the number of results to return from the query.
	 * @return a new {@link QueryString} if a limit was specified, or return this {@link QueryString}.
	 */
	public QueryString withLimit(Integer limit) {
		return limit != null ? QueryString.of(String.format(LIMIT_OQL_TEMPLATE, this.query, limit)) : this;
	}

	/**
	 * Applies TRACE logging to the OQL Query.
	 *
	 * @return a new {@link QueryString} with tracing enabled.
	 */
	public QueryString withTrace() {
		return QueryString.of(String.format(TRACE_OQL_TEMPLATE, this.query));
	}

	/**
	 * Returns a {@link String} representation of this {@link QueryString}.
	 *
	 * @return a {@link String} representation of this {@link QueryString}.
	 * @see java.lang.Object#toString()
	 * @see java.lang.String
	 */
	@Override
	public String toString() {
		return this.query;
	}
}
