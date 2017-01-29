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
 * {@link QueryString} is a utility class used to construct GemFire OQL query statement syntax.
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

	private final String query;

	/* (non-Javadoc) */
	static String asQuery(Class<?> domainType, boolean isCountQuery) {
		return String.format(SELECT_OQL_TEMPLATE, (isCountQuery ? "count(*)" : "*"),
			validateDomainType(domainType).getSimpleName());
	}

	/* (non-Javadoc) */
	static <T> Class<T> validateDomainType(Class<T> domainType) {
		Assert.notNull(domainType, "domainType must not be null");
		return domainType;
	}

	/* (non-Javadoc) */
	static String validateQuery(String query) {
		Assert.hasText(query, "An OQL Query must be specified");
		return query;
	}

	/**
	 * Constructs an instance of {@link QueryString} initialized with the given GemFire OQL Query {@link String}.
	 *
	 * @param query {@link String} specifying the GemFire OQL Query.
	 * @throws IllegalArgumentException if the query string is unspecified (null or empty).
	 */
	public QueryString(String query) {
		this.query = validateQuery(query);
	}

	/**
	 * Constructs a GemFire OQL {@literal SELECT} Query for the given domain class.
	 *
	 * @param domainType application domain object type to query; must not be {@literal null}.
	 * @see #QueryString(Class, boolean)
	 */
	@SuppressWarnings("unused")
	public QueryString(Class<?> domainType) {
		this(domainType, false);
	}

	/**
	 * Constructs a GemFire OQL {@literal SELECT} Query for the given domain class.  {@code isCountQuery} indicates
	 * whether to select a count or select the contents of the objects of the given domain object type.
	 *
	 * @param domainType application domain object type to query; must not be {@literal null}.
	 * @param isCountQuery boolean value to indicate if this is a count query.
	 * @throws IllegalArgumentException if {@code domainType} is null.
	 * @see #QueryString(String)
	 */
	public QueryString(Class<?> domainType, boolean isCountQuery) {
		this(asQuery(domainType, isCountQuery));
	}

	/**
	 * Binds the given {@link Collection} of values into the {@literal IN} parameters of the OQL Query by expanding
	 * the given values into a comma-separated {@link String}.
	 *
	 * @param values the values to bind, returns the {@link QueryString} as is if {@literal null} is given.
	 * @return a Query String having "in" parameters bound with values.
	 */
	public QueryString bindIn(Collection<?> values) {
		if (values != null) {
			return new QueryString(this.query.replaceFirst(IN_PATTERN, String.format("(%s)",
				StringUtils.collectionToDelimitedString(values, ", ", "'", "'"))));
		}

		return this;
	}

	/**
	 * Replaces the domain classes referenced inside the current query with the given {@link Region}.
	 *
	 * @param domainClass the class type of the GemFire persistent entity to query; must not be {@literal null}.
	 * @param region the GemFire Region in which to query; must not be {@literal null}.
	 * @return a Query String with the FROM clause in the OQL statement evaluated and replaced with
	 * the fully-qualified Region to query.
	 * @see org.apache.geode.cache.Region
	 */
	@SuppressWarnings("unused")
	public QueryString forRegion(Class<?> domainClass, Region<?, ?> region) {
		return new QueryString(this.query.replaceAll(REGION_PATTERN, region.getFullPath()));
	}

  /**
	 * Returns the parameter indexes used in this query.
	 *
	 * @return the parameter indexes used in this query or an empty {@link Iterable} if none are used.
	 * @see java.lang.Iterable
	 */
	public Iterable<Integer> getInParameterIndexes() {
		Pattern pattern = Pattern.compile(IN_PARAMETER_PATTERN);
		Matcher matcher = pattern.matcher(query);
		List<Integer> result = new ArrayList<>();

		while (matcher.find()) {
			result.add(Integer.parseInt(matcher.group()));
		}

		return result;
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
			StringBuilder orderClause = new StringBuilder("ORDER BY ");
			int count = 0;

			for (Sort.Order order : sort) {
				orderClause.append(count++ > 0 ? ", " : "");
				orderClause.append(String.format("%1$s %2$s", order.getProperty(), order.getDirection()));
			}

			return new QueryString(String.format("%1$s %2$s", makeDistinct(this.query), orderClause.toString()));
		}

		return this;
	}

	/* (non-Javadoc) */
	private boolean hasSort(Sort sort) {
		return (sort != null && sort.iterator().hasNext());
	}

	/**
	 * Replaces the SELECT query with a SELECT DISTINCT query if the query does not contain the DISTINCT OQL keyword.
	 *
	 * @param query {@link String} containing the query to evaluate.
	 * @return a SELECT DISTINCT query if {@code query} does not contain the DISTINCT OQL keyword.
	 */
	String makeDistinct(String query) {
		return (query.contains(OqlKeyword.DISTINCT.getKeyword()) ? query
			: query.replaceFirst(OqlKeyword.SELECT.getKeyword(),
				String.format("%1$s %2$s", OqlKeyword.SELECT.getKeyword(), OqlKeyword.DISTINCT.getKeyword())));
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
				builder.append(String.format("'%1$s'", hint));
			}

			return new QueryString(String.format(HINTS_OQL_TEMPLATE, builder.toString(), query));
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
		return (StringUtils.hasText(importExpression) ?
			new QueryString(String.format(IMPORT_OQL_TEMPLATE, importExpression, query)) : this);
	}

	/**
	 * Applies a LIMIT to the OQL Query.
	 *
	 * @param limit {@link Integer} indicating the number of results to return from the query.
	 * @return a new {@link QueryString} if a limit was specified, or return this {@link QueryString}.
	 */
	public QueryString withLimit(Integer limit) {
		return (limit != null ? new QueryString(String.format(LIMIT_OQL_TEMPLATE, query, limit)) : this);
	}

	/**
	 * Applies TRACE logging to the OQL Query.
	 *
	 * @return a new {@link QueryString} with tracing enabled.
	 */
	public QueryString withTrace() {
		return new QueryString(String.format(TRACE_OQL_TEMPLATE, query));
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return query;
	}
}
