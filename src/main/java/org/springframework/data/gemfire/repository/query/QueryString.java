/*
 * Copyright 2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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

import org.springframework.data.domain.Sort;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.Region;

/**
 * Value object to work with OQL query strings.
 * 
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 */
public class QueryString {

	private static final String IN_PATTERN = "(?<=IN (SET|LIST) )\\$\\d";
	private static final String IN_PARAMETER_PATTERN = "(?<=IN (SET|LIST) \\$)\\d";
	private static final String REGION_PATTERN = "\\/(\\/?\\w)+";

	private final String query;

	/**
	 * Creates a {@link QueryString} from the given {@link String} query.
	 * 
	 * @param source a String containing the OQL Query.
	 */
	public QueryString(String source) {
		Assert.hasText(source, "The OQL statement (Query) to execute must be specified!");
		this.query = source;
	}

	/**
	 * Creates a {@literal SELECT} query for the given domain class.
	 * 
	 * @param domainClass must not be {@literal null}.
	 */
	@SuppressWarnings("unused")
	public QueryString(Class<?> domainClass) {
		this(domainClass, false);
	}

	/**
	 * Creates a {@literal SELECT} query for the given domain class.
	 * 
	 * @param domainClass must not be {@literal null}.
	 * @param isCountQuery indicates if this is a count query
	 */
	public QueryString(Class<?> domainClass, boolean isCountQuery) {
		this(String.format(isCountQuery ? "SELECT count(*) FROM /%s" : "SELECT * FROM /%s",
			domainClass.getSimpleName()));
	}

	/**
	 * Binds the given values to the {@literal IN} parameter keyword by expanding the given values into a comma-separated
	 * {@link String}.
	 *
	 * @param values the values to bind, returns the {@link QueryString} as is if {@literal null} is given.
	 * @return a Query String having "in" parameters bound with values.
	 */
	public QueryString bindIn(Collection<?> values) {
		if (values != null) {
			String valueString = StringUtils.collectionToDelimitedString(values, ", ", "'", "'");
			return new QueryString(query.replaceFirst(IN_PATTERN, String.format("(%s)", valueString)));
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
	 * @see com.gemstone.gemfire.cache.Region
	 */
	@SuppressWarnings("unused")
	public QueryString forRegion(Class<?> domainClass, Region<?, ?> region) {
		return new QueryString(query.replaceAll(REGION_PATTERN, region.getFullPath()));
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
		List<Integer> result = new ArrayList<Integer>();

		while (matcher.find()) {
			result.add(Integer.parseInt(matcher.group()));
		}

		return result;
	}

	/**
	 * Appends the Sort order to this GemFire OQL Query string.
	 *
	 * @param sort the Sort object indicating the order by criteria.
	 * @return this QueryString with an ORDER BY clause if the Sort object is not null, or this QueryString as-is
	 * if the Sort object is null.
	 * @see org.springframework.data.domain.Sort
	 * @see org.springframework.data.gemfire.repository.query.QueryString
	 */
	public QueryString orderBy(Sort sort) {
		if (sort != null) {
			StringBuilder orderClause = new StringBuilder("ORDER BY ");
			int count = 0;

			for (Sort.Order order : sort) {
				orderClause.append(count++ > 0 ? ", " : "");
				orderClause.append(String.format("%1$s %2$s", order.getProperty(), order.getDirection()));
			}

			return new QueryString(String.format("%1$s %2$s", this.query, orderClause.toString()));
		}

		return this;
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
