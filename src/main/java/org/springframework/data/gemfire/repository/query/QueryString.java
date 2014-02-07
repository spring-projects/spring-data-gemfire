/*
 * Copyright 2012 the original author or authors.
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

import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.Region;

/**
 * Value object to work with OQL query strings.
 * 
 * @author Oliver Gierke
 * @author David Turanski
 */
class QueryString {

	//private static final String REGION_PATTERN = "(?<=\\/)\\w+";
	private static final String REGION_PATTERN = "\\/(\\/?\\w)+";
	private static final String IN_PARAMETER_PATTERN = "(?<=IN (SET|LIST) \\$)\\d";
	private static final String IN_PATTERN = "(?<=IN (SET|LIST) )\\$\\d";

	private final String query;

	/**
	 * Creates a {@link QueryString} from the given {@link String} query.
	 * 
	 * @param source
	 */
	public QueryString(String source) {

		Assert.hasText(source);
		this.query = source;
	}

	/**
	 * Creates a {@literal SELECT} query for the given domain class.
	 * 
	 * @param domainClass must not be {@literal null}.
	 */
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
		this(String
				.format(isCountQuery ? "SELECT count(*) FROM /%s" : "SELECT * FROM /%s", domainClass.getSimpleName()));
	}

	/**
	 * Replaces the domain classes referenced inside the current query with the given {@link Region}.
	 * 
	 * @param domainClass must not be {@literal null}.
	 * @param region must not be {@literal null}.
	 * @return
	 */
	public QueryString forRegion(Class<?> domainClass, Region<?, ?> region) {
		return new QueryString(query.replaceAll(REGION_PATTERN, region.getFullPath()));
	}

	/**
	 * Binds the given values to the {@literal IN} parameter keyword by expanding the given values into a comma-separated
	 * {@link String}.
	 * 
	 * @param values the values to bind, returns the {@link QueryString} as is if {@literal null} is given.
	 * @return
	 */
	public QueryString bindIn(Collection<?> values) {

		if (values == null) {
			return this;
		}

		String valueString = StringUtils.collectionToDelimitedString(values, ", ", "'", "'");
		return new QueryString(query.replaceFirst(IN_PATTERN, String.format("(%s)", valueString)));
	}

	/**
	 * Returns the parameter indexes used in this query.
	 * 
	 * @return the parameter indexes used in this query or an empty {@link Iterable} if none are used.
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return query;
	}
}
