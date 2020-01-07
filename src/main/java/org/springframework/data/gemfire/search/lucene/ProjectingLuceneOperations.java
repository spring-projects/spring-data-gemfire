/*
 * Copyright 2016-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.search.lucene;

import java.util.List;

import org.apache.geode.cache.lucene.LuceneQueryProvider;

import org.springframework.data.domain.Page;

/**
 * The {@link ProjectingLuceneOperations} interface defines a contract for implementing classes to execute
 * Lucene data access operations and mapping the results to entity domain {@link Class types}.
 *
 * @author John Blum
 * @see java.util.List
 * @see org.springframework.data.domain.Page
 * @see org.springframework.data.gemfire.search.lucene.LuceneOperations
 * @see org.apache.geode.cache.lucene.LuceneQueryProvider
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public interface ProjectingLuceneOperations extends LuceneOperations {

	/**
	 * Executes the given {@link String query} with the results projected as instances of
	 * the {@link Class projectionType}.
	 *
	 * @param <T> {@link Class} type of the projection.
	 * @param query Lucene {@link String query} to execute.
	 * @param defaultField {@link String} specifying the default field used in Lucene queries when a field
	 * is not explicitly defined in the Lucene query clause.
	 * @param projectionType {@link Class} type of the individual elements in the query results.
	 * @return a {@link List} of Lucene query results projected as instances of {@link Class projectionType}.
	 * @see #query(String, String, int, Class)
	 * @see java.util.List
	 */
	default <T> List<T> query(String query, String defaultField, Class<T> projectionType) {
		return query(query, defaultField, DEFAULT_RESULT_LIMIT, projectionType);
	}

	/**
	 * Executes the given {@link String query} with the limited results projected as instances of
	 * the {@link Class projectionType}.
	 *
	 * @param <T> {@link Class} type of the projection.
	 * @param query Lucene {@link String query} to execute.
	 * @param defaultField {@link String} specifying the default field used in Lucene queries when a field
	 * is not explicitly defined in the Lucene query clause.
	 * @param resultLimit limit on the number of query results to return.
	 * @param projectionType {@link Class} type of the individual elements in the query results.
	 * @return a {@link List} of Lucene query results projected as instances of {@link Class projectionType}.
	 * @see #query(String, String, int, Class)
	 * @see java.util.List
	 */
	<T> List<T> query(String query, String defaultField, int resultLimit, Class<T> projectionType);

	/**
	 * Executes the given {@link String query} with the limited results projected as instances of
	 * the {@link Class projectionType}.
	 *
	 * @param <T> {@link Class} type of the projection.
	 * @param query Lucene {@link String query} to execute.
	 * @param defaultField {@link String} specifying the default field used in Lucene queries when a field
	 * is not explicitly defined in the Lucene query clause.
	 * @param resultLimit limit on the number of query results to return.
	 * @param pageSize number of results on a {@link Page}.
	 * @param projectionType {@link Class} type of the individual elements in the query results.
	 * @return the first {@link Page} of results returned from the Lucene query.
	 * @see org.springframework.data.domain.Page
	 */
	<T> Page<T> query(String query, String defaultField, int resultLimit, int pageSize, Class<T> projectionType);

	/**
	 * Executes the provided {@link String query} with the results projected as instances of
	 * the {@link Class projectionType}.
	 *
	 * @param <T> {@link Class} type of the projection.
	 * @param queryProvider {@link LuceneQueryProvider} providing the Lucene {@link String query} to execute.
	 * @param projectionType {@link Class} type of the individual elements in the query results.
	 * @return a {@link List} of Lucene query results projected as instances of {@link Class projectionType}.
	 * @see org.apache.geode.cache.lucene.LuceneQueryProvider
	 * @see #query(LuceneQueryProvider, int, Class)
	 * @see java.util.List
	 */
	default <T> List<T> query(LuceneQueryProvider queryProvider, Class<T> projectionType) {
		return query(queryProvider, DEFAULT_RESULT_LIMIT, projectionType);
	}

	/**
	 * Executes the provided {@link String query} with the limited results projected as instances of
	 * the {@link Class projectionType}.
	 *
	 * @param <T> {@link Class} type of the projection.
	 * @param queryProvider {@link LuceneQueryProvider} providing the Lucene {@link String query} to execute.
	 * @param resultLimit limit on the number of query results to return.
	 * @param projectionType {@link Class} type of the individual elements in the query results.
	 * @return a {@link List} of Lucene query results projected as instances of {@link Class projectionType}.
	 * @see org.apache.geode.cache.lucene.LuceneQueryProvider
	 * @see java.util.List
	 */
	<T> List<T> query(LuceneQueryProvider queryProvider, int resultLimit, Class<T> projectionType);

	/**
	 * Executes the provided {@link String query} with the limited results projected as instances of
	 * the {@link Class projectionType}.
	 *
	 * @param <T> {@link Class} type of the projection.
	 * @param queryProvider {@link LuceneQueryProvider} providing the Lucene {@link String query} to execute.
	 * @param resultLimit limit on the number of query results to return.
	 * @param pageSize number of results on a {@link Page}.
	 * @param projectionType {@link Class} type of the individual elements in the query results.
	 * @return the first {@link Page} of results returned from the Lucene query.
	 * @see org.apache.geode.cache.lucene.LuceneQueryProvider
	 * @see org.springframework.data.domain.Page
	 */
	<T> Page<T> query(LuceneQueryProvider queryProvider, int resultLimit, int pageSize, Class<T> projectionType);

}
