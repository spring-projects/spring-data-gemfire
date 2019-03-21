/*
 * Copyright 2016-2019 the original author or authors.
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

import static org.springframework.data.gemfire.search.lucene.support.LucenePage.newLucenePage;

import java.util.List;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneQueryProvider;
import org.springframework.data.domain.Page;

/**
 * {@link ProjectingLuceneTemplate} is a Lucene data access operations class encapsulating functionality
 * for performing Lucene queries and other Lucene data access operations and returning the query results
 * as application-specific domain object views.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.search.lucene.ProjectingLuceneAccessor
 * @see org.springframework.data.gemfire.search.lucene.ProjectingLuceneOperations
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.lucene.LuceneQuery
 * @see org.apache.geode.cache.lucene.LuceneQueryFactory
 * @see org.apache.geode.cache.lucene.LuceneQueryProvider
 * @see org.apache.geode.cache.lucene.LuceneResultStruct
 * @see org.apache.geode.cache.lucene.PageableLuceneQueryResults
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public class ProjectingLuceneTemplate extends ProjectingLuceneAccessor {

	/**
	 * Constructs a default, uninitialized instance of the {@link ProjectingLuceneTemplate}.
	 */
	public ProjectingLuceneTemplate() {
	}

	/**
	 * Constructs an instance of the {@link ProjectingLuceneTemplate} initialized with the given {@link LuceneIndex}
	 * used to perform Lucene queries (searches).
	 *
	 * @param luceneIndex {@link LuceneIndex} used in Lucene queries.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 */
	public ProjectingLuceneTemplate(LuceneIndex luceneIndex) {
		super(luceneIndex);
	}

	/**
	 * Constructs an instance of the {@link ProjectingLuceneTemplate} initialized with the given Lucene index name
	 * and {@link Region} reference upon which Lucene queries are executed.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex} used in Lucene queries.
	 * @param region {@link Region} on which Lucene queries are executed.
	 * @see org.apache.geode.cache.Region
	 */
	public ProjectingLuceneTemplate(String indexName, Region<?, ?> region) {
		super(indexName, region);
	}

	/**
	 * Constructs an instance of the {@link ProjectingLuceneTemplate} initialized with the given Lucene index name
	 * and {@link Region} reference upon which Lucene queries are executed.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex} used in Lucene queries.
	 * @param regionPath {@link String} containing the name of the {@link Region} on which Lucene queries are executed.
	 */
	public ProjectingLuceneTemplate(String indexName, String regionPath) {
		super(indexName, regionPath);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <T> List<T> query(String query, String defaultField, int resultLimit, Class<T> projectionType) {
		return project(query(query, defaultField, resultLimit), projectionType);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <T> Page<T> query(String query, String defaultField, int resultLimit, int pageSize,
			Class<T> projectionType) {

		return newLucenePage(this, query(query, defaultField, resultLimit, pageSize), pageSize, projectionType);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <T> List<T> query(LuceneQueryProvider queryProvider, int resultLimit, Class<T> projectionType) {
		return project(query(queryProvider, resultLimit), projectionType);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <T> Page<T> query(LuceneQueryProvider queryProvider, int resultLimit, int pageSize,
			Class<T> projectionType) {

		return newLucenePage(this, query(queryProvider, resultLimit, pageSize), pageSize, projectionType);
	}
}
