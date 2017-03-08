/*
 * Copyright 2016 the original author or authors.
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

package org.springframework.data.gemfire.search.lucene;

import java.util.Collection;
import java.util.List;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneQuery;
import org.apache.geode.cache.lucene.LuceneQueryFactory;
import org.apache.geode.cache.lucene.LuceneQueryProvider;
import org.apache.geode.cache.lucene.LuceneResultStruct;
import org.apache.geode.cache.lucene.PageableLuceneQueryResults;
import org.springframework.data.gemfire.search.lucene.support.LuceneAccessorSupport;

/**
 * {@link LuceneTemplate} is a Lucene data access operations class encapsulating functionality
 * for performing Lucene queries and other Lucene data access operations.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.search.lucene.LuceneAccessor
 * @see org.springframework.data.gemfire.search.lucene.LuceneOperations
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
public class LuceneTemplate extends LuceneAccessorSupport implements LuceneOperations {

	/**
	 * Constructs an uninitialized instance of {@link LuceneTemplate}.
	 */
	public LuceneTemplate() {
	}

	/**
	 * Constructs an instance of {@link LuceneTemplate} initialized with the given {@link LuceneIndex}.
	 *
	 * @param luceneIndex {@link LuceneIndex} used in Lucene queries.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 */
	public LuceneTemplate(LuceneIndex luceneIndex) {
		super(luceneIndex);
	}

	/**
	 * Constructs an instance of {@link LuceneTemplate} initialized with the given Lucene {@link String index name}
	 * and {@link Region}.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex}.
	 * @param region {@link Region} on which the Lucene query is executed.
	 * @see org.apache.geode.cache.Region
	 */
	public LuceneTemplate(String indexName, Region<?, ?> region) {
		super(indexName, region);
	}

	/**
	 * Constructs an instance of {@link LuceneTemplate} initialized with the given Lucene {@link String index name}
	 * and {@link String fully-qualified Region path}.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex}.
	 * @param regionPath {@link String} containing the fully-qualified path of the {@link Region}.
	 */
	public LuceneTemplate(String indexName, String regionPath) {
		super(indexName, regionPath);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> List<LuceneResultStruct<K, V>> query(String query, String defaultField,
			int resultLimit, String... projectionFields) {

		String indexName = resolveIndexName();
		String regionPath = resolveRegionPath();

		LuceneQueryFactory queryFactory = createLuceneQueryFactory(resultLimit, projectionFields);

		LuceneQuery<K, V> queryWrapper = queryFactory.create(indexName, regionPath, query, defaultField);

		return doFind(queryWrapper::findResults, query, regionPath, indexName);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> PageableLuceneQueryResults<K, V> query(String query, String defaultField,
			int resultLimit, int pageSize, String... projectionFields) {

		String indexName = resolveIndexName();
		String regionPath = resolveRegionPath();

		LuceneQueryFactory queryFactory = createLuceneQueryFactory(resultLimit, pageSize, projectionFields);

		LuceneQuery<K, V> queryWrapper = queryFactory.create(indexName, regionPath, query, defaultField);

		return doFind(queryWrapper::findPages, query, regionPath, indexName);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> List<LuceneResultStruct<K, V>> query(LuceneQueryProvider queryProvider,
			int resultLimit, String... projectionFields) {

		String indexName = resolveIndexName();
		String regionPath = resolveRegionPath();

		LuceneQueryFactory queryFactory = createLuceneQueryFactory(resultLimit, projectionFields);

		LuceneQuery<K, V> queryWrapper = queryFactory.create(indexName, regionPath, queryProvider);

		return doFind(queryWrapper::findResults, queryProvider, regionPath, indexName);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> PageableLuceneQueryResults<K, V> query(LuceneQueryProvider queryProvider,
			int resultLimit, int pageSize, String... projectionFields) {

		String indexName = resolveIndexName();
		String regionPath = resolveRegionPath();

		LuceneQueryFactory queryFactory = createLuceneQueryFactory(resultLimit, pageSize, projectionFields);

		LuceneQuery<K, V> queryWrapper = queryFactory.create(indexName, regionPath, queryProvider);

		return doFind(queryWrapper::findPages, queryProvider, regionPath, indexName);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K> Collection<K> queryForKeys(String query, String defaultField, int resultLimit) {
		String indexName = resolveIndexName();
		String regionPath = resolveRegionPath();

		LuceneQueryFactory queryFactory = createLuceneQueryFactory(resultLimit);

		LuceneQuery<K, ?> queryWrapper = queryFactory.create(indexName, regionPath, query, defaultField);

		return doFind(queryWrapper::findKeys, query, regionPath, indexName);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K> Collection<K> queryForKeys(LuceneQueryProvider queryProvider, int resultLimit) {
		String indexName = resolveIndexName();
		String regionPath = resolveRegionPath();

		LuceneQueryFactory queryFactory = createLuceneQueryFactory(resultLimit);

		LuceneQuery<K, ?> queryWrapper = queryFactory.create(indexName, regionPath, queryProvider);

		return doFind(queryWrapper::findKeys, queryProvider, regionPath, indexName);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <V> Collection<V> queryForValues(String query, String defaultField, int resultLimit) {
		String indexName = resolveIndexName();
		String regionPath = resolveRegionPath();

		LuceneQueryFactory queryFactory = createLuceneQueryFactory(resultLimit);

		LuceneQuery<?, V> queryWrapper = queryFactory.create(indexName, regionPath, query, defaultField);

		return doFind(queryWrapper::findValues, query, regionPath, indexName);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <V> Collection<V> queryForValues(LuceneQueryProvider queryProvider, int resultLimit) {
		String indexName = resolveIndexName();
		String regionPath = resolveRegionPath();

		LuceneQueryFactory queryFactory = createLuceneQueryFactory(resultLimit);

		LuceneQuery<?, V> queryWrapper = queryFactory.create(indexName, regionPath, queryProvider);

		return doFind(queryWrapper::findValues, queryProvider, regionPath, indexName);
	}
}
