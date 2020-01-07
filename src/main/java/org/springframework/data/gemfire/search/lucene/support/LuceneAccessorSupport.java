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

package org.springframework.data.gemfire.search.lucene.support;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newUnsupportedOperationException;

import java.util.Collection;
import java.util.List;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneQueryProvider;
import org.apache.geode.cache.lucene.LuceneResultStruct;
import org.apache.geode.cache.lucene.PageableLuceneQueryResults;
import org.springframework.data.gemfire.search.lucene.LuceneAccessor;
import org.springframework.data.gemfire.util.RuntimeExceptionFactory;

/**
 * {@link LuceneAccessorSupport} is a {@link LuceneAccessor} class implementation providing support
 * for extending classes.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.search.lucene.LuceneAccessor
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public abstract class LuceneAccessorSupport extends LuceneAccessor {

	/**
	 * Constructs an uninitialized instance of {@link LuceneAccessorSupport}.
	 */
	@SuppressWarnings("all")
	public LuceneAccessorSupport() {
	}

	/**
	 * Constructs an instance of {@link LuceneAccessorSupport} initialized with the given {@link LuceneIndex}.
	 *
	 * @param luceneIndex {@link LuceneIndex} used in Lucene queries.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 */
	public LuceneAccessorSupport(LuceneIndex luceneIndex) {
		super(luceneIndex);
	}

	/**
	 * Constructs an instance of {@link LuceneAccessorSupport} initialized with the given Lucene
	 * {@link String index name} and {@link Region}.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex}.
	 * @param region {@link Region} on which the Lucene query is executed.
	 * @see org.apache.geode.cache.Region
	 */
	public LuceneAccessorSupport(String indexName, Region<?, ?> region) {
		super(indexName, region);
	}

	/**
	 * Constructs an instance of {@link LuceneAccessorSupport} initialized with the given Lucene
	 * {@link String index name} and {@link String fully-qualified Region path}.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex}.
	 * @param regionPath {@link String} containing the fully-qualified path of the {@link Region}.
	 */
	public LuceneAccessorSupport(String indexName, String regionPath) {
		super(indexName, regionPath);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> List<LuceneResultStruct<K, V>> query(String query, String defaultField, int resultLimit) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> PageableLuceneQueryResults<K, V> query(String query, String defaultField,
			int resultLimit, int pageSize) {

		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> List<LuceneResultStruct<K, V>> query(LuceneQueryProvider queryProvider, int resultLimit) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> PageableLuceneQueryResults<K, V> query(LuceneQueryProvider queryProvider,
			int resultLimit, int pageSize) {

		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K> Collection<K> queryForKeys(String query, String defaultField, int resultLimit) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K> Collection<K> queryForKeys(LuceneQueryProvider queryProvider, int resultLimit) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <V> Collection<V> queryForValues(String query, String defaultField, int resultLimit) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <V> Collection<V> queryForValues(LuceneQueryProvider queryProvider, int resultLimit) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}
}
