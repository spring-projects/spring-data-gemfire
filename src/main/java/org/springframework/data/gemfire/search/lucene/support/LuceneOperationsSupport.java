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

package org.springframework.data.gemfire.search.lucene.support;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newUnsupportedOperationException;

import java.util.Collection;
import java.util.List;

import org.apache.geode.cache.lucene.LuceneQueryProvider;
import org.apache.geode.cache.lucene.LuceneResultStruct;
import org.apache.geode.cache.lucene.PageableLuceneQueryResults;
import org.springframework.data.gemfire.search.lucene.LuceneOperations;
import org.springframework.data.gemfire.util.RuntimeExceptionFactory;

/**
 * {@link LuceneOperationsSupport} is a abstract supporting class for implementations
 * of the {@link LuceneOperations} interface.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.search.lucene.LuceneOperations
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public abstract class LuceneOperationsSupport implements LuceneOperations {

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> List<LuceneResultStruct<K, V>> query(String query, String defaultField,
			int resultLimit, String... projectionFields) {

		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> PageableLuceneQueryResults<K, V> query(String query, String defaultField,
			int resultLimit, int pageSize, String... projectionFields) {

		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> List<LuceneResultStruct<K, V>> query(LuceneQueryProvider queryProvider,
			int resultLimit, String... projectionFields) {

		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <K, V> PageableLuceneQueryResults<K, V> query(LuceneQueryProvider queryProvider,
			int resultLimit, int pageSize, String... projectionFields) {

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
