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

import java.util.List;

import org.apache.geode.cache.lucene.LuceneQueryProvider;
import org.springframework.data.domain.Page;
import org.springframework.data.gemfire.search.lucene.ProjectingLuceneOperations;
import org.springframework.data.gemfire.util.RuntimeExceptionFactory;

/**
 * {@link ProjectingLuceneOperationsSupport} is a abstract supporting class for implementations
 * of the {@link ProjectingLuceneOperations} interface.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.search.lucene.ProjectingLuceneOperations
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public abstract class ProjectingLuceneOperationsSupport extends LuceneOperationsSupport
		implements ProjectingLuceneOperations {

	/**
	 * @inheritDoc
	 */
	@Override
	public <T> List<T> query(String query, String defaultField, int resultLimit, Class<T> projectionType) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <T> Page<T> query(String query, String defaultField, int resultLimit, int pageSize,
			Class<T> projectionType) {

		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <T> List<T> query(LuceneQueryProvider queryProvider, int resultLimit, Class<T> projectionType) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <T> Page<T> query(LuceneQueryProvider queryProvider, int resultLimit, int pageSize,
			Class<T> projectionType) {

		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}
}
