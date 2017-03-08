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

import java.util.List;

import org.apache.geode.cache.lucene.LuceneQueryProvider;
import org.springframework.data.domain.Page;

/**
 * The {@link MappingLuceneOperations} interface defines a contract for implementing classes to execute
 * Lucene data access operations and mapping the results to entity domain {@link Class types}.
 *
 * @author John Blum
 * @see org.springframework.data.domain.Page
 * @see org.springframework.data.gemfire.search.lucene.LuceneOperations
 * @see org.apache.geode.cache.lucene.LuceneQueryProvider
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public interface MappingLuceneOperations extends LuceneOperations {

	default <T> List<T> query(Class<T> entityType, String query, String defaultField,
			String... projectionFields) {

		return query(entityType, query, defaultField, DEFAULT_RESULT_LIMIT, projectionFields);
	}

	<T> List<T> query(Class<T> entityType, String query, String defaultField,
		int resultLimit, String... projectionFields);

	<T> Page<T> query(Class<T> entityType, String query, String defaultField,
		int resultLimit, int pageSize, String... projectionFields);

	default <T> List<T> query(Class<T> entityType, LuceneQueryProvider queryProvider,
			String... projectionFields) {

		return query(entityType, queryProvider, DEFAULT_RESULT_LIMIT, projectionFields);
	}

	<T> List<T> query(Class<T> entityType, LuceneQueryProvider queryProvider,
		int resultLimit, String... projectionFields);

	<T> Page<T> query(Class<T> entityType, LuceneQueryProvider queryProvider,
		int resultLimit, int pageSize, String... projectionFields);

}
