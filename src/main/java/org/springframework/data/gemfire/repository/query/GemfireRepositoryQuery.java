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

import org.springframework.data.repository.Repository;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.data.repository.query.RepositoryQuery;
import org.springframework.util.Assert;

/**
 * Base class for GemFire specific {@link RepositoryQuery} implementations.
 * <p>
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.repository.query.GemfireQueryMethod
 * @see org.springframework.data.repository.query.RepositoryQuery
 */
public abstract class GemfireRepositoryQuery implements RepositoryQuery {

	private final GemfireQueryMethod queryMethod;

	private QueryPostProcessor<?, String> queryPostProcessor = ProvidedQueryPostProcessor.IDENTITY;

	/*
	 * (non-Javadoc)
	 * Constructor used for testing purposes only!
	 */
	GemfireRepositoryQuery() {
		this.queryMethod = null;
	}

	/**
	 * Constructs a new instance of {@link GemfireRepositoryQuery} initialized with
	 * the given {@link GemfireQueryMethod}.
	 *
	 * @param queryMethod {@link GemfireQueryMethod} capturing the meta-data
	 * for the {@link Repository} {@link QueryMethod}; must not be {@literal null}.
	 * @throws IllegalArgumentException if {@link GemfireQueryMethod query method} is {@literal null}.
	 * @see org.springframework.data.gemfire.repository.query.GemfireQueryMethod
	 */
	public GemfireRepositoryQuery(GemfireQueryMethod queryMethod) {

		Assert.notNull(queryMethod, "QueryMethod must not be null");

		this.queryMethod = queryMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.RepositoryQuery#getQueryMethod()
	 */
	@Override
	public QueryMethod getQueryMethod() {
		return this.queryMethod;
	}

	/**
	 * Returns a reference to the composed {@link QueryPostProcessor QueryPostProcessors}, which gets applied
	 * to the OQL query prior to execution.
	 *
	 * @return a reference to the composed {@link QueryPostProcessor QueryPostProcessors}.
	 * @see org.springframework.data.gemfire.repository.query.QueryPostProcessor
	 */
	protected QueryPostProcessor<?, String> getQueryPostProcessor() {
		return this.queryPostProcessor;
	}

	/**
	 * Registers the given {@link QueryPostProcessor} used to post process OQL queries
	 * generated from {@link Repository} {@link QueryMethod query methods}.
	 *
	 * @param queryPostProcessor {@link QueryPostProcessor} to register.
	 * @return this {@link GemfireRepositoryQuery}.
	 * @see org.springframework.data.gemfire.repository.query.QueryPostProcessor
	 */
	public GemfireRepositoryQuery register(QueryPostProcessor<?, String> queryPostProcessor) {
		this.queryPostProcessor = this.queryPostProcessor.processBefore(queryPostProcessor);
		return this;
	}

	enum ProvidedQueryPostProcessor implements QueryPostProcessor<Repository, String> {

		IDENTITY {

			@Override
			public String postProcess(QueryMethod queryMethod, String query, Object... arguments) {
				return query;
			}
		}
	}
}
