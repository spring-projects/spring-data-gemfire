/*
 * Copyright 2012-2018 the original author or authors.
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
abstract class GemfireRepositoryQuery implements RepositoryQuery {

	private final GemfireQueryMethod queryMethod;

	/*
	 * (non-Javadoc)
	 * Constructor used for testing purposes only!
	 */
	GemfireRepositoryQuery() {
		queryMethod = null;
	}

	/**
	 * Creates a new {@link GemfireRepositoryQuery} using the given {@link GemfireQueryMethod}.
	 *
	 * @param queryMethod must not be {@literal null}.
	 */
	public GemfireRepositoryQuery(GemfireQueryMethod queryMethod) {
		Assert.notNull(queryMethod);
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

}
