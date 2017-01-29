/*
 * Copyright 2017-2018 the original author or authors.
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

package org.springframework.data.gemfire.domain.support;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexStatistics;
import org.apache.geode.cache.query.IndexType;

/**
 * {@link AbstractIndexSupport} is an abstract base class supporting the implementation
 * of the Pivotal GemFire / Apache Geode {@link Index} interface.
 *
 * @author John Blum
 * @see org.apache.geode.cache.query.Index
 * @since 2.0.0
 */
public abstract class AbstractIndexSupport implements Index {

	private static final String NOT_IMPLEMENTED = "Not Implemented";

	@Override
	public String getCanonicalizedFromClause() {
		return getFromClause();
	}

	@Override
	public String getCanonicalizedIndexedExpression() {
		return getIndexedExpression();
	}

	@Override
	public String getCanonicalizedProjectionAttributes() {
		return getProjectionAttributes();
	}

	@Override
	public String getFromClause() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	@Override
	public String getIndexedExpression() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	@Override
	public String getName() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	@Override
	public String getProjectionAttributes() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	@Override
	public Region<?, ?> getRegion() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	@Override
	public IndexStatistics getStatistics() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	@Override
	@SuppressWarnings("deprecation")
	public IndexType getType() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}
}
