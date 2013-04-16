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

import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.repository.query.parser.PartTree;
import org.springframework.util.Assert;

/**
 * 
 * @author Oliver Gierke
 * @author David Turanski
 */
class QueryBuilder {

	static final String DEFAULT_ALIAS = "x";

	private final String query;

	public QueryBuilder(String source) {
		Assert.hasText(source);
		this.query = source;
	}

	public QueryBuilder(GemfirePersistentEntity<?> entity, PartTree tree) {
		this(String.format(tree.isCountProjection()? "SELECT count(*) FROM /%s %s":"SELECT * FROM /%s %s", entity.getRegionName(), DEFAULT_ALIAS));
	}

	public QueryString create(Predicate predicate) {

		return new QueryString(query + " WHERE " + predicate.toString(DEFAULT_ALIAS));
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return query;
	}
}
