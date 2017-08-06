/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.admin.functions;

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.query.QueryException;
import org.apache.geode.cache.query.QueryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.gemfire.GemfireCacheUtils;
import org.springframework.data.gemfire.config.schema.definitions.IndexDefinition;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;

/**
 * The CreateIndexFunction class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class CreateIndexFunction {

	public static final String CREATE_INDEX_FUNCTION_ID = "CreateOqlIndexFunction";

	private final Logger logger = LoggerFactory.getLogger(getClass());

	@GemfireFunction(id = CREATE_INDEX_FUNCTION_ID)
	public boolean createIndex(IndexDefinition indexDefinition) {

		Cache gemfireCache = resolveCache();

		if (isNonExistingIndex(gemfireCache, indexDefinition)) {

			if (logger.isInfoEnabled()) {
				logger.info("Creating Index with name [{}] having expression [{}] on Region [{}] with type [{}]",
					indexDefinition.getName(), indexDefinition.getExpression(), indexDefinition.getFromClause(),
						indexDefinition.getIndexType());
			}

			QueryService queryService = gemfireCache.getQueryService();

			try {
				switch (indexDefinition.getIndexType()) {
					case KEY:
					case PRIMARY_KEY:
						queryService.createKeyIndex(indexDefinition.getName(),
							indexDefinition.getExpression(), indexDefinition.getFromClause());
						return true;
					case HASH:
						queryService.createHashIndex(indexDefinition.getName(),
							indexDefinition.getExpression(), indexDefinition.getFromClause());
						return true;
					case FUNCTIONAL:
						queryService.createIndex(indexDefinition.getName(),
							indexDefinition.getExpression(), indexDefinition.getFromClause());
						return true;
					default:
						return false;
				}
			}
			catch (QueryException cause) {
				throw GemfireCacheUtils.convertGemfireAccessException(cause);
			}
		}
		else {

			if (logger.isInfoEnabled()) {
				logger.info("Index with name [{}] already exists", indexDefinition.getName());
			}

			return false;
		}
	}

	protected Cache resolveCache() {
		return CacheFactory.getAnyInstance();
	}

	protected boolean isNonExistingIndex(GemFireCache gemfireCache, IndexDefinition indexDefinition) {
		return !nullSafeCollection(gemfireCache.getQueryService().getIndexes()).stream()
			.anyMatch(index -> index.getName().equals(indexDefinition.getName()));
	}
}
