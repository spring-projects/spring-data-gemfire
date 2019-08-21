/*
 * Copyright 2017-2019 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.admin.functions;

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.query.Index;

import org.springframework.data.gemfire.function.annotation.GemfireFunction;

/**
 * The ListIndexesFunction class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ListIndexesFunction {

	public static final String LIST_INDEXES_FUNCTION_ID = "ListQqlIndexesFunction";

	@GemfireFunction(id = LIST_INDEXES_FUNCTION_ID)
	public Set<String> listIndexes() {

		return Optional.ofNullable(resolveCache())
			.map(cache -> cache.getQueryService())
			.map(queryService ->
				nullSafeCollection(queryService.getIndexes()).stream().map(Index::getName).collect(Collectors.toSet()))
			.orElseGet(Collections::emptySet);
	}

	protected Cache resolveCache() {
		return CacheFactory.getAnyInstance();
	}
}
