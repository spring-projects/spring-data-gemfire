/*
 * Copyright 2002-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.springframework.data.gemfire.function.execution;

import java.util.Set;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.execute.Function;

/**
 * Interface define {@link Region} {@link Function} data access operations.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.execute.Function
 * @see org.springframework.data.gemfire.function.execution.GemfireFunctionOperations
 */
@SuppressWarnings("unused")
public interface GemfireOnRegionOperations extends GemfireFunctionOperations {

	default <T> Iterable<T> execute(Function function, Set<?> keys, Object... args) {
		return execute(function.getId(), keys, args);
	}

	<T> Iterable<T> execute(String functionId, Set<?> keys, Object... args);

	default <T> T executeAndExtract(Function function, Set<?> keys, Object... args) {
		return executeAndExtract(function.getId(), keys, args);
	}

	<T> T executeAndExtract(String functionId, Set<?> keys, Object... args);

	default void executeWithNoResult(Function function, Set<?> keys, Object... args) {
		executeWithNoResult(function.getId(), keys, args);
	}

	void executeWithNoResult(String functionId, Set<?> keys, Object... args);

}
