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

package org.springframework.data.gemfire.repository.query;

import org.springframework.core.Ordered;
import org.springframework.data.gemfire.repository.Query;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;

/**
 * {@link QueryPostProcessor} defines a contract for implementations to post process a given {@link String OQL query}
 * and possibly return a new or modified version of the same {@link String OQL query}.
 *
 * {@link QueryPostProcessor QueryPostProcessors} are particularly useful for {@link Repository}
 * {@link QueryMethod query method} generated {@link String queries}, giving the user a chance
 * via the callback to further process the generated {@link String query}.
 *
 * @author John Blum
 * @param <T> {@link Class type} identifying the {@link Repository Repositories} to match on for registration.
 * @param <QUERY> {@link Class type} of the query to process.
 * @see org.springframework.data.repository.Repository
 * @see org.springframework.data.repository.query.QueryMethod
 * @since 2.1.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface QueryPostProcessor<T extends Repository, QUERY> extends Ordered {

	Object[] EMPTY_ARGUMENTS = new Object[0];

	/**
	 * Defines the {@link Integer order} of this {@link QueryPostProcessor} relative to
	 * other {@link QueryPostProcessor QueryPostProcessors} in a sort.
	 *
	 * Defaults to the {@link Ordered#LOWEST_PRECEDENCE}.
	 *
	 * @return an {@link Integer} value specifying the {@link Integer order} of this {@link QueryPostProcessor}
	 * relative to other {@link QueryPostProcessor QueryPostProcessors} in a sort.
	 * @see org.springframework.core.Ordered#getOrder()
	 */
	@Override
	default int getOrder() {
		return Ordered.LOWEST_PRECEDENCE;
	}

	/**
	 * Callback used to post process the given {@link QUERY OQL query} and return a possibly new
	 * or modified {@link String OQL query}.
	 *
	 * This callback is invoked for OQL queries generated from the SD Repository {@link QueryMethod} signature
	 * as well as OQL queries specified using the {@link Query @Query} annotation and OQL queries
	 * defined in a application properties file.
	 *
	 * @param query {@link QUERY OQL query} to process.
	 * @return a possibly new or modified version of the same {@link String OQL query}.
	 * @see org.springframework.data.repository.query.QueryMethod
	 */
	default QUERY postProcess(@NonNull QueryMethod queryMethod, QUERY query) {
		return postProcess(queryMethod, query, EMPTY_ARGUMENTS);
	}

	/**
	 * Callback used to post process the given {@link QUERY OQL query} and return a possibly new
	 * or modified {@link String OQL query}.
	 *
	 * This callback is invoked for OQL queries generated from the SD Repository {@link QueryMethod} signature
	 * as well as OQL queries specified using the {@link Query @Query} annotation and OQL queries
	 * defined in a application properties file.
	 *
	 * @param query {@link QUERY OQL query} to process.
	 * @param arguments array of {@link Object Objects} containing the arguments to the query parameters.
	 * @return a possibly new or modified version of the same {@link String OQL query}.
	 * @see org.springframework.data.repository.query.QueryMethod
	 */
	QUERY postProcess(@NonNull QueryMethod queryMethod, QUERY query, Object... arguments);

	/**
	 * Builder method used to compose 2 {@link QueryPostProcessor QueryPostProcessors}
	 * with this {@link QueryPostProcessor} preceding the given {@link QueryPostProcessor}.
	 *
	 * @param queryPostProcessor {@link QueryPostProcessor} to compose with this {@link QueryPostProcessor}.
	 * @return a composed {@link QueryPostProcessor} consisting of this {@link QueryPostProcessor}
	 * followed by the given {@link QueryPostProcessor}.  Returns this {@link QueryPostProcessor}
	 * if given {@link QueryPostProcessor} is {@literal null}.
	 */
	@NonNull
	default QueryPostProcessor<?, QUERY> processBefore(@Nullable QueryPostProcessor<?, QUERY> queryPostProcessor) {
		return queryPostProcessor == null ? this : (queryMethod, query, arguments) ->
			queryPostProcessor.postProcess(queryMethod, this.postProcess(queryMethod, query, arguments), arguments);
	}

	/**
	 * Builder method used to compose 2 {@link QueryPostProcessor QueryPostProcessors}
	 * with the given {@link QueryPostProcessor} preceding this {@link QueryPostProcessor}.
	 *
	 * @param queryPostProcessor {@link QueryPostProcessor} to compose with this {@link QueryPostProcessor}.
	 * @return a composed {@link QueryPostProcessor} consisting of the given {@link QueryPostProcessor}
	 * followed by this {@link QueryPostProcessor}.  Returns this {@link QueryPostProcessor}
	 * if given {@link QueryPostProcessor} is {@literal null}.
	 */
	@NonNull
	default QueryPostProcessor<?, QUERY> processAfter(@Nullable QueryPostProcessor<?, QUERY> queryPostProcessor) {
		return queryPostProcessor == null ? this : (queryMethod, query, arguments) ->
			this.postProcess(queryMethod, queryPostProcessor.postProcess(queryMethod, query, arguments), arguments);
	}
}
