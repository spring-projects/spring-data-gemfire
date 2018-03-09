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

import java.util.Properties;

import org.springframework.core.Ordered;
import org.springframework.data.gemfire.repository.Query;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.core.NamedQueries;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;

/**
 * The {@link QueryPostProcessor} interface defines a contract for implementations to post process
 * a given {@link QUERY query} and possibly return a new or modified version of the same {@link QUERY query}.
 *
 * {@link QueryPostProcessor QueryPostProcessors} are useful for handling and processing {@link QUERY queries}
 * generated from {@link Repository} {@link QueryMethod query methods}, and give a developer an opportunity,
 * via the callback, to further process the generated {@link QUERY query}.
 *
 * {@link QueryPostProcessor QueryPostProcessors} can be used on both generated {@link QUERY queries}
 * and {@literal manual} {@link QUERY queries}.  {@literal Manual} {@link QUERY queries} are defined as
 * {@link QUERY queries} specified using SDG's {@link Query @Query} annotation or by defining a {@literal named}
 * {@link QUERY query} in a module-specific {@link Properties} files.
 *
 * @author John Blum
 * @param <T> {@link Class type} identifying the {@link Repository Repositories} to match on during registration.
 * @param <QUERY> {@link Class type} of the query to process.
 * @see org.springframework.core.Ordered
 * @see org.springframework.data.repository.Repository
 * @see org.springframework.data.repository.query.QueryMethod
 * @since 2.1.0
 */
@FunctionalInterface
public interface QueryPostProcessor<T extends Repository, QUERY> extends Ordered {

	Object[] EMPTY_ARGUMENTS = new Object[0];

	/**
	 * Defines the {@link Integer order} of this {@link QueryPostProcessor} relative to
	 * other {@link QueryPostProcessor QueryPostProcessors} in a sort.
	 *
	 * Defaults to the {@link Ordered#LOWEST_PRECEDENCE}.
	 *
	 * @return an {@link Integer} value specifying the order of this {@link QueryPostProcessor} relative to
	 * other {@link QueryPostProcessor QueryPostProcessors} in a sort.
	 * @see org.springframework.core.Ordered#getOrder()
	 */
	@Override
	default int getOrder() {
		return Ordered.LOWEST_PRECEDENCE;
	}

	/**
	 * Callback method invoked by the Spring Data (SD) {@link Repository} framework to allow the user to process
	 * the given {@link QUERY query} and (possibly) return a new or modified version of the {@link QUERY query}.
	 *
	 * This callback is invoked for {@literal queries} generated from the SD {@link Repository} {@link QueryMethod}
	 * signature as well as {@literal queries} specified and defined in {@link NamedQueries},
	 * or even using SDG's {@link Query @Query} annotation.
	 *
	 * @param query {@link QUERY query} to process.
	 * @return a new or modified version of the same {@link QUERY query}.
	 * @see org.springframework.data.repository.query.QueryMethod
	 * @see #postProcess(QueryMethod, Object, Object...)
	 */
	default QUERY postProcess(@NonNull QueryMethod queryMethod, QUERY query) {
		return postProcess(queryMethod, query, EMPTY_ARGUMENTS);
	}

	/**
	 * Callback method invoked by the Spring Data (SD) {@link Repository} framework to allow the user to process
	 * the given {@link QUERY query} and (possibly) return a new or modified version of the {@link QUERY query}.
	 *
	 * This callback is invoked for {@literal queries} generated from the SD {@link Repository} {@link QueryMethod}
	 * signature as well as {@literal queries} specified and defined in {@link NamedQueries},
	 * or even using SDG's {@link Query @Query} annotation.
	 *
	 * @param query {@link QUERY query} to process.
	 * @param arguments array of {@link Object Objects} containing the arguments to the query parameters.
	 * @return a new or modified version of the same {@link QUERY query}.
	 * @see org.springframework.data.repository.query.QueryMethod
	 * @see #postProcess(QueryMethod, Object)
	 */
	QUERY postProcess(@NonNull QueryMethod queryMethod, QUERY query, Object... arguments);

	/**
	 * Builder method used to compose, or combine this {@link QueryPostProcessor QueryPostProcessors}
	 * with the given {@link QueryPostProcessor}.
	 *
	 * This {@link QueryPostProcessor} come before the given {@link QueryPostProcessor} in the processing chain.
	 *
	 * @param queryPostProcessor {@link QueryPostProcessor} to compose with this {@link QueryPostProcessor}.
	 * @return a composed {@link QueryPostProcessor} consisting of this {@link QueryPostProcessor}
	 * followed by the given {@link QueryPostProcessor}.  Returns this {@link QueryPostProcessor}
	 * if the given {@link QueryPostProcessor} is {@literal null}.
	 * @see #processAfter(QueryPostProcessor)
	 */
	@NonNull
	default QueryPostProcessor<?, QUERY> processBefore(@Nullable QueryPostProcessor<?, QUERY> queryPostProcessor) {
		return queryPostProcessor == null ? this : (queryMethod, query, arguments) ->
			queryPostProcessor.postProcess(queryMethod, this.postProcess(queryMethod, query, arguments), arguments);
	}

	/**
	 * Builder method used to compose, or combine this {@link QueryPostProcessor QueryPostProcessors}
	 * with the given {@link QueryPostProcessor}.
	 *
	 * This {@link QueryPostProcessor} will come after this {@link QueryPostProcessor} in the processing chain.
	 *
	 * @param queryPostProcessor {@link QueryPostProcessor} to compose with this {@link QueryPostProcessor}.
	 * @return a composed {@link QueryPostProcessor} consisting of the given {@link QueryPostProcessor}
	 * followed by this {@link QueryPostProcessor}.  Returns this {@link QueryPostProcessor}
	 * if the given {@link QueryPostProcessor} is {@literal null}.
	 * @see #processBefore(QueryPostProcessor)
	 */
	@NonNull
	default QueryPostProcessor<?, QUERY> processAfter(@Nullable QueryPostProcessor<?, QUERY> queryPostProcessor) {
		return queryPostProcessor == null ? this : (queryMethod, query, arguments) ->
			this.postProcess(queryMethod, queryPostProcessor.postProcess(queryMethod, query, arguments), arguments);
	}
}
