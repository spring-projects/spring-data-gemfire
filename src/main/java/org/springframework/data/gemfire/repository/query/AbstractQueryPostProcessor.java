/*
 * Copyright 2017-2019 the original author or authors.
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

package org.springframework.data.gemfire.repository.query;

import java.util.Properties;

import org.springframework.core.Ordered;
import org.springframework.data.gemfire.repository.Query;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.core.NamedQueries;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.util.Assert;

/**
 * The {@link AbstractQueryPostProcessor} class is an abstract base class for simplifying the implementation
 * of {@link QueryPostProcessor QueryPostProcessors}.
 *
 * {@link QueryPostProcessor QueryPostProcessors} are useful for handling and processing {@link QUERY queries}
 * generated from {@link Repository} {@link QueryMethod query methods}, and give a developer an opportunity,
 * via the callback, to further process the generated {@link QUERY query}.
 *
 * {@link QueryPostProcessor QueryPostProcessors} can be used on both {@literal generated} {@link QUERY queries}
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
public abstract class AbstractQueryPostProcessor<T extends Repository, QUERY> implements QueryPostProcessor<T, QUERY> {

	protected static final Object[] EMPTY_ARRAY = {};

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
	public int getOrder() {
		return Ordered.LOWEST_PRECEDENCE;
	}

	/**
	 * Callback method invoked by the Spring Data (SD) {@link Repository} framework to allow the user to process
	 * the given {@link QUERY query} and (possibly) return a new or modified version of the {@link QUERY query}.
	 *
	 * This callback is invoked for {@literal queries} generated from a SD {@link Repository} {@link QueryMethod}
	 * signature as well as {@literal queries} specified and defined in {@link NamedQueries},
	 * or even using SDG's {@link Query @Query} annotation.
	 *
	 * @param query {@link QUERY query} to process.
	 * @return a new or modified version of the same {@link QUERY query}.
	 * @see org.springframework.data.repository.query.QueryMethod
	 * @see #postProcess(QueryMethod, Object, Object...)
	 */
	@Override
	public QUERY postProcess(QueryMethod queryMethod, QUERY query) {
		return postProcess(queryMethod, query, EMPTY_ARRAY);
	}

	/**
	 * Builder method used to compose, or combine this {@link QueryPostProcessor QueryPostProcessors}
	 * with the given {@link QueryPostProcessor}.
	 *
	 * This {@link QueryPostProcessor} will come before the given {@link QueryPostProcessor} in the processing chain.
	 *
	 * @param queryPostProcessor {@link QueryPostProcessor} to compose with this {@link QueryPostProcessor}.
	 * @return a composed {@link QueryPostProcessor} consisting of this {@link QueryPostProcessor}
	 * followed by the given {@link QueryPostProcessor}.  Returns this {@link QueryPostProcessor}
	 * if the given {@link QueryPostProcessor} is {@literal null}.
	 * @see #processAfter(QueryPostProcessor)
	 */
	public QueryPostProcessor<?, QUERY> processBefore(QueryPostProcessor<?, QUERY> queryPostProcessor) {
		return ComposableQueryPostProcessor.compose(this, queryPostProcessor);
	}

	/**
	 * Builder method used to compose, or combine this {@link QueryPostProcessor} with
	 * the given {@link QueryPostProcessor}.
	 *
	 * This {@link QueryPostProcessor} will come after the given {@link QueryPostProcessor} in the processing chain.
	 *
	 * @param queryPostProcessor {@link QueryPostProcessor} to compose with this {@link QueryPostProcessor}.
	 * @return a composed {@link QueryPostProcessor} consisting of the given {@link QueryPostProcessor}
	 * followed by this {@link QueryPostProcessor}.  Returns this {@link QueryPostProcessor}
	 * if the given {@link QueryPostProcessor} is {@literal null}.
	 * @see #processBefore(QueryPostProcessor)
	 */
	public QueryPostProcessor<?, QUERY> processAfter(QueryPostProcessor<?, QUERY> queryPostProcessor) {
		return ComposableQueryPostProcessor.compose(queryPostProcessor, this);
	}

	protected static class ComposableQueryPostProcessor<T extends Repository, QUERY>
			extends AbstractQueryPostProcessor<T, QUERY> {

		@SuppressWarnings("unchecked")
		protected static <QUERY> QueryPostProcessor<?, QUERY> compose(
				QueryPostProcessor<?, QUERY> first, QueryPostProcessor<?, QUERY> second) {

			return first == null ? second : second == null ? first
				: new ComposableQueryPostProcessor<Repository, QUERY>(first, second);
		}

		private final QueryPostProcessor<?, QUERY> first;
		private final QueryPostProcessor<?, QUERY> second;

		protected ComposableQueryPostProcessor(QueryPostProcessor<?, QUERY> left, QueryPostProcessor<?, QUERY> second) {

			Assert.notNull(left, "The first QueryPostProcessor operand must not be null");
			Assert.notNull(second, "The second QueryPostProcessor operand must not be null");

			this.first = left;
			this.second = second;
		}

		protected QueryPostProcessor<?, QUERY> getFirst() {
			return this.first;
		}

		protected QueryPostProcessor<?, QUERY> getSecond() {
			return this.second;
		}

		@Override
		public QUERY postProcess(QueryMethod queryMethod, QUERY query, Object... arguments) {

			return getSecond().postProcess(queryMethod,
				getFirst().postProcess(queryMethod, query, arguments), arguments);
		}
	}
}
