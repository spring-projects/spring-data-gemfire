/*
 * Copyright 2018 the original author or authors.
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

package org.springframework.data.gemfire.util;

/**
 * The {@link Filter} interface defines a contract for filtering {@link Object objects}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object objects} being filtered.
 * @since 1.0.0
 */
public interface Filter<T> {

	/**
	 * Evaluates the given {@link Object} and determines whether the {@link Object} is accepted
	 * based on the filter criteria.
	 *
	 * @param obj {@link Object} to filter.
	 * @return a boolean value indicating whether this {@link Filter} accepts the given {@link Object}
	 * based on the filter criteria.
	 */
	boolean accept(T obj);

	/**
	 * Combines this {@link Filter} with the given {@link Filter} using {@literal logical AND}.
	 *
	 * @param filter {@link Filter} to compose with this {@link Filter}.
	 * @return a new {@link Filter} composed of this {@link Filter} and the given {@link Filter}
	 * using {@literal logical AND}.
	 * @see org.springframework.data.gemfire.util.Filter
	 */
	Filter<T> and(Filter<T> filter);

	/**
	 * Negates the result of the {@link #accept(Object)} method.
	 *
	 * @return a new {@link Filter} negating the results of the {@link #accept(Object)} method.
	 * @see org.springframework.data.gemfire.util.Filter
	 */
	Filter<T> negate();

	/**
	 * Combines this {@link Filter} with the given {@link Filter} using {@literal logical OR}.
	 *
	 * @param filter {@link Filter} to compose with this {@link Filter}.
	 * @return a new {@link Filter} composed of this {@link Filter} and the given {@link Filter}
	 * using {@literal logical OR}.
	 * @see org.springframework.data.gemfire.util.Filter
	 */
	Filter<T> or(Filter<T> filter);

}
