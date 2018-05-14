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

import java.util.function.Predicate;

import org.springframework.lang.Nullable;

/**
 * The {@link Filter} interface defines a contract for filtering {@link Object objects}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object objects} being filtered.
 * @see java.util.function.Predicate
 * @since 1.0.0
 */
public interface Filter<T> extends Predicate<T> {

	/**
	 * Evaluates the given {@link Object} and determines whether the {@link Object} is accepted
	 * based on the filter criteria.
	 *
	 * @param obj {@link Object} to filter.
	 * @return a boolean value indicating whether this {@link Filter} accepts the given {@link Object}
	 * based on the filter criteria.
	 */
	boolean accept(@Nullable T obj);

	/**
	 * Tests whether the given {@link Object} matches the criteria defined by this {@link Filter}.
	 *
	 * @param obj {@link Object} to test.
	 * @return a boolean value indicating whether the given {@link Object} matches the criteria
	 * defined by this {@link Filter}.
	 * @see #accept(Object)
	 */
	@Override
	default boolean test(T obj) {
		return accept(obj);
	}
}
