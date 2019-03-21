/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.domain;

import java.util.function.Function;

import org.springframework.data.domain.Page;

/**
 * The {@link EmptyPage} class is an implementation of an empty Spring Data {@link Page}.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the elements in this {@link Page}.
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.domain.Page
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public final class EmptyPage<T> extends EmptySlice<T> implements Page<T> {

	@SuppressWarnings("all")
	public static final EmptyPage<?> EMPTY_PAGE = new EmptyPage<>();

	/**
	 * @inheritDoc
	 */
	@Override
	public int getTotalPages() {
		return 1;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public long getTotalElements() {
		return 0;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <S> Page<S> map(Function<? super T, ? extends S> converter) {
		return (Page<S>) EMPTY_PAGE;
	}
}
