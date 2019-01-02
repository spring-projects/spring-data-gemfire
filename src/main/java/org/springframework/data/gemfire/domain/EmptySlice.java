/*
 * Copyright 2016-2019 the original author or authors.
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

package org.springframework.data.gemfire.domain;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.domain.support.AbstractSliceSupport;

/**
 * The {@link EmptySlice} class is an implementation of an empty Spring Data {@link Slice}.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the elements in this {@link Slice}.
 * @see org.springframework.data.domain.Pageable
 * @see org.springframework.data.domain.Slice
 * @see org.springframework.data.domain.Sort
 * @see org.springframework.data.gemfire.domain.support.AbstractSliceSupport
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public abstract class EmptySlice<T> extends AbstractSliceSupport<T> {

	@SuppressWarnings("all")
	public static final EmptySlice<Object> EMPTY_SLICE = new EmptySlice<Object>() { };

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean hasNext() {
		return false;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean hasPrevious() {
		return false;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public List<T> getContent() {
		return Collections.emptyList();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public int getNumber() {
		return 1;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Sort getSort() {
		return null;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Pageable nextPageable() {
		return null;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Pageable previousPageable() {
		return null;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <S> Slice<S> map(Function<? super T, ? extends S> converter) {
		return (Slice<S>) EMPTY_SLICE;
	}
}
