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

package org.springframework.data.gemfire.domain.support;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newUnsupportedOperationException;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.util.RuntimeExceptionFactory;

/**
 * The {@link AbstractSliceSupport} class is an abstract Spring Data {@link Slice} type
 * supporting the implementation of application specific {@link Slice} implementations.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the individual elements on this {@link Slice}.
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.domain.Page
 * @see org.springframework.data.domain.Pageable
 * @see org.springframework.data.domain.Slice
 * @see org.springframework.data.domain.Sort
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public abstract class AbstractSliceSupport<T> implements Slice<T> {

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean hasContent() {
		return (getNumberOfElements() > 0);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean hasNext() {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean hasPrevious() {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean isFirst() {
		return !hasPrevious();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean isLast() {
		return !hasNext();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public List<T> getContent() {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public int getNumber() {
		AtomicInteger number = new AtomicInteger(1);

		Optional.ofNullable(previousPageable()).ifPresent(previousPageable -> {
			Pageable currentPageable;

			do {
				number.incrementAndGet();
				currentPageable = previousPageable;
				previousPageable = previousPageable.previousOrFirst();
			}
			while (currentPageable != previousPageable);
		});

		return number.get();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public int getNumberOfElements() {
		return getContent().size();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public int getSize() {
		return getNumberOfElements();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Sort getSort() {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Iterator<T> iterator() {
		return Collections.unmodifiableList(Optional.ofNullable(getContent())
			.orElseGet(Collections::emptyList)).iterator();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <S> Slice<S> map(Function<? super T, ? extends S> converter) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Pageable nextPageable() {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Pageable previousPageable() {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}
}
