/*
 * Copyright 2016-2018 the original author or authors.
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

package org.springframework.data.gemfire.domain.support;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newUnsupportedOperationException;

import java.util.function.Function;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Slice;
import org.springframework.data.gemfire.util.RuntimeExceptionFactory;

/**
 * The {@link AbstractPageSupport} class is an abstract Spring Data {@link Page} type supporting the implementation of
 * application specific {@link Page} implementations.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the individual elements on this {@link Slice}.
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.domain.Page
 * @see org.springframework.data.domain.Slice
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public abstract class AbstractPageSupport<T> extends AbstractSliceSupport<T> implements Page<T> {

	/**
	 * @inheritDoc
	 */
	@Override
	public long getTotalElements() {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public int getTotalPages() {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <S> Page<S> map(Function<? super T, ? extends S> converter) {
		throw newUnsupportedOperationException(RuntimeExceptionFactory.NOT_IMPLEMENTED);
	}
}
