/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.snapshot.filter;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.CoreMatchers.isA;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.geode.cache.snapshot.SnapshotFilter;
import org.junit.Test;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.snapshot.filter.ComposableSnapshotFilter.Operator;

/**
 * The ComposableSnapshotFilterTest class is a test suite of test cases testing the contract and functionality
 * of the ComposableSnapshotFilter class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.snapshot.filter.ComposableSnapshotFilter
 * @see org.springframework.data.gemfire.snapshot.filter.ComposableSnapshotFilter.Operator
 * @see org.apache.geode.cache.snapshot.SnapshotFilter
 * @since 1.7.0
 */
public class ComposableSnapshotFilterTest {

	private static final AtomicInteger ID_SEQUENCE = new AtomicInteger(0);

	@SuppressWarnings("unchecked")
	protected SnapshotFilter mockSnapshotFilter(boolean accept) {
		SnapshotFilter mockSnapshotFilter = mock(SnapshotFilter.class, String.format(
			"MockSnapshotFilter-%1$d", ID_SEQUENCE.incrementAndGet()));

		when(mockSnapshotFilter.accept((Map.Entry) any())).thenReturn(accept);

		return mockSnapshotFilter;
	}

	@Test
	public void operatorIdentityIsSuccessful() {
		assertThat(Operator.AND.isAnd(), is(true));
		assertThat(Operator.AND.isOr(), is(false));
		assertThat(Operator.OR.isAnd(), is(false));
		assertThat(Operator.OR.isOr(), is(true));
	}

	@Test
	public void andOperatorOperationIsValid() {
		assertThat(Operator.AND.operate(true, true), is(true));
		assertThat(Operator.AND.operate(true, false), is(false));
		assertThat(Operator.AND.operate(false, true), is(false));
		assertThat(Operator.AND.operate(false, false), is(false));
	}

	@Test
	public void orOperatorOperationIsValid() {
		assertThat(Operator.OR.operate(true, true), is(true));
		assertThat(Operator.OR.operate(true, false), is(true));
		assertThat(Operator.OR.operate(false, true), is(true));
		assertThat(Operator.OR.operate(false, false), is(false));
	}

	@Test
	public void nullSafeArrayWithNonNullArray() {
		SnapshotFilter[] expectedArray = {};

		assertThat(ComposableSnapshotFilter.nullSafeArray(expectedArray), is(sameInstance(expectedArray)));
	}

	@Test
	public void nullSafeArrayWithNullArray() {
		SnapshotFilter[] actualArray = ComposableSnapshotFilter.nullSafeArray((SnapshotFilter[]) null);

		assertThat(actualArray, is(notNullValue()));
		assertThat(actualArray.length, is(equalTo(0)));
	}

	@Test
	public void composeSingle() {
		SnapshotFilter mockSnapshotFilter = mockSnapshotFilter(false);
		SnapshotFilter composedFilter = ComposableSnapshotFilter.compose(Operator.AND, mockSnapshotFilter);

		assertThat(composedFilter, is(sameInstance(mockSnapshotFilter)));
	}

	@Test
	public void composeMultiple() throws Exception {
		SnapshotFilter mockSnapshotFilterOne = mockSnapshotFilter(false);
		SnapshotFilter mockSnapshotFilterTwo = mockSnapshotFilter(true);

		SnapshotFilter composedFilter = ComposableSnapshotFilter.compose(Operator.AND, mockSnapshotFilterOne,
			mockSnapshotFilterTwo);

		assertThat(composedFilter, is(not(sameInstance(mockSnapshotFilterOne))));
		assertThat(composedFilter, is(not(sameInstance(mockSnapshotFilterTwo))));
		assertThat((ComposableSnapshotFilter) composedFilter, isA(ComposableSnapshotFilter.class));
		assertThat((SnapshotFilter) TestUtils.readField("leftOperand", composedFilter),
			is(equalTo(mockSnapshotFilterTwo)));
		assertThat((Operator) TestUtils.readField("operator", composedFilter), is(equalTo(Operator.AND)));
		assertThat((SnapshotFilter) TestUtils.readField("rightOperand", composedFilter),
			is(equalTo(mockSnapshotFilterOne)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void composeAndThenAccept() {
		SnapshotFilter falseFilter = mockSnapshotFilter(false);
		SnapshotFilter trueFilter = mockSnapshotFilter(true);

		SnapshotFilter composedFilter = ComposableSnapshotFilter.and(trueFilter, trueFilter);

		assertThat((ComposableSnapshotFilter) composedFilter, isA(ComposableSnapshotFilter.class));
		assertThat(composedFilter.accept(null), is(true));

		composedFilter = ComposableSnapshotFilter.and(falseFilter, trueFilter);

		assertThat((ComposableSnapshotFilter) composedFilter, isA(ComposableSnapshotFilter.class));
		assertThat(composedFilter.accept(null), is(false));

		composedFilter = ComposableSnapshotFilter.and(falseFilter, falseFilter);

		assertThat((ComposableSnapshotFilter) composedFilter, isA(ComposableSnapshotFilter.class));
		assertThat(composedFilter.accept(null), is(false));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void composeOrThenAccept() {
		SnapshotFilter falseFilter = mockSnapshotFilter(false);
		SnapshotFilter trueFilter = mockSnapshotFilter(true);

		SnapshotFilter composedFilter = ComposableSnapshotFilter.or(trueFilter, trueFilter);

		assertThat((ComposableSnapshotFilter) composedFilter, isA(ComposableSnapshotFilter.class));
		assertThat(composedFilter.accept(null), is(true));

		composedFilter = ComposableSnapshotFilter.or(falseFilter, trueFilter);

		assertThat((ComposableSnapshotFilter) composedFilter, isA(ComposableSnapshotFilter.class));
		assertThat(composedFilter.accept(null), is(true));

		composedFilter = ComposableSnapshotFilter.or(falseFilter, falseFilter);

		assertThat((ComposableSnapshotFilter) composedFilter, isA(ComposableSnapshotFilter.class));
		assertThat(composedFilter.accept(null), is(false));
	}

}
