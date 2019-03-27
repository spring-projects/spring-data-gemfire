/*
 * Copyright 2018-2019 the original author or authors.
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
 */

package org.springframework.data.gemfire.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;

/**
 * Unit tests for {@link Filter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.util.Filter
 * @since 1.0.0
 */
public class FilterUnitTests {

	@Test
	@SuppressWarnings("unchecked")
	public void andIsCorrect() {

		Filter mockFilterOne = mock(AbstractFilter.class);
		Filter mockFilterTwo = mock(AbstractFilter.class);
		Filter mockFilterThree = mock(AbstractFilter.class);
		Filter mockFilterFour = mock(AbstractFilter.class);

		when(mockFilterOne.accept(any())).thenReturn(false);
		when(mockFilterOne.and(any(Filter.class))).thenCallRealMethod();
		when(mockFilterTwo.accept(any())).thenReturn(true);
		when(mockFilterTwo.and(any(Filter.class))).thenCallRealMethod();
		when(mockFilterThree.accept(any())).thenReturn(false);
		when(mockFilterThree.and(any(Filter.class))).thenCallRealMethod();
		when(mockFilterFour.accept(any())).thenReturn(true);
		when(mockFilterFour.and(any(Filter.class))).thenCallRealMethod();

		assertThat(mockFilterOne.and(mockFilterTwo).accept("test")).isFalse();
		assertThat(mockFilterOne.and(mockFilterThree).accept("test")).isFalse();
		assertThat(mockFilterTwo.and(mockFilterThree).accept("test")).isFalse();
		assertThat(mockFilterTwo.and(mockFilterFour).accept("test")).isTrue();

		verify(mockFilterOne, times(2)).accept(eq("test"));
		verify(mockFilterTwo, times(2)).accept(eq("test"));
		verify(mockFilterThree, times(1)).accept(eq("test"));
		verify(mockFilterFour, times(1)).accept(eq("test"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void negateReturnsFalseForTrue() {

		Filter<Object> mockFilter = mock(AbstractFilter.class);

		when(mockFilter.accept(any())).thenReturn(true);
		when(mockFilter.negate()).thenCallRealMethod();

		assertThat(mockFilter.negate().accept("test")).isFalse();

		verify(mockFilter, times(1)).accept(eq("test"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void negateReturnsTrueForFalse() {

		Filter<Object> mockFilter = mock(AbstractFilter.class);

		when(mockFilter.accept(any())).thenReturn(false);
		when(mockFilter.negate()).thenCallRealMethod();

		assertThat(mockFilter.negate().accept("test")).isTrue();

		verify(mockFilter, times(1)).accept(eq("test"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void orIsCorrect() {

		Filter mockFilterOne = mock(AbstractFilter.class);
		Filter mockFilterTwo = mock(AbstractFilter.class);
		Filter mockFilterThree = mock(AbstractFilter.class);
		Filter mockFilterFour = mock(AbstractFilter.class);

		when(mockFilterOne.accept(any())).thenReturn(false);
		when(mockFilterOne.or(any(Filter.class))).thenCallRealMethod();
		when(mockFilterTwo.accept(any())).thenReturn(true);
		when(mockFilterTwo.or(any(Filter.class))).thenCallRealMethod();
		when(mockFilterThree.accept(any())).thenReturn(false);
		when(mockFilterThree.or(any(Filter.class))).thenCallRealMethod();
		when(mockFilterFour.accept(any())).thenReturn(true);
		when(mockFilterFour.or(any(Filter.class))).thenCallRealMethod();

		assertThat(mockFilterOne.or(mockFilterTwo).accept("test")).isTrue();
		assertThat(mockFilterOne.or(mockFilterThree).accept("test")).isFalse();
		assertThat(mockFilterTwo.or(mockFilterThree).accept("test")).isTrue();
		assertThat(mockFilterTwo.or(mockFilterFour).accept("test")).isTrue();

		verify(mockFilterOne, times(2)).accept(eq("test"));
		verify(mockFilterTwo, times(3)).accept(eq("test"));
		verify(mockFilterThree, times(1)).accept(eq("test"));
		verify(mockFilterFour, never()).accept(eq("test"));
	}
}
