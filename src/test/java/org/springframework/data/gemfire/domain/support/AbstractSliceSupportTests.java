/*
 * Copyright 2016-2020 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.domain.Pageable;

/**
 * Unit tests for {@link AbstractSliceSupport}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.domain.support.AbstractSliceSupport
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractSliceSupportTests {

	@Spy
	private AbstractSliceSupport mockSlice;

	@Test
	public void hasContentIsTrue() {
		doReturn(1).when(mockSlice).getNumberOfElements();
		assertThat(mockSlice.hasContent()).isTrue();
		verify(mockSlice, times(1)).getNumberOfElements();
	}

	@Test
	public void hasContentIsFalse() {
		doReturn(0).when(mockSlice).getNumberOfElements();
		assertThat(mockSlice.hasContent()).isFalse();
		verify(mockSlice, times(1)).getNumberOfElements();
	}

	@Test
	public void isFirstReturnsTrueWhenHasPreviousIsFalse() {
		doReturn(false).when(mockSlice).hasPrevious();
		assertThat(mockSlice.isFirst()).isTrue();
		verify(mockSlice, times(1)).hasPrevious();
	}

	@Test
	public void isFirstReturnsFalseWhenHasPreviousIsTrue() {
		doReturn(true).when(mockSlice).hasPrevious();
		assertThat(mockSlice.isFirst()).isFalse();
		verify(mockSlice, times(1)).hasPrevious();
	}

	@Test
	public void isLastReturnsTrueWhenHasNextIsFalse() {
		doReturn(false).when(mockSlice).hasNext();
		assertThat(mockSlice.isLast()).isTrue();
		verify(mockSlice, times(1)).hasNext();
	}

	@Test
	public void isLastReturnFalseWhenHasNextIsTrue() {
		doReturn(true).when(mockSlice).hasNext();
		assertThat(mockSlice.isLast()).isFalse();
		verify(mockSlice, times(1)).hasNext();
	}

	@Test
	public void getNumberReturnsOne() {
		doReturn(null).when(mockSlice).previousPageable();
		assertThat(mockSlice.getNumber()).isEqualTo(1);
		verify(mockSlice, times(1)).previousPageable();
	}

	@Test
	public void getNumberReturnsTwo() {
		Pageable mockPageable = mock(Pageable.class);

		when(mockPageable.previousOrFirst()).thenReturn(mockPageable);
		doReturn(mockPageable).when(mockSlice).previousPageable();

		assertThat(mockSlice.getNumber()).isEqualTo(2);

		verify(mockSlice, times(1)).previousPageable();
		verify(mockPageable, times(1)).previousOrFirst();
	}

	@Test
	public void getNumberReturnsThree() {
		Pageable mockPageableOne = mock(Pageable.class, "Page One");
		Pageable mockPageableTwo = mock(Pageable.class, "Page Two");

		when(mockPageableOne.previousOrFirst()).thenReturn(mockPageableOne);
		when(mockPageableTwo.previousOrFirst()).thenReturn(mockPageableOne);
		doReturn(mockPageableTwo).when(mockSlice).previousPageable();

		assertThat(mockSlice.getNumber()).isEqualTo(3);

		verify(mockSlice, times(1)).previousPageable();
		verify(mockPageableOne, times(1)).previousOrFirst();
		verify(mockPageableTwo, times(1)).previousOrFirst();
	}

	@Test
	public void getNumberOfElementsReturnsTwenty() {
		List<?> mockContent = mock(List.class);

		when(mockContent.size()).thenReturn(20);
		doReturn(mockContent).when(mockSlice).getContent();

		assertThat(mockSlice.getNumberOfElements()).isEqualTo(20);

		verify(mockSlice, times(1)).getContent();
		verify(mockContent, times(1)).size();
	}

	@Test
	public void getSizeCallsGetNumberOfElements() {
		doReturn(18).when(mockSlice).getNumberOfElements();
		assertThat(mockSlice.getSize()).isEqualTo(18);
		verify(mockSlice, times(1)).getNumberOfElements();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void iteratorWithContent() {
		doReturn(Arrays.asList(1, 2, 3)).when(mockSlice).getContent();
		assertThat((Iterable) () -> mockSlice.iterator()).contains(1, 2, 3);
		verify(mockSlice, atLeastOnce()).getContent();
	}
}
