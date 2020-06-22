/*
 * Copyright 2020 the original author or authors.
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
package org.springframework.data.gemfire.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;

import java.io.IOException;

import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mockito;

import org.springframework.core.type.classreading.MetadataReader;
import org.springframework.core.type.classreading.MetadataReaderFactory;
import org.springframework.core.type.filter.TypeFilter;

/**
 * Unit Tests for {@link CompositeTypeFilter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.core.type.filter.TypeFilter
 * @see org.springframework.data.gemfire.support.CompositeTypeFilter
 * @since 2.4.0
 */
public class CompositeTypeFilterUnitTests {

	@Test
	public void allowTypeFilterMatchReturnsTrue() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		assertThat(CompositeTypeFilter.ALLOW.match(mockMetadataReader, mockMetadataReaderFactory)).isTrue();
	}

	@Test
	public void denyTypeFilterMatchReturnsFalse() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		assertThat(CompositeTypeFilter.DENY.match(mockMetadataReader, mockMetadataReaderFactory)).isFalse();
	}

	@Test
	public void ofWillNotWrapExistingCompositeTypeFilter() {

		CompositeTypeFilter mockCompositeTypeFilter = mock(CompositeTypeFilter.class);

		assertThat(CompositeTypeFilter.of(mockCompositeTypeFilter)).isSameAs(mockCompositeTypeFilter);
	}

	@Test
	public void ofWrapsExistingTypeFilter() {

		TypeFilter mockTypeFilter = mock(TypeFilter.class);

		CompositeTypeFilter compositeTypeFilter = CompositeTypeFilter.of(mockTypeFilter);

		assertThat(compositeTypeFilter).isNotNull();
	}

	@Test(expected = IllegalArgumentException.class)
	public void ofWithNullTypeFilterThrowsIllegalArgumentException() {

		try {
			CompositeTypeFilter.of(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessageContaining("TypeFilter to wrap must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void andThenComposesTrueTrueTypeFilters() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);

		doReturn(true).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(true).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		assertThat(CompositeTypeFilter.of(mockTypeFilterOne)
			.andThen(mockTypeFilterTwo)
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isTrue();

		InOrder inOrder = Mockito.inOrder(mockTypeFilterOne, mockTypeFilterTwo);

		inOrder.verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		inOrder.verify(mockTypeFilterTwo, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void andThenComposesTrueFalseTypeFilters() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);

		doReturn(true).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(false).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		assertThat(CompositeTypeFilter.of(mockTypeFilterOne)
			.andThen(mockTypeFilterTwo)
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isFalse();

		InOrder inOrder = Mockito.inOrder(mockTypeFilterOne, mockTypeFilterTwo);

		inOrder.verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		inOrder.verify(mockTypeFilterTwo, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void andThenComposesFalseTrueTypeFilters() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);

		doReturn(false).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(true).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		assertThat(CompositeTypeFilter.of(mockTypeFilterOne)
			.andThen(mockTypeFilterTwo)
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isFalse();

		InOrder inOrder = Mockito.inOrder(mockTypeFilterOne, mockTypeFilterTwo);

		inOrder.verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		inOrder.verify(mockTypeFilterTwo, never()).match(any(), any());

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void andThenComposesFalseFalseTypeFilters() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);

		doReturn(false).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(false).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		assertThat(CompositeTypeFilter.of(mockTypeFilterOne)
			.andThen(mockTypeFilterTwo)
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isFalse();

		InOrder inOrder = Mockito.inOrder(mockTypeFilterOne, mockTypeFilterTwo);

		inOrder.verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		inOrder.verify(mockTypeFilterTwo, never()).match(any(), any());

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void negateTrueIsCorrect() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilter = mock(TypeFilter.class);

		doReturn(true).when(mockTypeFilter).match(any(), any());

		assertThat(CompositeTypeFilter.of(mockTypeFilter)
			.negate()
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isFalse();

		verify(mockTypeFilter, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verifyNoMoreInteractions(mockTypeFilter);
	}

	@Test
	public void negateFalseIsCorrect() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilter = mock(TypeFilter.class);

		doReturn(false).when(mockTypeFilter).match(any(), any());

		assertThat(CompositeTypeFilter.of(mockTypeFilter)
			.negate()
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isTrue();

		verify(mockTypeFilter, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verifyNoMoreInteractions(mockTypeFilter);
	}

	@Test
	public void orThenComposesTrueTrueTypeFilters() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);

		doReturn(true).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(true).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		assertThat(CompositeTypeFilter.of(mockTypeFilterOne)
			.orThen(mockTypeFilterTwo)
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isTrue();

		InOrder inOrder = Mockito.inOrder(mockTypeFilterOne, mockTypeFilterTwo);

		inOrder.verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		inOrder.verify(mockTypeFilterTwo, never()).match(any(), any());

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void orThenComposesTrueFalseFilters() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);

		doReturn(true).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(false).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		assertThat(CompositeTypeFilter.of(mockTypeFilterOne)
			.orThen(mockTypeFilterTwo)
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isTrue();

		InOrder inOrder = Mockito.inOrder(mockTypeFilterOne, mockTypeFilterTwo);

		inOrder.verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		inOrder.verify(mockTypeFilterTwo, never()).match(any(), any());

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void orThenComposesFalseTrueFilters() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);

		doReturn(false).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(true).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		assertThat(CompositeTypeFilter.of(mockTypeFilterOne)
			.orThen(mockTypeFilterTwo)
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isTrue();

		InOrder inOrder = Mockito.inOrder(mockTypeFilterOne, mockTypeFilterTwo);

		inOrder.verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		inOrder.verify(mockTypeFilterTwo, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void orThenComposesFalseFalseFilters() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);

		doReturn(false).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(false).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		assertThat(CompositeTypeFilter.of(mockTypeFilterOne)
			.orThen(mockTypeFilterTwo)
			.match(mockMetadataReader, mockMetadataReaderFactory))
			.isFalse();

		InOrder inOrder = Mockito.inOrder(mockTypeFilterOne, mockTypeFilterTwo);

		inOrder.verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		inOrder.verify(mockTypeFilterTwo, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void composeAndWithTypeFiltersCallMatchReturnsTrue() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);
		TypeFilter mockTypeFilterThree = mock(TypeFilter.class);

		doReturn(true).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(true).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(true).when(mockTypeFilterThree)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		TypeFilter typeFilter = CompositeTypeFilter.composeAnd(null, mockTypeFilterOne, mockTypeFilterTwo,
			null, null, mockTypeFilterThree);

		assertThat(typeFilter).isNotNull();
		assertThat(typeFilter.match(mockMetadataReader, mockMetadataReaderFactory)).isTrue();

		verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verify(mockTypeFilterTwo, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verify(mockTypeFilterThree, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void composeAndWithTypeFiltersCallMatchReturnsFalse() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);
		TypeFilter mockTypeFilterThree = mock(TypeFilter.class);

		doReturn(true).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(false).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(true).when(mockTypeFilterThree)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		TypeFilter typeFilter =
			CompositeTypeFilter.composeAnd(mockTypeFilterOne, mockTypeFilterTwo, mockTypeFilterThree);

		assertThat(typeFilter).isNotNull();
		assertThat(typeFilter.match(mockMetadataReader, mockMetadataReaderFactory)).isFalse();

		verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verify(mockTypeFilterTwo, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verify(mockTypeFilterThree, never()).match(any(), any());

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void composeAndWithEmptyTypeFiltersReturnsNull() {
		assertThat(CompositeTypeFilter.composeAnd()).isNull();
	}

	@Test
	public void composeAndWithNullTypeFiltersIsNullSafeReturnsNull() {
		assertThat(CompositeTypeFilter.composeAnd((TypeFilter[]) null)).isNull();
	}

	@Test
	public void composeOrWithTypeFiltersCallMatchReturnsTrue() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);
		TypeFilter mockTypeFilterThree = mock(TypeFilter.class);

		doReturn(false).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(true).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(false).when(mockTypeFilterThree)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		TypeFilter typeFilter =
			CompositeTypeFilter.composeOr(mockTypeFilterOne, null, mockTypeFilterTwo, mockTypeFilterThree, null);

		assertThat(typeFilter).isNotNull();
		assertThat(typeFilter.match(mockMetadataReader, mockMetadataReaderFactory)).isTrue();

		verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verify(mockTypeFilterTwo, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verify(mockTypeFilterThree, never()).match(any(), any());

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void composeOrWithTypeFiltersCallMatchReturnsFalse() throws IOException {

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		MetadataReaderFactory mockMetadataReaderFactory = mock(MetadataReaderFactory.class);

		TypeFilter mockTypeFilterOne = mock(TypeFilter.class);
		TypeFilter mockTypeFilterTwo = mock(TypeFilter.class);
		TypeFilter mockTypeFilterThree = mock(TypeFilter.class);

		doReturn(false).when(mockTypeFilterOne)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(false).when(mockTypeFilterTwo)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));
		doReturn(false).when(mockTypeFilterThree)
			.match(any(MetadataReader.class), any(MetadataReaderFactory.class));

		TypeFilter typeFilter =
			CompositeTypeFilter.composeOr(mockTypeFilterOne, mockTypeFilterTwo, mockTypeFilterThree);

		assertThat(typeFilter).isNotNull();
		assertThat(typeFilter.match(mockMetadataReader, mockMetadataReaderFactory)).isFalse();

		verify(mockTypeFilterOne, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verify(mockTypeFilterTwo, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));
		verify(mockTypeFilterThree, times(1))
			.match(eq(mockMetadataReader), eq(mockMetadataReaderFactory));

		verifyZeroInteractions(mockMetadataReader, mockMetadataReaderFactory);
	}

	@Test
	public void composeOrWithEmptyTypeFiltersReturnsNull() {
		assertThat(CompositeTypeFilter.composeOr()).isNull();
	}

	@Test
	public void composeOrWithNullTypeFiltersIsNullSafeReturnsNull() {
		assertThat(CompositeTypeFilter.composeOr((TypeFilter[]) null)).isNull();
	}
}
