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

package org.springframework.data.gemfire.search.lucene.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.search.lucene.support.LucenePage.newLucenePage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import org.apache.geode.cache.lucene.LuceneResultStruct;
import org.apache.geode.cache.lucene.PageableLuceneQueryResults;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.data.domain.Page;
import org.springframework.data.gemfire.search.lucene.ProjectingLuceneAccessor;

/**
 * Unit tests for {@link LucenePage}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.search.lucene.ProjectingLuceneAccessor
 * @see org.springframework.data.gemfire.search.lucene.support.LucenePage
 * @see org.apache.geode.cache.lucene.LuceneResultStruct
 * @see org.apache.geode.cache.lucene.PageableLuceneQueryResults
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class LucenePageUnitTests {

	@Mock
	private PageableLuceneQueryResults<Long, String> mockQueryResults;

	@Mock
	private ProjectingLuceneAccessor mockTemplate;

	@SuppressWarnings("unchecked")
	protected <K, V> LuceneResultStruct<K, V> mockLuceneResultStruct(K key, V value) {
		LuceneResultStruct<K, V> mockLuceneResultStruct = mock(LuceneResultStruct.class,
			String.format("MockLuceneResultStruct$%1$s", key));

		when(mockLuceneResultStruct.getValue()).thenReturn(value);

		return mockLuceneResultStruct;
	}

	protected List<LuceneResultStruct<Long, String>> mockLuceneResultStructList(List<Person> people) {
		AtomicLong id = new AtomicLong(0L);

		return people.stream().map(person -> mockLuceneResultStruct(id.incrementAndGet(), person.getName()))
			.collect(Collectors.toList());
	}

	protected List<LuceneResultStruct<Long, String>> prepare(PageableLuceneQueryResults<Long, String> mockQueryResults,
			Person... results) {

		return prepare(mockQueryResults, Arrays.asList(results), results.length).get(0);
	}

	protected List<LuceneResultStruct<Long, String>> prepare(PageableLuceneQueryResults<Long, String> mockQueryResults,
			Iterable<Person> results) {

		return prepare(mockQueryResults, results, size(results)).get(0);
	}

	protected List<List<LuceneResultStruct<Long, String>>> prepare(
			PageableLuceneQueryResults<Long, String> mockQueryResults, Iterable<Person> results, int pageSize) {

		List<List<Person>> pages = paginate(results, pageSize);

		List<List<LuceneResultStruct<Long, String>>> resultStructs =
			pages.stream().map(this::mockLuceneResultStructList).collect(Collectors.toList());

		Iterator iterator = resultStructs.iterator();

		when(mockQueryResults.hasNext()).thenAnswer(invocation -> iterator.hasNext());
		when(mockQueryResults.next()).thenAnswer(invocation -> iterator.next());

		return resultStructs;
	}

	private List<List<Person>> paginate(Iterable<Person> people, int pageSize) {
		List<List<Person>> pages = new ArrayList<>();
		List<Person> page = new ArrayList<>(pageSize);

		for (Person person : people) {
			if (page.size() == pageSize) {
				pages.add(page);
				page = new ArrayList<>(pageSize);
			}

			page.add(person);
		}

		pages.add(page);

		return pages;
	}

	private int size(Iterable<?> iterable) {
		int size = 0;

		for (Object element : iterable) {
			size++;
		}

		return size;
	}

	@SuppressWarnings("unchecked")
	protected ProjectingLuceneAccessor prepare(ProjectingLuceneAccessor mockTemplate) {
		when(mockTemplate.project(isA(List.class), eq(Person.class))).thenAnswer(invocation -> {
			List<LuceneResultStruct<Long, String>> results = invocation.getArgument(0);
			assertThat(invocation.<Class>getArgument(1)).isEqualTo(Person.class);
			return results.stream().map(result -> Person.parse(result.getValue())).collect(Collectors.toList());
		});

		return mockTemplate;
	}

	@Test
	@SuppressWarnings("unchecked")
	public void newLucenePageWithNoPreviousPageIsMaterialized() {
		List<Person> people = Collections.singletonList(Person.newPerson("Jon", "Doe"));

		List<LuceneResultStruct<Long, String>> mockResultStructList = prepare(mockQueryResults, people);

		LucenePage<Person, Long, String> page =
			newLucenePage(prepare(mockTemplate), mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getContent()).isEqualTo(people);
		assertThat(page.getPageSize()).isEqualTo(20);
		assertThat(page.getPrevious()).isNull();
		assertThat(page.getProjectionType()).isEqualTo(Person.class);
		assertThat(page.getQueryResults()).isSameAs(mockQueryResults);
		assertThat(page.getTemplate()).isSameAs(mockTemplate);
		assertThat(page.hasNext()).isFalse();
		assertThat(page.hasPrevious()).isFalse();

		verify(mockQueryResults, times(2)).hasNext();
		verify(mockQueryResults, times(1)).next();
		verifyNoMoreInteractions(mockQueryResults);
		verify(mockTemplate, times(1)).project(eq(mockResultStructList), eq(Person.class));
		verifyNoMoreInteractions(mockTemplate);
	}

	@Test
	public void newLucenePageWithPreviousPageIsMaterialized() {
		List<Person> expectedContent = Arrays.asList(Person.newPerson("Jon", "Doe"), Person.newPerson("Jane", "Doe"));

		List<List<LuceneResultStruct<Long, String>>> mockResults =
			prepare(mockQueryResults, expectedContent, 1);

		LucenePage<Person, Long, String> firstPage =
			newLucenePage(prepare(mockTemplate), mockQueryResults, 1, Person.class);

		LucenePage<Person, Long, String> secondPage =
			newLucenePage(mockTemplate, mockQueryResults, 1, Person.class, firstPage);

		assertThat(secondPage).isNotNull();
		assertThat(secondPage.getContent()).isEqualTo(Collections.singletonList(expectedContent.get(1)));
		assertThat(secondPage.getPageSize()).isEqualTo(1);
		assertThat(secondPage.getPrevious()).isEqualTo(firstPage);
		assertThat(secondPage.getProjectionType()).isEqualTo(Person.class);
		assertThat(secondPage.getQueryResults()).isSameAs(mockQueryResults);
		assertThat(secondPage.getTemplate()).isSameAs(mockTemplate);
		assertThat(secondPage.hasNext()).isFalse();
		assertThat(secondPage.hasPrevious()).isTrue();

		verify(mockQueryResults, times(3)).hasNext();
		verify(mockQueryResults, times(2)).next();
		verifyNoMoreInteractions(mockQueryResults);
		verify(mockTemplate, times(1))
			.project(eq(mockResults.get(0)), eq(Person.class));
		verify(mockTemplate, times(1))
			.project(eq(mockResults.get(1)), eq(Person.class));
		verifyNoMoreInteractions(mockTemplate);
	}

	@Test(expected = IllegalArgumentException.class)
	public void newLucenePageWithNullTemplateThrowsIllegalArgumentException() {
		try {
			newLucenePage(null, mockQueryResults, 10, Person.class);
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("ProjectingLuceneAccessor must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verifyZeroInteractions(mockQueryResults);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void newLucenePageWithNullQueryResultsThrowsIllegalArgumentException() {
		try {
			newLucenePage(mockTemplate, null, 10, Person.class);
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("PageableLuceneQueryResults must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verifyZeroInteractions(mockTemplate);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void newLucenePageWithNoContentThrowsIllegalArgumentException() {
		try {
			when(mockQueryResults.hasNext()).thenReturn(false);
			newLucenePage(mockTemplate, mockQueryResults, 10, Person.class);
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("PageableLuceneQueryResults must have content");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockQueryResults, times(1)).hasNext();
			verifyZeroInteractions(mockTemplate);
		}
	}

	@Test
	public void getNextReturnsNextPage() {
		List<Person> expectedContent = Arrays.asList(Person.newPerson("Jon", "Doe"), Person.newPerson("Jane", "Doe"));

		List<List<LuceneResultStruct<Long, String>>> mockResults =
			prepare(mockQueryResults, expectedContent, 1);

		LucenePage<Person, Long, String> firstPage =
			newLucenePage(prepare(mockTemplate), mockQueryResults, 20, Person.class);

		assertThat(firstPage).isNotNull();
		assertThat(firstPage.getContent()).isEqualTo(Collections.singletonList(expectedContent.get(0)));

		LucenePage<Person, Long, String> nextPage = firstPage.getNext();

		assertThat(nextPage).isNotNull();
		assertThat(nextPage.getContent()).isEqualTo(Collections.singletonList(expectedContent.get(1)));

		verify(mockQueryResults, times(3)).hasNext();
		verify(mockQueryResults, times(2)).next();
		verifyNoMoreInteractions(mockQueryResults);
		verify(mockTemplate, times(1)).project(eq(mockResults.get(0)), eq(Person.class));
		verify(mockTemplate, times(1)).project(eq(mockResults.get(1)), eq(Person.class));
		verifyNoMoreInteractions(mockQueryResults);
	}

	@SuppressWarnings("unchecked")
	@Test(expected = IllegalStateException.class)
	public void getNextThrowsIllegalStateExceptionWhenNoMorePages() {
		try {
			prepare(mockQueryResults, Person.newPerson("Jon", "Doe"));

			LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

			assertThat(page).isNotNull();
			assertThat(page.getPrevious()).isNull();

			page.getNext();
		}
		catch (IllegalStateException expected) {
			assertThat(expected).hasMessage("No more pages");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockQueryResults, times(2)).hasNext();
			verify(mockQueryResults, times(1)).next();
			verifyNoMoreInteractions(mockQueryResults);
			verify(mockTemplate, times(1)).project(isA(List.class), eq(Person.class));
			verifyNoMoreInteractions(mockTemplate);
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getNumberReturnsOne() {
		prepare(mockQueryResults, Person.newPerson("Jon", "Doe"));

		LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getPrevious()).isNull();
		assertThat(page.getNumber()).isEqualTo(1);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getNumberReturnsTwo() {
		prepare(mockQueryResults, Arrays.asList(Person.newPerson("Jon", "Doe"), Person.newPerson("Jane", "Doe")),
			1);

		LucenePage<Person, Long, String> previousPage =
			newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		LucenePage<Person, Long, String> page =
			newLucenePage(mockTemplate, mockQueryResults, 20, Person.class, previousPage);

		assertThat(page).isNotNull();
		assertThat(page.getPrevious()).isEqualTo(previousPage);
		assertThat(page.getNumber()).isEqualTo(2);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getNumberReturnsThree() {
		prepare(mockQueryResults, Arrays.asList(Person.newPerson("Jon", "Doe"), Person.newPerson("Jane", "Doe"),
			Person.newPerson("Pie", "Doe")), 1);

		LucenePage<Person, Long, String> firstPage = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		LucenePage<Person, Long, String> secondPage =
			newLucenePage(mockTemplate, mockQueryResults, 20, Person.class, firstPage);

		LucenePage<Person, Long, String> thirdPage =
			newLucenePage(mockTemplate, mockQueryResults, 20, Person.class, secondPage);

		assertThat(thirdPage).isNotNull();
		assertThat(thirdPage.getPrevious()).isEqualTo(secondPage);
		assertThat(secondPage.getPrevious()).isEqualTo(firstPage);
		assertThat(firstPage.getPrevious()).isNull();
		assertThat(thirdPage.getNumber()).isEqualTo(3);
	}

	@Test
	public void getSizeWithNoContentEqualsPageSize() {
		when(mockQueryResults.hasNext()).thenReturn(true);
		when(mockQueryResults.next()).thenReturn(Collections.emptyList());

		LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getNumberOfElements()).isEqualTo(0);
		assertThat(page.getPageSize()).isEqualTo(20);
		assertThat(page.getSize()).isEqualTo(20);
	}

	@Test
	public void getSizeWithSingleElementEqualsPageSize() {
		prepare(mockQueryResults, Person.newPerson("Jon", "Doe"));

		LucenePage<Person, Long, String> page =
			newLucenePage(prepare(mockTemplate), mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getNumberOfElements()).isEqualTo(1);
		assertThat(page.getPageSize()).isEqualTo(20);
		assertThat(page.getSize()).isEqualTo(20);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getSizeWithMultipleElementsEqualsPageSize() {
		List<Person> mockList = mock(List.class);

		when(mockList.size()).thenReturn(101);
		when(mockQueryResults.hasNext()).thenReturn(true);
		when(mockQueryResults.next()).thenReturn(Collections.emptyList());
		when(mockTemplate.project(isA(List.class), eq(Person.class))).thenReturn(mockList);

		LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getNumberOfElements()).isEqualTo(101);
		assertThat(page.getPageSize()).isEqualTo(20);
		assertThat(page.getSize()).isEqualTo(20);

		verify(mockList, times(1)).size();
	}

	@Test
	public void totalElementsEqualsLuceneQueryResultsSize() {
		when(mockQueryResults.hasNext()).thenReturn(true);
		when(mockQueryResults.next()).thenReturn(Collections.emptyList());
		when(mockQueryResults.size()).thenReturn(409);

		LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getTotalElements()).isEqualTo(409);

		verify(mockQueryResults, times(1)).size();
	}

	@Test
	public void totalPagesIsOneWhenTotalElementsIsLessThanPageSize() {
		when(mockQueryResults.hasNext()).thenReturn(true);
		when(mockQueryResults.next()).thenReturn(Collections.emptyList());
		when(mockQueryResults.size()).thenReturn(9);

		LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getTotalPages()).isEqualTo(1);

		verify(mockQueryResults, times(1)).size();
	}

	@Test
	public void totalPagesIsOneWhenTotalElementsEqualsPageSize() {
		when(mockQueryResults.hasNext()).thenReturn(true);
		when(mockQueryResults.next()).thenReturn(Collections.emptyList());
		when(mockQueryResults.size()).thenReturn(20);

		LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getTotalPages()).isEqualTo(1);

		verify(mockQueryResults, times(1)).size();
	}

	@Test
	public void totalPagesIsTwoWhenTotalElementsIsGreaterThanPageSize() {
		when(mockQueryResults.hasNext()).thenReturn(true);
		when(mockQueryResults.next()).thenReturn(Collections.emptyList());
		when(mockQueryResults.size()).thenReturn(31);

		LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getTotalPages()).isEqualTo(2);

		verify(mockQueryResults, times(1)).size();
	}

	@Test
	public void totalPagesIsFiveWhenTotalElementsIsGreaterThanEqualToPageSize() {
		when(mockQueryResults.hasNext()).thenReturn(true);
		when(mockQueryResults.next()).thenReturn(Collections.emptyList());
		when(mockQueryResults.size()).thenReturn(100);

		LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getTotalPages()).isEqualTo(5);

		verify(mockQueryResults, times(1)).size();
	}

	@Test
	public void totalPagesIsSixWhenTotalElementsIsGreaterThanPageSize() {
		when(mockQueryResults.hasNext()).thenReturn(true);
		when(mockQueryResults.next()).thenReturn(Collections.emptyList());
		when(mockQueryResults.size()).thenReturn(101);

		LucenePage<Person, Long, String> page = newLucenePage(mockTemplate, mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getTotalPages()).isEqualTo(6);

		verify(mockQueryResults, times(1)).size();
	}

	@Test
	public void mapIsSuccessful() {
		List<Person> expectedContent = Arrays.asList(Person.newPerson("Jon", "Doe"), Person.newPerson("Jane", "Doe"));

		prepare(mockQueryResults, expectedContent);

		LucenePage<Person, Long, String> page =
			newLucenePage(prepare(mockTemplate), mockQueryResults, 20, Person.class);

		assertThat(page).isNotNull();
		assertThat(page.getNumberOfElements()).isEqualTo(expectedContent.size());
		assertThat(page).containsAll(expectedContent);

		Page<User> users = page.map(User::from);

		assertThat(users).isNotNull();
		assertThat(users.getNumberOfElements()).isEqualTo(expectedContent.size());
		assertThat(users).contains(User.newUser("jonDoe"), User.newUser("janeDoe"));
	}

	@Data
	@RequiredArgsConstructor(staticName = "newPerson")
	static class Person {
		@NonNull String firstName;
		@NonNull String lastName;

		static Person parse(String name) {
			String[] firstNameLastName = name.split(" ");
			return newPerson(firstNameLastName[0], firstNameLastName[1]);
		}

		String getName() {
			return String.format("%1$s %2$s", getFirstName(), getLastName());
		}
	}

	@Data
	@RequiredArgsConstructor(staticName = "newUser")
	static class User {
		@NonNull String name;

		static User from(Person person) {
			return newUser(String.format("%1$s%2$s", person.getFirstName().toLowerCase(), person.getLastName()));
		}
	}
}
