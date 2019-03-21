/*
 * Copyright 2012 the original author or authors.
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
package org.springframework.data.gemfire.repository.query;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.RootUser;

import com.gemstone.gemfire.cache.Region;

/**
 * 
 * @author Oliver Gierke
 * @author John Blum
 */
@RunWith(MockitoJUnitRunner.class)
public class QueryStringUnitTests {

	@Mock
	@SuppressWarnings("rawtypes")
	Region region;

	protected Sort.Order createOrder(final String property) {
		return createOrder(property, Sort.Direction.ASC);
	}

	protected Sort.Order createOrder(final String property, final Sort.Direction direction) {
		return new Sort.Order(direction, property);
	}

	protected Sort createSort(Sort.Order... orders) {
		return new Sort(orders);
	}

	// SGF-251
	@Test
	public void replacesDomainObjectWithRegionPathCorrectly() {
		QueryString query = new QueryString("SELECT * FROM /Person p WHERE p.firstname = $1");

		when(region.getFullPath()).thenReturn("/foo/bar");

		assertThat(query.forRegion(Person.class, region).toString(), is("SELECT * FROM /foo/bar p WHERE p.firstname = $1"));

		verify(region, never()).getName();
	}
	
	// SGF-156
	// SGF-251
	@Test
	public void replacesDomainObjectWithPluralRegionPathCorrectly() {
		QueryString query = new QueryString("SELECT * FROM /Persons p WHERE p.firstname = $1");

		when(region.getFullPath()).thenReturn("/People");

		assertThat(query.forRegion(Person.class, region).toString(), is("SELECT * FROM /People p WHERE p.firstname = $1"));

		verify(region, never()).getName();
	}

	// SGF-252
	@Test
	public void replacesFullyQualifiedSubregionReferenceWithRegionPathCorrectly() {
		QueryString query = new QueryString("SELECT * FROM //Local/Root/Users u WHERE u.username = $1");

		when(region.getFullPath()).thenReturn("/Local/Root/Users");

		assertThat(query.forRegion(RootUser.class, region).toString(), is("SELECT * FROM /Local/Root/Users u WHERE u.username = $1"));

		verify(region, never()).getName();
	}

	@Test
	public void bindsInValuesCorrectly() {
		QueryString query = new QueryString("SELECT * FROM /Person p WHERE p.firstname IN SET $1");
		List<Integer> values = Arrays.asList(1, 2, 3);
		assertThat(query.bindIn(values).toString(), is("SELECT * FROM /Person p WHERE p.firstname IN SET ('1', '2', '3')"));
	}

	@Test
	public void detectsInParameterIndexesCorrectly() {
		QueryString query = new QueryString("IN SET $1 OR IN SET $2");
		Iterable<Integer> indexes = query.getInParameterIndexes();
		assertThat(indexes, is((Iterable<Integer>) Arrays.asList(1, 2)));
	}

	@Test
	public void addsOrderByClauseCorrectly() {
		QueryString query = new QueryString("SELECT * FROM /People p WHERE p.lastName = $1")
			.orderBy(createSort(createOrder("lastName", Sort.Direction.DESC), createOrder("firstName")));

		assertEquals("SELECT * FROM /People p WHERE p.lastName = $1 ORDER BY lastName DESC, firstName ASC",
			query.toString());
	}

	@Test
	public void addsSingleOrderByClauseCorrectly() {
		QueryString query = new QueryString("SELECT DISTINCT p.lastName FROM /People p WHERE p.firstName = $1")
			.orderBy(createSort(createOrder("lastName")));

		assertEquals("SELECT DISTINCT p.lastName FROM /People p WHERE p.firstName = $1 ORDER BY lastName ASC",
			query.toString());
	}

	@Test
	public void addsNoOrderByClauseCorrectly() {
		QueryString query = new QueryString("SELECT * FROM /People p").orderBy(null);

		assertEquals("SELECT * FROM /People p", query.toString());
	}

}
