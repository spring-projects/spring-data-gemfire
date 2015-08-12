/*
 * Copyright 2012 the original author or authors.
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
 */

package org.springframework.data.gemfire.repository.query;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
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

	@Test
	public void createQueryStringWithCount() {
		QueryString queryString = new QueryString(Person.class, true);

		assertThat(queryString, is(notNullValue()));
		assertThat(queryString.toString(), is(equalTo("SELECT count(*) FROM /Person")));
	}

	@Test
	public void createQueryStringWithoutCount() {
		QueryString queryString = new QueryString(Person.class);

		assertThat(queryString, is(notNullValue()));
		assertThat(queryString.toString(), is(equalTo("SELECT * FROM /Person")));
	}

	@Test
	public void hintPatternMatches() {
		assertThat(QueryString.HINT_PATTERN.matcher("<HINT 'ExampleIdx'>").find(), is(true));
		assertThat(QueryString.HINT_PATTERN.matcher("<HINT 'IdIdx'> SELECT * FROM /Example WHERE id = $1").find(), is(true));
		assertThat(QueryString.HINT_PATTERN.matcher("<HINT 'LastNameIdx', 'BirthDateIdx'> SELECT * FROM /Person WHERE lastName = $1 AND birthDate = $2").find(), is(true));
	}

	@Test
	public void hintPatternNoMatches() {
		assertThat(QueryString.HINT_PATTERN.matcher("HINT").find(), is(false));
		assertThat(QueryString.HINT_PATTERN.matcher("<HINT>").find(), is(false));
		assertThat(QueryString.HINT_PATTERN.matcher("<HINT ''>").find(), is(false));
		assertThat(QueryString.HINT_PATTERN.matcher("<HINT IdIdx>").find(), is(false));
		assertThat(QueryString.HINT_PATTERN.matcher("<HINT IdIdx, LastNameIdx>").find(), is(false));
		assertThat(QueryString.HINT_PATTERN.matcher("SELECT * FROM /Example").find(), is(false));
		assertThat(QueryString.HINT_PATTERN.matcher("SELECT * FROM /Hint").find(), is(false));
		assertThat(QueryString.HINT_PATTERN.matcher("SELECT x.hint FROM /Clues x WHERE x.hint > $1").find(), is(false));
	}

	@Test
	public void importPatternMatches() {
		assertThat(QueryString.IMPORT_PATTERN.matcher("IMPORT *;").find(), is(true));
		assertThat(QueryString.IMPORT_PATTERN.matcher("IMPORT org.example.*;").find(), is(true));
		assertThat(QueryString.IMPORT_PATTERN.matcher("IMPORT org.example.Type;").find(), is(true));
		assertThat(QueryString.IMPORT_PATTERN.matcher("IMPORT org.example.app.DomainType; SELECT * FROM /Domain").find(), is(true));
	}

	@Test
	public void importPatternNoMatches() {
		assertThat(QueryString.IMPORT_PATTERN.matcher("IMPORT").find(), is(false));
		assertThat(QueryString.IMPORT_PATTERN.matcher("IMPORT ;").find(), is(false));
		assertThat(QueryString.IMPORT_PATTERN.matcher("IMPORT *").find(), is(false));
		assertThat(QueryString.IMPORT_PATTERN.matcher("SELECT * FROM /Example").find(), is(false));
	}

	@Test
	public void limitPatternMatches() {
		assertThat(QueryString.LIMIT_PATTERN.matcher("LIMIT 10").find(), is(true));
		assertThat(QueryString.LIMIT_PATTERN.matcher("SELECT * FROM /Example LIMIT 10").find(), is(true));
		assertThat(QueryString.LIMIT_PATTERN.matcher("SELECT * FROM /Example WHERE id = $1 LIMIT 10").find(), is(true));
	}

	@Test
	public void limitPatternNoMatches() {
		assertThat(QueryString.LIMIT_PATTERN.matcher("LIMIT").find(), is(false));
		assertThat(QueryString.LIMIT_PATTERN.matcher("LIMIT lO").find(), is(false));
		assertThat(QueryString.LIMIT_PATTERN.matcher("LIMIT FF").find(), is(false));
		assertThat(QueryString.LIMIT_PATTERN.matcher("LIMIT ten").find(), is(false));
		assertThat(QueryString.LIMIT_PATTERN.matcher("SELECT * FROM /Example LIMIT").find(), is(false));
		assertThat(QueryString.LIMIT_PATTERN.matcher("SELECT * FROM /Example WHERE id = $1 LIM 10").find(), is(false));
	}

	@Test
	public void tracePatternMatches() {
		assertThat(QueryString.TRACE_PATTERN.matcher("<TRACE>").find(), is(true));
		assertThat(QueryString.TRACE_PATTERN.matcher("<TRACE> SELECT * FROM /Example").find(), is(true));
		assertThat(QueryString.TRACE_PATTERN.matcher("<TRACE>SELECT * FROM /Example").find(), is(true));
		assertThat(QueryString.TRACE_PATTERN.matcher("SELECT * FROM /Example<TRACE>").find(), is(true));
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

		assertThat(query.forRegion(Person.class, region).toString(),
			is("SELECT * FROM /People p WHERE p.firstname = $1"));

		verify(region, never()).getName();
	}

	// SGF-252
	@Test
	public void replacesFullyQualifiedSubRegionReferenceWithRegionPathCorrectly() {
		QueryString query = new QueryString("SELECT * FROM //Local/Root/Users u WHERE u.username = $1");

		when(region.getFullPath()).thenReturn("/Local/Root/Users");

		assertThat(query.forRegion(RootUser.class, region).toString(), is(equalTo(
			"SELECT * FROM /Local/Root/Users u WHERE u.username = $1")));

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
	public void addsNoOrderByClauseCorrectly() {
		QueryString query = new QueryString("SELECT * FROM /People p").orderBy(null);

		assertThat(query.toString(), is(equalTo("SELECT * FROM /People p")));
	}

	@Test
	public void addsOrderByClauseCorrectly() {
		QueryString query = new QueryString("SELECT * FROM /People p WHERE p.lastName = $1")
			.orderBy(createSort(createOrder("lastName", Sort.Direction.DESC), createOrder("firstName")));

		assertThat(query.toString(), is(equalTo(
			"SELECT * FROM /People p WHERE p.lastName = $1 ORDER BY lastName DESC, firstName ASC")));
	}

	@Test
	public void addsSingleOrderByClauseCorrectly() {
		QueryString query = new QueryString("SELECT DISTINCT p.lastName FROM /People p WHERE p.firstName = $1")
			.orderBy(createSort(createOrder("lastName")));

		assertThat(query.toString(), is(equalTo(
			"SELECT DISTINCT p.lastName FROM /People p WHERE p.firstName = $1 ORDER BY lastName ASC")));
	}

	@Test
	public void withHints() {
		assertThat(new QueryString("SELECT * FROM /Example").withHints("IdIdx").toString(),
			is(equalTo("<HINT 'IdIdx'> SELECT * FROM /Example")));
		assertThat(new QueryString("SELECT * FROM /Example").withHints("IdIdx", "SpatialIdx", "TxDateIdx").toString(),
			is(equalTo("<HINT 'IdIdx', 'SpatialIdx', 'TxDateIdx'> SELECT * FROM /Example")));
	}

	@Test
	public void withoutHints() {
		QueryString expectedQueryString = new QueryString("SELECT * FROM /Example");

		assertThat(expectedQueryString.withHints((String[]) null), is(sameInstance(expectedQueryString)));
		assertThat(expectedQueryString.withHints(), is(sameInstance(expectedQueryString)));
	}

	@Test
	public void withImport() {
		assertThat(new QueryString("SELECT * FROM /People").withImport("org.example.app.domain.Person").toString(),
			is(equalTo("IMPORT org.example.app.domain.Person; SELECT * FROM /People")));
	}

	@Test
	public void withoutImport() {
		QueryString expectedQueryString = new QueryString("SELECT * FROM /Example");

		assertThat(expectedQueryString.withImport(null), is(sameInstance(expectedQueryString)));
		assertThat(expectedQueryString.withImport(""), is(sameInstance(expectedQueryString)));
		assertThat(expectedQueryString.withImport("  "), is(sameInstance(expectedQueryString)));
	}

	@Test
	public void withLimit() {
		assertThat(new QueryString("SELECT * FROM /Example").withLimit(10).toString(),
			is(equalTo("SELECT * FROM /Example LIMIT 10")));
		assertThat(new QueryString("SELECT * FROM /Example").withLimit(-5).toString(),
			is(equalTo("SELECT * FROM /Example LIMIT -5")));
		assertThat(new QueryString("SELECT * FROM /Example").withLimit(0).toString(),
			is(equalTo("SELECT * FROM /Example LIMIT 0")));
	}

	@Test
	public void withoutLimit() {
		QueryString expectedQueryString = new QueryString("SELECT * FROM /Example");
		assertThat(expectedQueryString.withLimit(null), is(sameInstance(expectedQueryString)));
	}

	@Test
	public void withTrace() {
		assertThat(new QueryString("SELECT * FROM /Example").withTrace().toString(),
			is(equalTo("<TRACE> SELECT * FROM /Example")));
	}

	@Test
	public void withHintAndTrace() {
		assertThat(new QueryString("SELECT * FROM /Example").withHints("IdIdx").withTrace().toString(),
			is(equalTo("<TRACE> <HINT 'IdIdx'> SELECT * FROM /Example")));
	}

	@Test
	public void withHintImportLimitAndTrace() {
		assertThat(new QueryString("SELECT * FROM /Example").withImport("org.example.domain.Type")
			.withHints("IdIdx", "NameIdx").withLimit(20).withTrace().toString(),
				is(equalTo("<TRACE> <HINT 'IdIdx', 'NameIdx'> IMPORT org.example.domain.Type; SELECT * FROM /Example LIMIT 20")));
	}

}
