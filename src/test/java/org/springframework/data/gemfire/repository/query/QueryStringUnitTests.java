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

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.data.gemfire.repository.sample.Person;

import com.gemstone.gemfire.cache.Region;

/**
 * 
 * @author Oliver Gierke
 */
@RunWith(MockitoJUnitRunner.class)
public class QueryStringUnitTests {

	@Mock
	@SuppressWarnings("rawtypes")
	Region region;

	@Test
	public void replacesDomainObjectWithRegionNameCorrectly() {

		QueryString query = new QueryString("SELECT * FROM /Person p WHERE p.firstname = $1");

		when(region.getName()).thenReturn("foo");
		assertThat(query.forRegion(Person.class, region).toString(), is("SELECT * FROM /foo p WHERE p.firstname = $1"));
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
}
