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

package org.springframework.data.gemfire.serialization.json;

import static org.junit.Assert.assertEquals;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.io.IOException;
import java.util.Arrays;
import java.util.Map;

import javax.annotation.Resource;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.SelectResults;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.GemfireOperations;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.test.support.MapBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration test to test SDG support for storing and reading JSON data to/from a {@link Region}
 * by (un)marshalling JSON data using Jackson.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.GemfireOperations
 * @see org.springframework.data.gemfire.serialization.json.JSONRegionAdvice
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see com.fasterxml.jackson.databind.ObjectMapper
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings({ "unchecked", "unused" })
public class JSONRegionAdviceIntegrationTests {

	// TODO figure out why auto-proxying the Region for JSON support prevents the GemfireTemplate from being "auto-wired",
	// as a GemfireTemplate rather than GemfireOperations, resulting in a NoSuchBeanDefinitionException thrown by the
	// Spring container?
	@Autowired
	private GemfireOperations template;

	@Resource(name = "JsonRegion")
	private Region jsonRegion;

	@Before
	public void setup() {
		jsonRegion.clear();
	}

	private static String toJson(Object bean) {

		try {
			return new ObjectMapper().writeValueAsString(bean);
		}
		catch (JsonProcessingException cause) {
			throw newIllegalArgumentException(cause, "Failed to convert object (%1$s) into JSON", bean);
		}
	}

	@Test
	public void putAndCreate() {

		String json = "{\"hello\":\"world\"}";

		this.jsonRegion.put("keyOne", json);

		assertEquals(json, this.jsonRegion.put("keyOne", json));

		this.jsonRegion.create("keyTwo", json);

		assertEquals(json, this.jsonRegion.get("keyTwo"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void putAll() {

		Map<String, String> map = MapBuilder.<String, String>newMapBuilder()
			.put("key1", "{\"hello1\":\"world1\"}")
			.put("key2", "{\"hello2\":\"world2\"}")
			.build();

		this.jsonRegion.putAll(map);

		Map<String, String> results = this.jsonRegion.getAll(Arrays.asList("key1", "key2"));

		assertEquals("{\"hello1\":\"world1\"}", results.get("key1"));
		assertEquals("{\"hello2\":\"world2\"}", results.get("key2"));
	}

	@Test
	public void objectToJSon() throws IOException {

		Person davidTuranski = new Person(1L, "David", "Turanski");

		this.jsonRegion.put("dave", davidTuranski);

		String json = String.valueOf(this.jsonRegion.get("dave"));

		System.out.printf("JSON [%s]%n", json);

		assertEquals(json, toJson(davidTuranski));

		Object result = jsonRegion.put("dave", davidTuranski);

		assertEquals(toJson(davidTuranski), result);
	}

	@Test
	public void templateFind() {

		Person davidTuranski = new Person(1L, "David", "Turanski");

		this.jsonRegion.put("dave", davidTuranski);

		SelectResults<String> results = this.template.find(String.format("SELECT * FROM %s WHERE firstname=$1",
			this.jsonRegion.getFullPath()), davidTuranski.getFirstname());

		assertEquals(toJson(davidTuranski), results.iterator().next());
	}

	@Test
	public void templateFindUnique() {

		Person davidTuranski = new Person(1L, "David", "Turanski");

		this.jsonRegion.put("dave", davidTuranski);

		String json = this.template.findUnique(String.format("SELECT * FROM %s WHERE firstname=$1",
			this.jsonRegion.getFullPath()), davidTuranski.getFirstname());

		assertEquals(toJson(davidTuranski), json);
	}

	@Test
	public void templateQuery() {

		Person davidTuranski = new Person(1L, "David", "Turanski");

		this.jsonRegion.put("dave", davidTuranski);

		SelectResults<String> results =
			this.template.query(String.format("firstname='%s'", davidTuranski.getFirstname()));

		assertEquals(toJson(davidTuranski), results.iterator().next());
	}
}
