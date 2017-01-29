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

package org.springframework.data.gemfire.serialization.json;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
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
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration test to test SDG support for storing and reading JSON data to/from
 * a GemFire Cache {@link Region} by (un)marshalled JSON data using Jackson.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.GemfireOperations
 * @see org.springframework.data.gemfire.serialization.json.JSONRegionAdvice
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings({ "unchecked", "unused" })
public class JSONRegionAdviceTest {

	// TODO figure out why auto-proxying the Region for JSON support prevents the GemfireTemplate from being "auto-wired",
	// as a GemfireTemplate rather than GemfireOperations, resulting in a NoSuchBeanDefinitionException thrown by the
	// Spring container?
	@Autowired
	private GemfireOperations template;

	@Resource(name = "jsonRegion")
	private Region jsonRegion;

	@Before
	public void setup() {
		jsonRegion.clear();
	}

	protected static String toJson(Object bean) {
		try {
			return new ObjectMapper().writeValueAsString(bean);
		}
		catch (JsonProcessingException e) {
			throw new IllegalArgumentException(String.format("Failed to convert object (%1$s) into JSON", bean), e);
		}
	}

	@Test
	public void testPutString() {
		String json = "{\"hello\":\"world\"}";

		jsonRegion.put("key", json);

		assertEquals(json, jsonRegion.put("key", json));

		jsonRegion.create("key2", json);

		assertEquals(json, jsonRegion.get("key"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testPutAll() {
		Map<String, String> map = new HashMap<String, String>();
		map.put("key1", "{\"hello1\":\"world1\"}");
		map.put("key2", "{\"hello2\":\"world2\"}");

		jsonRegion.putAll(map);

		Map<String, String> results = jsonRegion.getAll(Arrays.asList("key1", "key2"));

		assertEquals("{\"hello1\":\"world1\"}", results.get("key1"));
		assertEquals("{\"hello2\":\"world2\"}", results.get("key2"));
	}

	@Test
	public void testObjectToJSon() throws IOException {
		Person daveTuranski = new Person(1L, "Dave", "Turanski");
		jsonRegion.put("dave", daveTuranski);
		String json = String.valueOf(jsonRegion.get("dave"));
		assertEquals(json, toJson(daveTuranski), json);
		Object result = jsonRegion.put("dave", daveTuranski);
		assertEquals(toJson(daveTuranski), result);
	}

	@Test
	public void testTemplateFindUnique() {
		Person daveTuranski = new Person(1L, "Dave", "Turanski");
		jsonRegion.put("dave", daveTuranski);
		String json = template.findUnique("SELECT * FROM /jsonRegion WHERE firstname=$1", "Dave");
		assertEquals(toJson(daveTuranski), json);
	}

	@Test
	public void testTemplateFind() {
		Person daveTuranski = new Person(1L, "Dave", "Turanski");
		jsonRegion.put("dave", daveTuranski);
		SelectResults<String> results = template.find("SELECT * FROM /jsonRegion WHERE firstname=$1", "Dave");
		assertEquals(toJson(daveTuranski), results.iterator().next());
	}

	@Test
	public void testTemplateQuery() {
		Person daveTuranski = new Person(1L, "Dave", "Turanski");
		jsonRegion.put("dave", daveTuranski);
		SelectResults<String> results = template.query("firstname='Dave'");
		assertEquals(toJson(daveTuranski), results.iterator().next());
	}
}
