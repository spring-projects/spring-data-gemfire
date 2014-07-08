/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.springframework.data.gemfire.support;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.GemfireOperations;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.query.SelectResults;

/**
 * @author David Turanski
 * @author John Blum
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unchecked")
public class JSONRegionAdviceTest {

	// TODO figure out why auto-proxying the Region for JSON support prevents the GemfireTemplate from being "auto-wired",
	// as a GemfireTemplate rather than GemfireOperations, resulting in a NoSuchBeanDefinitionException thrown by the
	// Spring container!?!?!?!?
	@Autowired
	private GemfireOperations template;

	@SuppressWarnings("rawtypes")
	@Resource(name = "jsonRegion")
	private Region jsonRegion;

	@Before
	public void setup() {
		jsonRegion.clear();
	}

	@Test
	public void testPutString() {
		String json = "{\"hello\":\"world\"}";

		jsonRegion.put("key", json);

		assertEquals(json, jsonRegion.put("key", json));

		jsonRegion.create("key2", json);

		System.out.println(jsonRegion.get("key"));

		assertEquals(json, jsonRegion.get("key"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testPutAll() {
		Map<String, String> map = new HashMap<String, String>();
		map.put("key1", "{\"hello1\":\"world1\"}");
		map.put("key2", "{\"hello2\":\"world2\"}");
		jsonRegion.putAll(map);
		List<String> keys = Arrays.asList("key1", "key2");
		Map<String, String> results = jsonRegion.getAll(keys);
		assertEquals("{\"hello1\":\"world1\"}", results.get("key1"));
		assertEquals("{\"hello2\":\"world2\"}", results.get("key2"));
	}

	@Test
	public void testObjectToJSon() throws IOException {
		Person dave = new Person(1L, "Dave", "Turanski");
		jsonRegion.put("dave", dave);
		String json = (String) jsonRegion.get("dave");
		assertEquals(json, "{\"id\":1,\"firstname\":\"Dave\",\"lastname\":\"Turanski\",\"address\":null}", json);
		Object result = jsonRegion.put("dave", dave);
		assertEquals("{\"id\":1,\"firstname\":\"Dave\",\"lastname\":\"Turanski\",\"address\":null}", result);
	}

	@Test
	public void testTemplateFindUnique() {
		Person dave = new Person(1L, "Dave", "Turanski");
		jsonRegion.put("dave", dave);
		String json = (String) template.findUnique("SELECT * FROM /jsonRegion WHERE firstname=$1", "Dave");
		assertEquals("{\"id\":1,\"firstname\":\"Dave\",\"lastname\":\"Turanski\",\"address\":null}", json);
	}

	@Test
	public void testTemplateFind() {
		Person dave = new Person(1L, "Dave", "Turanski");
		jsonRegion.put("dave", dave);
		SelectResults<String> results = template.find("SELECT * FROM /jsonRegion WHERE firstname=$1", "Dave");
		assertEquals("{\"id\":1,\"firstname\":\"Dave\",\"lastname\":\"Turanski\",\"address\":null}",
			results.iterator().next());
	}

	@Test
	public void testTemplateQuery() {
		Person dave = new Person(1L, "Dave", "Turanski");
		jsonRegion.put("dave", dave);
		SelectResults<String> results = template.query("firstname='Dave'");
		assertEquals("{\"id\":1,\"firstname\":\"Dave\",\"lastname\":\"Turanski\",\"address\":null}",
			results.iterator().next());
	}

}
