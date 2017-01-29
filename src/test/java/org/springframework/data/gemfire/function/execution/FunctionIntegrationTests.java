/*
 * Copyright 2002-2018 the original author or authors.
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

package org.springframework.data.gemfire.function.execution;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.fork.ServerProcess;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;
import org.springframework.data.gemfire.function.annotation.RegionData;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.stereotype.Component;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @author David Turanski
 * @author John Blum
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class FunctionIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static ProcessWrapper gemfireServer;

	@Resource(name = "test-region")
	private Region<String, Integer> region;

	@BeforeClass
	public static void startGemFireServer() throws Exception {
		int availablePort = findAvailablePort();

		gemfireServer = run(ServerProcess.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort),
			getServerContextXmlFileLocation(FunctionIntegrationTests.class));

		waitForServerToStart(DEFAULT_HOSTNAME, availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}

	@Before
	public void initializeRegion() {
		region.put("one", 1);
		region.put("two", 2);
		region.put("three", 3);
	}

	@Test
	//@Ignore
	public void testVoidReturnType() {
		GemfireOnRegionOperations template = new GemfireOnRegionFunctionTemplate(region);
		// Should work either way but the first invocation traps an exception if there is a result.
		template.execute("noResult");
		template.executeWithNoResult("noResult");
	}

	@Test
	//@Ignore
	@SuppressWarnings("unchecked")
	public void testCollectionReturnTypes() {
		GemfireOnRegionOperations template = new GemfireOnRegionFunctionTemplate(region);

		Object result = template.executeAndExtract("getMapWithNoArgs");

		assertTrue(result.getClass().getName(), result instanceof Map);

		Map<String, Integer> map = (Map<String, Integer>) result;

		assertEquals(1, map.get("one").intValue());
		assertEquals(2, map.get("two").intValue());
		assertEquals(3, map.get("three").intValue());

		result = template.executeAndExtract("collections", Arrays.asList(1, 2, 3, 4, 5));

		assertTrue(result.getClass().getName(), result instanceof List);

		List<?> list = (List<?>) result;

		assertFalse(list.isEmpty());
		assertEquals(5, list.size());

		int expectedNumber = 1;

		for (Object actualNumber : list) {
			assertEquals(expectedNumber++, actualNumber);
		}
	}

	@Test
	@SuppressWarnings("all")
	public void testArrayReturnTypes() {
		Object result = new GemfireOnRegionFunctionTemplate(region)
			.executeAndExtract("arrays", new int[] { 1, 2, 3, 4, 5 });

		assertTrue(result.getClass().getName(), result instanceof int[]);
		assertEquals(5, ((int[]) result).length);
	}

	@Test
	//@Ignore
	public void testOnRegionFunctionExecution() {
		GemfireOnRegionOperations template = new GemfireOnRegionFunctionTemplate(region);

		assertEquals(2, template.<Integer>execute("oneArg", "two").iterator().next().intValue());
		assertFalse(template.<Integer>execute("oneArg", Collections.singleton("one"), "two").iterator().hasNext());
		assertEquals(5, template.<Integer>execute("twoArg", "two", "three").iterator().next().intValue());
		assertEquals(5, template.<Integer>executeAndExtract("twoArg", "two", "three").intValue());
	}

	/*
	 * This gets wrapped in a GemFire Function and registered on the forked server.
	 */
	@Component
	@SuppressWarnings("unused")
	public static class Foo {

		@GemfireFunction(id = "oneArg")
		public Integer oneArg(String key, @RegionData Map<String, Integer> region) {
			return region.get(key);
		}

		@GemfireFunction(id = "twoArg")
		public Integer twoArg(String keyOne, String keyTwo, @RegionData Map<String, Integer> region) {
			if (region.get(keyOne) != null && region.get(keyTwo) != null) {
				return region.get(keyOne) + region.get(keyTwo);
			}

			return null;
		}

		@GemfireFunction(id = "collections")
		public List<Integer> collections(List<Integer> args) {
			return args;
		}

		@GemfireFunction(id = "getMapWithNoArgs")
		public Map<String, Integer> getMapWithNoArgs(@RegionData Map<String, Integer> region) {
			if (region.size() == 0) {
				return null;
			}

			return new HashMap<String, Integer>(region);
		}

		@GemfireFunction(id = "arrays")
		// TODO causes OOME!
		//@GemfireFunction(id = "arrays", batchSize = 2)
		public int[] collections(int[] args) {
			return args;
		}

		@GemfireFunction
		public void noResult() {
		}
	}

}
