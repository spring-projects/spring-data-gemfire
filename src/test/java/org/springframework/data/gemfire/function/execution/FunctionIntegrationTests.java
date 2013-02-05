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
package org.springframework.data.gemfire.function.execution;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.annotation.Resource;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.fork.SpringCacheServerProcess;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;
import org.springframework.data.gemfire.function.annotation.RegionData;
import org.springframework.stereotype.Component;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Region;

/**
 * @author David Turanski
 *
 */

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class FunctionIntegrationTests {
	@Resource(name="test-region") 
	Region<String,Integer> region;
	
	@BeforeClass
	public static void startUp() throws Exception {
		ForkUtil.startCacheServer(SpringCacheServerProcess.class.getName() + 
		 " /org/springframework/data/gemfire/function/execution/FunctionIntegrationTests-server-context.xml");
	}

	@AfterClass
	public static void cleanUp() {
		ForkUtil.sendSignal();
	}
	
	@Before 
	public void initializeRegion() {
		region.put("one", 1);
		region.put("two", 2);
		region.put("three",3);
	}
	
	@Test
	public void testOnRegionFunctionExecution() {
		
		GemfireOnRegionOperations template = new GemfireOnRegionFunctionTemplate(region);
		
		Iterable<Integer> results;
		results = template.execute("oneArg","two");
		assertEquals(2, results.iterator().next().intValue());
		
		Set<String> filter = new HashSet<String>();
		filter.add("one");
		results = template.execute("oneArg",filter,"two");
		assertFalse(results.iterator().hasNext());
		
		results = template.execute("twoArg","two","three");
		assertEquals(5, results.iterator().next().intValue());
		
		Integer result = template.executeAndExtract("twoArg","two","three");
		assertEquals(5,result.intValue());
	}
	
	@Test
	public void testCollectionReturnTypes() {
		GemfireOnRegionOperations template = new GemfireOnRegionFunctionTemplate(region);
		
		Object result = template.executeAndExtract("getMapWithNoArgs");
		assertTrue(result instanceof Map);
		@SuppressWarnings("unchecked")
		Map<String,Integer> map = (Map<String,Integer>)result;
		assertEquals(1,map.get("one").intValue());
		assertEquals(2,map.get("two").intValue());
		assertEquals(3,map.get("three").intValue());
		
		result = template.executeAndExtract("collections",Arrays.asList(new Integer[]{1,2,3,4,5}));
		assertTrue(result.getClass().getName(),result instanceof List);
 	 
		List<?> list = (List<?>)result;
		assertEquals(5, list.size());
		for (int i=1; i<= list.size(); i++) {
			assertEquals(i,list.get(i-1));
		}
	}
	
	@Test 
	public void testArrayReturnTypes() {
		GemfireOnRegionOperations template = new GemfireOnRegionFunctionTemplate(region);
		Object result = template.executeAndExtract("arrays",new int[]{1,2,3,4,5});
		assertTrue(result.getClass().getName(),result instanceof int[]);
	}
	/*
	 * This gets wrapped in a GemFire Function and registered on the forked server.
	 */
	@Component
	public static class Foo  {

	    @GemfireFunction(id="oneArg")
		public Integer oneArg(String key, @RegionData Map<String,Integer> dataSet) {
			return dataSet.get(key);
		}
	    
	    @GemfireFunction(id="twoArg")
		public Integer twoArg(String akey, String bkey, @RegionData Map<String,Integer> dataSet) {
			if (dataSet.get(akey) != null && dataSet.get(bkey) != null) {
				return dataSet.get(akey) + dataSet.get(bkey);
			}
			return null;	
		}
	    
	    @GemfireFunction(id="collections")
		public List<Integer> collections(List<Integer> args) {
			return args;
		}

	    @GemfireFunction(id="getMapWithNoArgs")
		public Map<String, Integer> getMapWithNoArgs(@RegionData Map<String,Integer> dataSet) {
			if (dataSet.size() == 0) {
				return null;
			}
			return new HashMap<String, Integer>(dataSet);
		}
	    
	    @GemfireFunction(id="arrays",batchSize=2)  
	    	public int[] collections(int[] args) {
				return args;
	    }
	}
}
