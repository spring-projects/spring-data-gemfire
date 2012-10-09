/*
 * Copyright 2002-2011 the original author or authors.
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
package org.springframework.data.gemfire.function;

/**
 * @author David Turanski
 *
 */


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.function.foo.Foo;
import org.springframework.data.gemfire.function.foo.IFoo;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Region;

/**
 * 
 * @author David Turanski
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class GemfireFunctionProxyFactoryBeanTest {
	
	private GemfireFunctionOperations<?> functionOperations;
	static Log logger = LogFactory.getLog(GemfireFunctionProxyFactoryBeanTest.class);

	@Autowired
	private IFoo foo;

	@Autowired
	private ApplicationContext context;

	private Region<String,Integer> region;

	@SuppressWarnings("unchecked")
	@Before
	public void setUp() {
		assertNotNull(foo);
		functionOperations = mock(GemfireFunctionOperations.class);
		
		region = context.getBean("someRegion",Region.class);
		assertNotNull(region);

		region.put("one",1);
		region.put("two",2);
		region.put("three",3);
	}

	@Test
	public void testInstance() throws Exception {
		GemfireFunctionOperations<?> functionOperations = mock(GemfireFunctionOperations.class);
		GemfireFunctionProxyFactoryBean proxy = new GemfireFunctionProxyFactoryBean(IFoo.class,Foo.class.getName(),functionOperations); 	
		IFoo foo = (IFoo)proxy.getObject();
		assertTrue(foo instanceof FilterAware);
	}

	@Test
	public void testSetFilter() throws Exception {

		GemfireFunctionProxyFactoryBean proxy = new GemfireFunctionProxyFactoryBean(IFoo.class,Foo.class.getName(),functionOperations);  
		IFoo foo = (IFoo)proxy.getObject();

		Set<String> filter = Collections.singleton("foo");
		Object obj = ((FilterAware) foo).setFilter(filter);
		assertSame(obj,foo);
		assertSame(filter,proxy.getFilter());

	}


	@Test
	public void testRemoteExecutionOneArg() {  
		assertEquals(1,foo.oneArg("one").intValue());
		((FilterAware)foo).setFilter(Collections.singleton("one"));
		assertEquals(1,foo.oneArg("one").intValue());
	}

	@Test
	public void testRemoteExecutionTwoArg() {  
		assertEquals(3,foo.twoArg("one","two").intValue());   
	}

	
	@Test
	public void testRemoteExectionArrayList() {
		ArrayList<Integer> ints = new ArrayList<Integer>(Arrays.asList(new Integer[]{1,2,3}));
		assertEquals(1,foo.collections(ints).get(0).intValue());  
	}

	@Test
	public void testRemoteExectionMap() {
		Map<String,Integer> result = foo.getMapWithNoArgs();
		assertEquals(1,result.get("one").intValue());
	}

	@After
	public void tearDown() {
		
	}
}


