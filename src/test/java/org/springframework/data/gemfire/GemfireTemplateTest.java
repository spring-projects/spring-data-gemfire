/*
 * Copyright 2011-2013 the original author or authors.
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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.data.gemfire.test.MockRegionFactory;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.query.FunctionDomainException;
import com.gemstone.gemfire.cache.query.NameResolutionException;
import com.gemstone.gemfire.cache.query.Query;
import com.gemstone.gemfire.cache.query.QueryInvocationTargetException;
import com.gemstone.gemfire.cache.query.QueryService;
import com.gemstone.gemfire.cache.query.SelectResults;
import com.gemstone.gemfire.cache.query.TypeMismatchException;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="/org/springframework/data/gemfire/basic-template.xml",
initializers=GemfireTestApplicationContextInitializer.class)
public class GemfireTemplateTest  {

	private static final String MULTI_QUERY = "select * from /simple";

	private static final String SINGLE_QUERY = "(select * from /simple).size";
	
	@Autowired GemfireOperations template;
	
	@SuppressWarnings("rawtypes")
	@Before 
	public void setUp() throws FunctionDomainException, TypeMismatchException, NameResolutionException, QueryInvocationTargetException {
		//Only applies if Mocks are enabled
		QueryService querySevice = MockRegionFactory.mockQueryService();
		Query singleQuery = mock(Query.class);
		when(singleQuery.execute(any(Object[].class))).thenReturn(0);
		Query multipleQuery = mock(Query.class);
	 	
		SelectResults selectResults = mock(SelectResults.class);
		when(multipleQuery.execute(any(Object[].class))).thenReturn(selectResults);
		
		when(querySevice.newQuery(SINGLE_QUERY)).thenReturn(singleQuery);
		when(querySevice.newQuery(MULTI_QUERY)).thenReturn(multipleQuery);
	}

	@Test(expected = InvalidDataAccessApiUsageException.class)
	public void testFindMultiException() throws Exception {
  		template.find(SINGLE_QUERY);
	}

	@Test(expected = InvalidDataAccessApiUsageException.class)
	public void testFindMultiOne() throws Exception {
 		template.findUnique(MULTI_QUERY);
	}
	
	@Test
	public void testFind() throws Exception {
 		SelectResults<Object> find = template.find(MULTI_QUERY);
		assertNotNull(find);
	}
	
	@Test
	public void testFindUnique() throws Exception {
 		Integer find = template.findUnique(SINGLE_QUERY);
		assertEquals(find, Integer.valueOf(0));
	}

}