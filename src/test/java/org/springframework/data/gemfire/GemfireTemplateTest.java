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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.atomic.AtomicBoolean;
import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.data.gemfire.test.MockRegionFactory;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.GemFireCheckedException;
import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.query.FunctionDomainException;
import com.gemstone.gemfire.cache.query.NameResolutionException;
import com.gemstone.gemfire.cache.query.Query;
import com.gemstone.gemfire.cache.query.QueryInvocationTargetException;
import com.gemstone.gemfire.cache.query.QueryService;
import com.gemstone.gemfire.cache.query.SelectResults;
import com.gemstone.gemfire.cache.query.TypeMismatchException;

/**
 * The GemfireTemplateTest class is a test suite of test cases testing the contract and functionality of the SDG
 * GemfireTemplate class.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.GemfireTemplate
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="basic-template.xml", initializers=GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class GemfireTemplateTest  {

	private static final String MULTI_QUERY = "SELECT * FROM /simple";
	private static final String SINGLE_QUERY = "(SELECT * FROM /simple).size";

	@Autowired
	private GemfireTemplate template;

	@Resource(name = "simple")
	private Region<?, ?> simple;

	@Before
	@SuppressWarnings("rawtypes")
	public void setUp() throws FunctionDomainException, TypeMismatchException, NameResolutionException, QueryInvocationTargetException {
		QueryService queryService = MockRegionFactory.mockQueryService();
		Query singleQuery = mock(Query.class);

		when(singleQuery.execute(any(Object[].class))).thenReturn(0);
		when(queryService.newQuery(SINGLE_QUERY)).thenReturn(singleQuery);

		Query multipleQuery = mock(Query.class);
		SelectResults selectResults = mock(SelectResults.class);

		when(multipleQuery.execute(any(Object[].class))).thenReturn(selectResults);
		when(queryService.newQuery(MULTI_QUERY)).thenReturn(multipleQuery);
	}

	@After
	public void tearDown() {
		template.setExposeNativeRegion(false);
	}

	@Test
	public void testConstructWithNonNullRegion() {
		GemfireTemplate localTemplate = new GemfireTemplate(simple);

		assertNotNull(localTemplate);
		assertSame(simple, localTemplate.getRegion());
		assertFalse(localTemplate.isExposeNativeRegion());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConstructWithNullRegion() {
		try {
			new GemfireTemplate(null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The GemFire Cache Region is required.", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testExecuteUsingNativeRegion() {
		template.setExposeNativeRegion(true);

		assertTrue(template.isExposeNativeRegion());
		assertSame(simple, template.getRegion());

		final AtomicBoolean callbackInvoked = new AtomicBoolean(false);

		template.execute(new GemfireCallback<Object>() {
			@Override
			public Object doInGemfire(final Region<?, ?> region) throws GemFireCheckedException, GemFireException {
				callbackInvoked.set(true);
				assertSame(simple, region);
				return null;
			}
		});

		assertTrue(callbackInvoked.get());
	}

	@Test
	public void testExecuteUsingProxyRegion() {
		assertFalse(template.isExposeNativeRegion());

		final AtomicBoolean callbackInvoked = new AtomicBoolean(false);

		template.execute(new GemfireCallback<Object>() {
			@Override
			public Object doInGemfire(final Region<?, ?> region) throws GemFireCheckedException, GemFireException {
				callbackInvoked.set(true);
				assertNotSame(simple, region);
				return null;
			}
		});

		assertTrue(callbackInvoked.get());
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
		assertNotNull(template.find(MULTI_QUERY));
	}

	@Test
	public void testFindUnique() throws Exception {
		assertEquals(0, template.findUnique(SINGLE_QUERY));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testLookupQueryService() {
		ClientCache mockClientCache = mock(ClientCache.class, "testLookupQueryService.ClientCache");
		QueryService mockQueryService = mock(QueryService.class, "testLookupQueryService.QueryService");
		Region<Object, Object> mockRegion = mock(Region.class, "testLookupQueryService.Region");

		when(mockRegion.getRegionService()).thenReturn(mockClientCache);
		when(mockClientCache.getQueryService()).thenReturn(mockQueryService);

		GemfireTemplate localTemplate = new GemfireTemplate(mockRegion) {
			@Override boolean isLocalWithNoServerProxy(final Region<?, ?> region) {
				return false;
			}
		};

		assertSame(mockQueryService, localTemplate.lookupQueryService(mockRegion));

		verify(mockRegion, times(2)).getRegionService();
		verify(mockClientCache, times(1)).getQueryService();
		verify(mockClientCache, never()).getLocalQueryService();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testLookupLocalQueryService() {
		ClientCache mockClientCache = mock(ClientCache.class, "testLookupLocalQueryService.ClientCache");
		QueryService mockQueryService = mock(QueryService.class, "testLookupLocalQueryService.QueryService");
		Region<Object, Object> mockRegion = mock(Region.class, "testLookupLocalQueryService.Region");
		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class, "testLookupLocalQueryService.RegionAttributes");

		when(mockClientCache.getLocalQueryService()).thenReturn(mockQueryService);
		when(mockRegion.getRegionService()).thenReturn(mockClientCache);
		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getScope()).thenReturn(Scope.LOCAL);

		GemfireTemplate localTemplate = new GemfireTemplate(mockRegion) {
			@Override boolean isLocalWithNoServerProxy(final Region<?, ?> region) {
				return true;
			}
		};

		assertSame(mockQueryService, localTemplate.lookupQueryService(mockRegion));

		verify(mockClientCache, never()).getQueryService();
		verify(mockClientCache, times(1)).getLocalQueryService();
		verify(mockRegion, times(2)).getRegionService();
		verify(mockRegionAttributes, times(1)).getScope();
	}

}
