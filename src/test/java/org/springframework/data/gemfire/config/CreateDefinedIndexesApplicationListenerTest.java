/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.startsWith;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;

import org.apache.commons.logging.Log;
import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;

import com.gemstone.gemfire.cache.query.MultiIndexCreationException;
import com.gemstone.gemfire.cache.query.QueryService;

/**
 * The CreateDefinedIndexesApplicationListenerTest class is a test suite of test cases testing the contract
 * and functionality of the CreateDefinedIndexesApplicationListener class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.event.ContextRefreshedEvent
 * @see org.springframework.data.gemfire.config.CreateDefinedIndexesApplicationListener
 * @see com.gemstone.gemfire.cache.query.QueryService
 * @since 1.7.0
 */
public class CreateDefinedIndexesApplicationListenerTest {

	private CreateDefinedIndexesApplicationListener listener = new CreateDefinedIndexesApplicationListener();

	@Test
	public void createDefinedIndexesCalledOnContextRefreshedEvent() throws Exception {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"testCreateDefinedIndexesCalledOnContextRefreshedEvent.MockApplicationContext");

		ContextRefreshedEvent mockEvent = mock(ContextRefreshedEvent.class,
			"testCreateDefinedIndexesCalledOnContextRefreshedEvent.MockContextRefreshedEvent");

		QueryService mockQueryService = mock(QueryService.class,
			"testCreateDefinedIndexesCalledOnContextRefreshedEvent.MockQueryService");

		when(mockEvent.getApplicationContext()).thenReturn(mockApplicationContext);
		when(mockApplicationContext.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE)))
			.thenReturn(true);
		when(mockApplicationContext.getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			eq(QueryService.class))).thenReturn(mockQueryService);

		listener.onApplicationEvent(mockEvent);

		verify(mockEvent, times(1)).getApplicationContext();
		verify(mockApplicationContext, times(1)).containsBean(
			eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));
		verify(mockApplicationContext, times(1)).getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE), eq(QueryService.class));
		verify(mockQueryService, times(1)).createDefinedIndexes();
	}

	@Test
	public void createDefinedIndexesNotCalledOnContextRefreshedEvent() throws Exception {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"testCreateDefinedIndexesUsingClientCacheLocalQueryService.MockApplicationContext");

		ContextRefreshedEvent mockEvent = mock(ContextRefreshedEvent.class,
			"testCreateDefinedIndexesUsingClientCacheLocalQueryService.MockContextRefreshedEvent");

		QueryService mockQueryService = mock(QueryService.class,
			"testCreateDefinedIndexesUsingClientCacheLocalQueryService.MockQueryService");

		when(mockEvent.getApplicationContext()).thenReturn(mockApplicationContext);
		when(mockApplicationContext.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE)))
			.thenReturn(false);
		when(mockApplicationContext.getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			eq(QueryService.class))).thenReturn(mockQueryService);

		listener.onApplicationEvent(mockEvent);

		verify(mockEvent, times(1)).getApplicationContext();
		verify(mockApplicationContext, times(1)).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));
		verify(mockApplicationContext, never()).getBean(anyString(), any(QueryService.class));
		verify(mockQueryService, never()).createDefinedIndexes();
	}

	@Test
	public void createDefinedIndexesThrowingAnExceptionIsLogged() throws Exception {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"testCreateDefinedIndexesThrowingAnExceptionIsLogged.MockApplicationContext");

		ContextRefreshedEvent mockEvent = mock(ContextRefreshedEvent.class,
			"testCreateDefinedIndexesThrowingAnExceptionIsLogged.MockContextRefreshedEvent");

		QueryService mockQueryService = mock(QueryService.class,
			"testCreateDefinedIndexesThrowingAnExceptionIsLogged.MockQueryService");

		when(mockEvent.getApplicationContext()).thenReturn(mockApplicationContext);
		when(mockApplicationContext.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE)))
			.thenReturn(true);
		when(mockApplicationContext.getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			eq(QueryService.class))).thenReturn(mockQueryService);
		when(mockQueryService.createDefinedIndexes()).thenThrow(new MultiIndexCreationException(
			new HashMap<String, Exception>(Collections.singletonMap("TestKey", new RuntimeException("TEST")))));

		final Log mockLog = mock(Log.class, "testCreateDefinedIndexesThrowingAnExceptionIsLogged.MockLog");

		CreateDefinedIndexesApplicationListener listener = new CreateDefinedIndexesApplicationListener() {
			@Override Log initLogger() {
				return mockLog;
			}
		};

		listener.onApplicationEvent(mockEvent);

		verify(mockEvent, times(1)).getApplicationContext();
		verify(mockApplicationContext, times(1)).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));
		verify(mockApplicationContext, times(1)).getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE), eq(QueryService.class));
		verify(mockLog, times(1)).warn(startsWith("unable to create defined Indexes (if any):"));
	}

}
