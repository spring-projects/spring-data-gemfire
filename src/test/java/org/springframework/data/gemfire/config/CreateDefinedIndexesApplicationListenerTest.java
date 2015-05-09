/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertSame;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.startsWith;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;

import org.apache.commons.logging.Log;
import org.junit.Test;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;

import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.query.MultiIndexCreationException;
import com.gemstone.gemfire.cache.query.QueryService;

/**
 * The CreateDefinedIndexesApplicationListenerTest class is a test suite of test cases testing the contract
 * and functionality of the CreateDefinedIndexesApplicationListener class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.CreateDefinedIndexesApplicationListener
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.event.ContextRefreshedEvent
 * @see com.gemstone.gemfire.cache.GemFireCache
 * @see com.gemstone.gemfire.cache.query.QueryService
 * @since 1.7.0
 */
public class CreateDefinedIndexesApplicationListenerTest {

	private CreateDefinedIndexesApplicationListener listener = new CreateDefinedIndexesApplicationListener();

	@Test
	public void getCacheWithApplicationContext() {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"getCacheWithApplicationContext.MockApplicationContext");

		ContextRefreshedEvent mockEvent = mock(ContextRefreshedEvent.class,
			"getCacheWithApplicationContext.MockContextRefreshedEvent");

		GemFireCache mockGemFireCache = mock(GemFireCache.class, "getCacheWithApplicationContext.MockGemFireCache");

		when(mockEvent.getApplicationContext()).thenReturn(mockApplicationContext);
		when(mockApplicationContext.getBean(eq(GemFireCache.class))).thenReturn(mockGemFireCache);

		GemFireCache actualGemFireCache = listener.getCache(mockEvent);

		assertSame(mockGemFireCache, actualGemFireCache);
	}

	@Test(expected = CacheClosedException.class)
	public void getCacheWithApplicationContextThrowingABeansException() {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"getCacheWhenApplicationContextThrowsBeanException.MockApplicationContext");

		ContextRefreshedEvent mockEvent = mock(ContextRefreshedEvent.class,
			"getCacheWhenApplicationContextThrowsBeanException.MockContextRefreshedEvent");

		when(mockEvent.getApplicationContext()).thenReturn(mockApplicationContext);
		when(mockApplicationContext.getBean(eq(GemFireCache.class))).thenThrow(
			new NoSuchBeanDefinitionException("gemfireCachce"));

		listener.getCache(mockEvent);
	}

	@Test
	public void createDefinedIndexesCalledOnContextRefreshedEvent() throws Exception {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"createDefinedIndexesCalledOnContextRefreshedEvent.MockApplicationContext");

		ContextRefreshedEvent mockEvent = mock(ContextRefreshedEvent.class,
			"createDefinedIndexesCalledOnContextRefreshedEvent.MockContextRefreshedEvent");

		GemFireCache mockGemFireCache = mock(GemFireCache.class,
			"createDefinedIndexesCalledOnContextRefreshedEvent.MockGemFireCache");

		QueryService mockQueryService = mock(QueryService.class,
			"createDefinedIndexesCalledOnContextRefreshedEvent.MockQueryService");

		when(mockEvent.getApplicationContext()).thenReturn(mockApplicationContext);
		when(mockApplicationContext.getBean(eq(GemFireCache.class))).thenReturn(mockGemFireCache);
		when(mockGemFireCache.getQueryService()).thenReturn(mockQueryService);

		listener.onApplicationEvent(mockEvent);

		verify(mockQueryService, times(1)).createDefinedIndexes();
	}

	@Test
	public void createDefinedIndexesThrowingAnExceptionIsLogged() throws Exception {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"createDefinedIndexesThrowingAnExceptionIsLogged.MockApplicationContext");

		ContextRefreshedEvent mockEvent = mock(ContextRefreshedEvent.class,
			"createDefinedIndexesThrowingAnExceptionIsLogged.MockContextRefreshedEvent");

		GemFireCache mockGemFireCache = mock(GemFireCache.class,
			"createDefinedIndexesThrowingAnExceptionIsLogged.MockGemFireCache");

		QueryService mockQueryService = mock(QueryService.class,
			"createDefinedIndexesThrowingAnExceptionIsLogged.MockQueryService");

		when(mockEvent.getApplicationContext()).thenReturn(mockApplicationContext);
		when(mockApplicationContext.getBean(eq(GemFireCache.class))).thenReturn(mockGemFireCache);
		when(mockGemFireCache.getQueryService()).thenReturn(mockQueryService);
		when(mockQueryService.createDefinedIndexes()).thenThrow(new MultiIndexCreationException(
			new HashMap<String, Exception>(Collections.singletonMap("TestKey", new RuntimeException("TEST")))));

		final Log mockLog = mock(Log.class, "createDefinedIndexesThrowingAnExceptionIsLogged.MockLog");

		CreateDefinedIndexesApplicationListener listener = new CreateDefinedIndexesApplicationListener() {
			@Override Log initLogger() {
				return mockLog;
			}
		};

		listener.onApplicationEvent(mockEvent);

		verify(mockLog, times(1)).warn(startsWith("unable to create defined Indexes (if any):"));
	}

	@Test
	public void createDefinedIndexesUsingClientCacheLocalQueryService() throws Exception {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"createDefinedIndexesUsingClientCacheLocalQueryService.MockApplicationContext");

		ContextRefreshedEvent mockEvent = mock(ContextRefreshedEvent.class,
			"createDefinedIndexesUsingClientCacheLocalQueryService.MockContextRefreshedEvent");

		ClientCache mockClientCache = mock(ClientCache.class,
			"createDefinedIndexesUsingClientCacheLocalQueryService.MockGemFireCache");

		QueryService mockQueryService = mock(QueryService.class,
			"createDefinedIndexesUsingClientCacheLocalQueryService.MockQueryService");

		when(mockEvent.getApplicationContext()).thenReturn(mockApplicationContext);
		when(mockApplicationContext.getBean(eq(GemFireCache.class))).thenReturn(mockClientCache);
		when(mockClientCache.getLocalQueryService()).thenReturn(mockQueryService);

		listener.onApplicationEvent(mockEvent);

		verify(mockQueryService, times(1)).createDefinedIndexes();
	}

}
