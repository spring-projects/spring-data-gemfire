/*
 * Copyright 2012 the original author or authors.
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

package org.springframework.data.gemfire.listener;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.concurrent.Executor;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.config.xml.GemfireConstants;

import com.gemstone.gemfire.cache.RegionService;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.query.QueryService;
import com.gemstone.gemfire.internal.cache.PoolManagerImpl;

/**
 * The ContinuousQueryListenerContainerTest class is a test suite of test cases testing the contract and functionality
 * of the {@link ContinuousQueryListenerContainer} class.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer
 * @since 1.8.0
 */
public class ContinuousQueryListenerContainerTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private ContinuousQueryListenerContainer listenerContainer;

	@Before
	public void setup() {
		listenerContainer = new ContinuousQueryListenerContainer();
	}

	@Test
	public void afterPropertiesSetAutoStarts() throws Exception {
		BeanFactory mockBeanFactory = mock(BeanFactory.class);
		Pool mockPool = mock(Pool.class);
		QueryService mockQueryService = mock(QueryService.class);

		when(mockBeanFactory.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME))).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class))).thenReturn(true);
		when(mockPool.getName()).thenReturn(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);
		when(mockPool.getQueryService()).thenReturn(mockQueryService);

		try {
			PoolManagerImpl.getPMI().register(mockPool);

			listenerContainer.setAutoStartup(true);
			listenerContainer.setBeanFactory(mockBeanFactory);
			listenerContainer.afterPropertiesSet();
		}
		finally {
			assertThat(PoolManagerImpl.getPMI().unregister(mockPool), is(true));
			assertThat(listenerContainer.isAutoStartup(), is(true));
			assertThat((QueryService) TestUtils.readField("queryService", listenerContainer), is(equalTo(mockQueryService)));
			assertThat(TestUtils.readField("taskExecutor", listenerContainer), is(instanceOf(Executor.class)));
			assertThat(listenerContainer.isActive(), is(true));
			assertThat(listenerContainer.isRunning(), is(true));

			verify(mockBeanFactory, times(1)).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));
			verify(mockBeanFactory, times(1)).isTypeMatch(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class));
			verify(mockBeanFactory, times(1)).getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class));
			verify(mockPool, times(2)).getName();
			verify(mockPool, times(1)).getQueryService();
			verifyZeroInteractions(mockQueryService);
		}
	}

	@Test
	public void afterPropertiesSetStartsManually() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class);
		QueryService mockQueryService = mock(QueryService.class);

		PoolManagerImpl poolManagerSpy = spy(PoolManagerImpl.getPMI());

		when(mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(true);

		listenerContainer.setAutoStartup(false);
		listenerContainer.setBeanFactory(mockBeanFactory);
		listenerContainer.setPoolName("TestPool");
		listenerContainer.setQueryService(mockQueryService);
		listenerContainer.afterPropertiesSet();

		assertThat(listenerContainer.isActive(), is(true));
		assertThat(listenerContainer.isAutoStartup(), is(false));
		assertThat(listenerContainer.isRunning(), is(false));

		verify(mockBeanFactory, times(1)).isTypeMatch(eq("TestPool"), eq(Pool.class));
		verify(mockBeanFactory, times(1)).getBean(eq("TestPool"), eq(Pool.class));
		verify(poolManagerSpy, never()).find(anyString());
		verifyZeroInteractions(mockQueryService);
	}

	@Test
	public void afterPropertiesSetThrowsIllegalStateExceptionWhenQueryServicesIsUninitialized() {
		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("QueryService was not properly initialized");

		try {
			listenerContainer.setPoolName("TestPool");
			listenerContainer.afterPropertiesSet();
		}
		finally {
			assertThat(listenerContainer.isActive(), is(false));
			assertThat(listenerContainer.isAutoStartup(), is(true));
			assertThat(listenerContainer.isRunning(), is(false));
		}
	}

	@Test
	public void resolvesToProvidedPoolName() {
		listenerContainer.setPoolName("TestPool");
		assertThat(listenerContainer.resolvePoolName(), is(equalTo("TestPool")));
	}

	@Test
	public void resolvesToGemFireDefaultPoolName() {
		listenerContainer.setPoolName(null);
		assertThat(listenerContainer.resolvePoolName(), is(equalTo(GemfireUtils.DEFAULT_POOL_NAME)));
	}

	@Test
	public void resolvesToGemFireDefaultPoolNameWhenBeanFactoryDoesNotContainNamedPool() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		when(mockBeanFactory.containsBean(anyString())).thenReturn(false);

		listenerContainer.setBeanFactory(mockBeanFactory);
		listenerContainer.setPoolName(null);

		assertThat(listenerContainer.resolvePoolName(), is(equalTo(GemfireUtils.DEFAULT_POOL_NAME)));

		verify(mockBeanFactory, times(1)).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));
	}

	@Test
	public void resolvesToSpringDataGemFireDefaultPoolName() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		when(mockBeanFactory.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME))).thenReturn(true);

		listenerContainer.setBeanFactory(mockBeanFactory);
		listenerContainer.setPoolName(null);

		assertThat(listenerContainer.resolvePoolName(), is(equalTo(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME)));

		verify(mockBeanFactory, times(1)).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));
	}

	@Test
	public void eagerlyInitializesNamedPool() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		when(mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(true);

		listenerContainer.setBeanFactory(mockBeanFactory);

		assertThat(listenerContainer.eagerlyInitializePool("TestPool"), is(equalTo("TestPool")));

		verify(mockBeanFactory, times(1)).isTypeMatch(eq("TestPool"), eq(Pool.class));
		verify(mockBeanFactory, times(1)).getBean(eq("TestPool"), eq(Pool.class));
	}

	@Test
	public void eagerlyInitializePoolFindsRegisteredPoolByNameInGemFire() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class);
		Pool mockPool = mock(Pool.class);

		when(mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("TestPool"), eq(Pool.class))).thenThrow(
			new NoSuchBeanDefinitionException("test"));
		when(mockPool.getName()).thenReturn("TestPool");

		listenerContainer.setBeanFactory(mockBeanFactory);

		try {
			PoolManagerImpl.getPMI().register(mockPool);
			assertThat(listenerContainer.eagerlyInitializePool("TestPool"), is(equalTo("TestPool")));
		}
		finally {
			assertThat(PoolManagerImpl.getPMI().unregister(mockPool), is(true));
			verify(mockPool, times(2)).getName();
		}
	}

	@Test
	public void eagerlyInitializePoolThrowsIllegalArgumentExceptionCausedByNoSuchBeanDefinitionException() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		when(mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("TestPool"), eq(Pool.class))).thenThrow(
			new NoSuchBeanDefinitionException("test"));

		try {
			exception.expect(IllegalArgumentException.class);
			exception.expectCause(is(nullValue(Throwable.class)));
			exception.expectMessage("No GemFire Pool with name [TestPool] was found");

			listenerContainer.setBeanFactory(mockBeanFactory);
			listenerContainer.eagerlyInitializePool("TestPool");
		}
		finally {
			verify(mockBeanFactory, times(1)).isTypeMatch(eq("TestPool"), eq(Pool.class));
			verify(mockBeanFactory, times(1)).getBean(eq("TestPool"), eq(Pool.class));
		}
	}

	@Test
	public void initQueryServiceReturnsProvidedQueryService() {
		QueryService mockQueryService = mock(QueryService.class);

		listenerContainer.setQueryService(mockQueryService);

		assertThat(listenerContainer.initQueryService(null), is(sameInstance(mockQueryService)));

		verifyZeroInteractions(mockQueryService);
	}

	@Test
	public void initializesQueryServiceFromContainer() {
		QueryService mockQueryService = mock(QueryService.class);

		listenerContainer.setQueryService(mockQueryService);

		assertThat(listenerContainer.initQueryService("TestPool"), is(equalTo(mockQueryService)));

		verifyZeroInteractions(mockQueryService);
	}

	@Test
	public void initializesQueryServiceFromPool() {
		Pool mockPool = mock(Pool.class);
		QueryService mockQueryService = mock(QueryService.class);

		when(mockPool.getName()).thenReturn("TestPool");
		when(mockPool.getQueryService()).thenReturn(mockQueryService);

		try {
			PoolManagerImpl.getPMI().register(mockPool);
			listenerContainer.setQueryService(null);
			assertThat(listenerContainer.initQueryService("TestPool"), is(equalTo(mockQueryService)));
		}
		finally {
			assertThat(PoolManagerImpl.getPMI().unregister(mockPool), is(true));
			verify(mockPool, times(2)).getName();
			verify(mockPool, times(1)).getQueryService();
			verifyZeroInteractions(mockQueryService);
		}
	}

	@Test
	public void initializesQueryServiceFromPoolInThePresenceOfAProvidedQueryService() {
		Pool mockPool = mock(Pool.class);

		QueryService mockQueryServiceOne = mock(QueryService.class);
		QueryService mockQueryServiceTwo = mock(QueryService.class);

		when(mockPool.getName()).thenReturn("TestPool");
		when(mockPool.getQueryService()).thenReturn(mockQueryServiceOne);

		try {
			PoolManagerImpl.getPMI().register(mockPool);
			listenerContainer.setQueryService(mockQueryServiceTwo);
			assertThat(listenerContainer.initQueryService("TestPool"), is(equalTo(mockQueryServiceOne)));
		}
		finally {
			assertThat(PoolManagerImpl.getPMI().unregister(mockPool), is(true));
			verify(mockPool, times(2)).getName();
			verify(mockPool, times(1)).getQueryService();
			verifyZeroInteractions(mockQueryServiceOne);
			verifyZeroInteractions(mockQueryServiceTwo);
		}
	}

	@Test
	public void initExecutorReturnsProvidedExecutor() {
		Executor mockExecutor = mock(Executor.class);

		listenerContainer.setTaskExecutor(mockExecutor);

		assertThat(listenerContainer.initExecutor(), is(sameInstance(mockExecutor)));

		verifyZeroInteractions(mockExecutor);
	}

	@Test
	public void initializesDefaultTaskExecutor() {
		assertThat(listenerContainer.initExecutor(), is(instanceOf(Executor.class)));
	}

	@Test
	public void setCacheSetsQueryService() {
		QueryService mockQueryService = mock(QueryService.class);
		RegionService mockRegionService = mock(RegionService.class);

		when(mockRegionService.getQueryService()).thenReturn(mockQueryService);

		listenerContainer.setCache(mockRegionService);

		assertThat(listenerContainer.initQueryService(null), is(equalTo(mockQueryService)));

		verify(mockRegionService, times(1)).getQueryService();
		verifyZeroInteractions(mockQueryService);
	}

	@Test
	public void setAndGetAutoStartup() {
		assertThat(listenerContainer.isAutoStartup(), is(true));

		listenerContainer.setAutoStartup(false);

		assertThat(listenerContainer.isAutoStartup(), is(false));

		listenerContainer.setAutoStartup(true);

		assertThat(listenerContainer.isAutoStartup(), is(true));
	}

}
