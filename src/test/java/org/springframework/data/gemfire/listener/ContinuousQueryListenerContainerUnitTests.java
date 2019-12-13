/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.springframework.data.gemfire.listener;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executor;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.query.CqAttributes;
import org.apache.geode.cache.query.CqEvent;
import org.apache.geode.cache.query.CqException;
import org.apache.geode.cache.query.CqQuery;
import org.apache.geode.cache.query.CqState;
import org.apache.geode.cache.query.QueryException;
import org.apache.geode.cache.query.QueryService;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.data.gemfire.GemfireQueryException;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.client.PoolResolver;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.util.ErrorHandler;

/**
 * Unit Tests for {@link ContinuousQueryListenerContainer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer
 * @since 1.8.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ContinuousQueryListenerContainerUnitTests {

	@Mock
	private BeanFactory mockBeanFactory;

	private ContinuousQueryListenerContainer cqListenerContainer;

	@Before
	public void setup() {
		this.cqListenerContainer = spy(new ContinuousQueryListenerContainer());
	}

	private CqQuery mockCqQuery(String name, String query, CqAttributes attributes, boolean durable) {

		CqQuery mockQuery = mock(CqQuery.class);

		when(mockQuery.getName()).thenReturn(name);
		when(mockQuery.getQueryString()).thenReturn(query);
		when(mockQuery.getCqAttributes()).thenReturn(attributes);
		when(mockQuery.isDurable()).thenReturn(durable);

		return mockQuery;
	}

	@Test
	public void afterPropertiesSetIsAutoStart() {

		Pool mockPool = mock(Pool.class);

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		QueryService mockQueryService = mock(QueryService.class);

		when(this.mockBeanFactory.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME))).thenReturn(true);
		when(this.mockBeanFactory.isTypeMatch(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class))).thenReturn(true);
		when(this.mockBeanFactory.getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class))).thenReturn(mockPool);
		when(mockPoolResolver.resolve(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME))).thenReturn(mockPool);
		when(mockPool.getQueryService()).thenReturn(mockQueryService);

		this.cqListenerContainer.setAutoStartup(true);
		this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);
		this.cqListenerContainer.setPoolResolver(mockPoolResolver);

		try {
			this.cqListenerContainer.afterPropertiesSet();
		}
		finally {

			assertThat(this.cqListenerContainer.isActive()).isTrue();
			assertThat(this.cqListenerContainer.isAutoStartup()).isTrue();
			assertThat(this.cqListenerContainer.isRunning()).isFalse();
			assertThat(this.cqListenerContainer.getPoolResolver()).isEqualTo(mockPoolResolver);
			assertThat(this.cqListenerContainer.getQueryService()).isEqualTo(mockQueryService);
			assertThat(this.cqListenerContainer.getTaskExecutor()).isInstanceOf(Executor.class);

			verify(this.mockBeanFactory, times(2)).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));
			verify(this.mockBeanFactory, times(2)).isTypeMatch(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class));
			verify(this.mockBeanFactory, times(1)).getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class));
			verify(mockPoolResolver, times(1)).resolve(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));
			verify(mockPool, times(1)).getQueryService();
			verifyZeroInteractions(mockQueryService);
		}
	}

	@Test
	public void afterPropertiesSetIsManualStart() {

		Executor mockExecutor = mock(Executor.class);

		Pool mockPool = mock(Pool.class);

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		QueryService mockQueryService = mock(QueryService.class);

		when(this.mockBeanFactory.containsBean(eq("TestPool"))).thenReturn(true);
		when(this.mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(true);
		when(mockPoolResolver.resolve(eq("TestPool"))).thenReturn(mockPool);

		this.cqListenerContainer.setAutoStartup(false);
		this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);
		this.cqListenerContainer.setPoolName("TestPool");
		this.cqListenerContainer.setPoolResolver(mockPoolResolver);
		this.cqListenerContainer.setQueryService(mockQueryService);
		this.cqListenerContainer.setTaskExecutor(mockExecutor);

		try {
			this.cqListenerContainer.afterPropertiesSet();
		}
		finally {

			assertThat(this.cqListenerContainer.isActive()).isTrue();
			assertThat(this.cqListenerContainer.isAutoStartup()).isFalse();
			assertThat(this.cqListenerContainer.isRunning()).isFalse();
			assertThat(this.cqListenerContainer.getBeanFactory()).isSameAs(this.mockBeanFactory);
			assertThat(this.cqListenerContainer.getPoolName()).isEqualTo("TestPool");
			assertThat(this.cqListenerContainer.getPoolResolver()).isEqualTo(mockPoolResolver);
			assertThat(this.cqListenerContainer.getQueryService()).isSameAs(mockQueryService);
			assertThat(this.cqListenerContainer.getTaskExecutor()).isSameAs(mockExecutor);

			verify(this.mockBeanFactory, times(1)).containsBean(eq("TestPool"));
			verify(this.mockBeanFactory, times(1)).isTypeMatch(eq("TestPool"), eq(Pool.class));
			verify(this.mockBeanFactory, times(1)).getBean(eq("TestPool"), eq(Pool.class));
			verify(mockPoolResolver, times(1)).resolve(eq("TestPool"));
			verify(mockPool, times(1)).getQueryService();
			verifyZeroInteractions(mockQueryService);
		}
	}

	@Test(expected = IllegalStateException.class)
	public void afterPropertiesSetThrowsIllegalStateExceptionWhenQueryServiceIsUninitialized() {

		when(this.mockBeanFactory.containsBean(eq("TestPoolZero"))).thenReturn(true);
		when(this.mockBeanFactory.isTypeMatch(eq("TestPoolZero"), eq(Pool.class))).thenReturn(true);

		try {
			this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);
			this.cqListenerContainer.setPoolName("TestPoolZero");
			this.cqListenerContainer.afterPropertiesSet();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage("QueryService is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {

			assertThat(cqListenerContainer.isActive()).isFalse();
			assertThat(cqListenerContainer.isAutoStartup()).isTrue();
			assertThat(cqListenerContainer.isRunning()).isFalse();

			verify(mockBeanFactory, times(1)).containsBean(eq("TestPoolZero"));
			verify(mockBeanFactory, times(1)).isTypeMatch(eq("TestPoolZero"), eq(Pool.class));
			verify(mockBeanFactory, times(1)).getBean(eq("TestPoolZero"), eq(Pool.class));
		}
	}

	@Test
	public void setAndGetPoolResolver() {

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		assertThat(this.cqListenerContainer.getPoolResolver())
			.isEqualTo(ContinuousQueryListenerContainer.DEFAULT_POOL_RESOLVER);

		this.cqListenerContainer.setPoolResolver(mockPoolResolver);

		assertThat(this.cqListenerContainer.getPoolResolver()).isEqualTo(mockPoolResolver);

		this.cqListenerContainer.setPoolResolver(null);

		assertThat(this.cqListenerContainer.getPoolResolver())
			.isEqualTo(ContinuousQueryListenerContainer.DEFAULT_POOL_RESOLVER);

		verifyZeroInteractions(mockPoolResolver);
	}

	@Test
	public void resolvePoolCallsPoolResolverResolveReturnsNull() {

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		when(mockPoolResolver.resolve(anyString())).thenReturn(null);

		this.cqListenerContainer.setPoolResolver(mockPoolResolver);

		assertThat(this.cqListenerContainer.resolvePool("TestPool")).isNull();

		verify(mockPoolResolver, times(1)).resolve(eq("TestPool"));
	}

	@Test
	public void resolvePoolCallsPoolResolverResolveReturnsPool() {

		Pool mockPool = mock(Pool.class);

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		when(mockPoolResolver.resolve(eq("TestPool"))).thenReturn(mockPool);

		this.cqListenerContainer.setPoolResolver(mockPoolResolver);

		assertThat(this.cqListenerContainer.resolvePool("TestPool")).isEqualTo(mockPool);

		verify(mockPoolResolver, times(1)).resolve(eq("TestPool"));
		verifyZeroInteractions(mockPool);
	}

	@Test
	public void resolvePoolNameReturnsConfiguredPoolName() {

		this.cqListenerContainer.setPoolName("TestPool");

		assertThat(this.cqListenerContainer.resolvePoolName()).isEqualTo("TestPool");
	}

	@Test
	public void resolvePoolNameReturnsApacheGeodeDefaultPoolName() {

		this.cqListenerContainer.setPoolName(null);

		assertThat(this.cqListenerContainer.resolvePoolName()).isEqualTo(GemfireUtils.DEFAULT_POOL_NAME);
	}

	@Test
	public void resolvePoolNameReturnsApacheGeodeDefaultPoolNameWhenBeanFactoryDoesNotContainNamedPool() {

		when(this.mockBeanFactory.containsBean(anyString())).thenReturn(false);

		this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);
		this.cqListenerContainer.setPoolName("");

		assertThat(this.cqListenerContainer.resolvePoolName()).isEqualTo(GemfireUtils.DEFAULT_POOL_NAME);

		verify(this.mockBeanFactory, times(1))
			.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));
		verify(this.mockBeanFactory, never()).isTypeMatch(anyString(), any(Class.class));
	}

	@Test
	public void resolvePoolNameReturnsApacheGeodeDefaultPoolNameWhenBeanFactoryContainsBeanWithNonMatchingPoolType() {

		when(this.mockBeanFactory.containsBean(anyString())).thenReturn(true);
		when(this.mockBeanFactory.isTypeMatch(anyString(), eq(Pool.class))).thenReturn(false);

		this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);
		this.cqListenerContainer.setPoolName("");

		assertThat(this.cqListenerContainer.resolvePoolName()).isEqualTo(GemfireUtils.DEFAULT_POOL_NAME);

		verify(this.mockBeanFactory, times(1))
			.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));
		verify(this.mockBeanFactory, times(1))
			.isTypeMatch(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), any(Class.class));
	}

	@Test
	public void resolvePoolNameReturnsSpringDataGemFireDefaultPoolName() {

		when(this.mockBeanFactory.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME))).thenReturn(true);
		when(this.mockBeanFactory.isTypeMatch(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class))).thenReturn(true);

		this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);
		this.cqListenerContainer.setPoolName("  ");

		assertThat(this.cqListenerContainer.resolvePoolName()).isEqualTo(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);

		verify(this.mockBeanFactory, times(1))
			.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));
		verify(this.mockBeanFactory, times(1))
			.isTypeMatch(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class));
	}

	@Test
	public void eagerlyInitializePoolWithGivenPoolName() {

		when(this.mockBeanFactory.containsBean(eq("TestPool"))).thenReturn(true);
		when(this.mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(true);

		this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);

		assertThat(this.cqListenerContainer.eagerlyInitializePool("TestPool")).isEqualTo("TestPool");

		verify(this.mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(this.mockBeanFactory, times(1)).isTypeMatch(eq("TestPool"), eq(Pool.class));
		verify(this.mockBeanFactory, times(1)).getBean(eq("TestPool"), eq(Pool.class));
	}

	@Test
	public void eagerlyInitializePoolFindsRegisteredPoolWithTheGivenName() {

		Pool mockPool = mock(Pool.class);

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		when(this.mockBeanFactory.containsBean(eq("TestPoolOne"))).thenReturn(false);
		when(mockPoolResolver.resolve(eq("TestPoolOne"))).thenReturn(mockPool);

		try {

			this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);
			this.cqListenerContainer.setPoolResolver(mockPoolResolver);

			assertThat(this.cqListenerContainer.eagerlyInitializePool("TestPoolOne"))
				.isEqualTo("TestPoolOne");
		}
		finally {

			verify(this.mockBeanFactory, times(1)).containsBean(eq("TestPoolOne"));
			verify(this.mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
			verify(this.mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
			verify(mockPoolResolver, times(1)).resolve(eq("TestPoolOne"));
			verifyZeroInteractions(mockPool);
		}
	}

	@Test
	public void eagerlyInitializePoolFindsRegisteredPoolWithTheGivenNameWhenBeanFactoryGetBeanThrowsBeansException() {

		Pool mockPool = mock(Pool.class);

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		when(this.mockBeanFactory.containsBean(eq("TestPoolTwo"))).thenReturn(true);
		when(this.mockBeanFactory.isTypeMatch(eq("TestPoolTwo"), eq(Pool.class))).thenReturn(true);
		when(this.mockBeanFactory.getBean(eq("TestPoolTwo"), eq(Pool.class)))
			.thenThrow(new NoSuchBeanDefinitionException("TEST"));
		when(mockPoolResolver.resolve(eq("TestPoolTwo"))).thenReturn(mockPool);

		this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);
		this.cqListenerContainer.setPoolResolver(mockPoolResolver);

		try {
			assertThat(this.cqListenerContainer.eagerlyInitializePool("TestPoolTwo"))
				.isEqualTo("TestPoolTwo");
		}
		finally {

			verify(this.mockBeanFactory, times(1)).containsBean(eq("TestPoolTwo"));
			verify(this.mockBeanFactory, times(1)).isTypeMatch(eq("TestPoolTwo"), eq(Pool.class));
			verify(this.mockBeanFactory, times(1)).getBean(eq("TestPoolTwo"), eq(Pool.class));
			verify(mockPoolResolver, times(1)).resolve(eq("TestPoolTwo"));
			verifyZeroInteractions(mockPool);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void eagerlyInitializePoolThrowsIllegalArgumentExceptionCausedByNoPoolWithGivenName() {

		when(this.mockBeanFactory.containsBean(anyString())).thenReturn(false);

		try {
			this.cqListenerContainer.setBeanFactory(this.mockBeanFactory);
			this.cqListenerContainer.eagerlyInitializePool("TestPoolThree");
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("No Pool with name [TestPoolThree] was found");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {

			verify(mockBeanFactory, times(1)).containsBean(eq("TestPoolThree"));
			verify(mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
			verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		}
	}

	@Test
	public void initQueryServiceReturnsConfiguredQueryService() {

		QueryService mockQueryService = mock(QueryService.class);

		cqListenerContainer.setQueryService(mockQueryService);

		assertThat(cqListenerContainer.initQueryService("TestPool")).isSameAs(mockQueryService);

		verifyZeroInteractions(mockQueryService);
	}

	@Test
	public void initQueryServiceReturnsNull() {
		assertThat(cqListenerContainer.initQueryService(null)).isNull();
	}

	@Test
	public void initializesQueryServiceFromPool() {

		Pool mockPool = mock(Pool.class);

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		QueryService mockQueryService = mock(QueryService.class);

		when(mockPoolResolver.resolve(eq("TestPoolFour"))).thenReturn(mockPool);
		when(mockPool.getQueryService()).thenReturn(mockQueryService);

		this.cqListenerContainer.setPoolResolver(mockPoolResolver);

		try {

			assertThat(this.cqListenerContainer.getQueryService()).isNull();
			assertThat(this.cqListenerContainer.initQueryService("TestPoolFour")).isEqualTo(mockQueryService);
		}
		finally {

			verify(mockPoolResolver, times(1)).resolve(eq("TestPoolFour"));
			verify(mockPool, times(1)).getQueryService();
			verifyZeroInteractions(mockQueryService);
		}
	}

	@Test
	public void initializesQueryServiceFromPoolIgnoresConfiguredQueryService() {

		Pool mockPool = mock(Pool.class);

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		QueryService mockQueryServiceOne = mock(QueryService.class);
		QueryService mockQueryServiceTwo = mock(QueryService.class);

		when(mockPoolResolver.resolve(eq("TestPoolFive"))).thenReturn(mockPool);
		when(mockPool.getQueryService()).thenReturn(mockQueryServiceOne);

		this.cqListenerContainer.setPoolResolver(mockPoolResolver);
		this.cqListenerContainer.setQueryService(mockQueryServiceTwo);

		try {

			assertThat(this.cqListenerContainer.getPoolResolver()).isSameAs(mockPoolResolver);
			assertThat(this.cqListenerContainer.getQueryService()).isSameAs(mockQueryServiceTwo);
			assertThat(this.cqListenerContainer.initQueryService("TestPoolFive"))
				.isEqualTo(mockQueryServiceOne);
		}
		finally {

			verify(mockPoolResolver, times(1)).resolve(eq("TestPoolFive"));
			verify(mockPool, times(1)).getQueryService();
			verifyZeroInteractions(mockQueryServiceOne);
			verifyZeroInteractions(mockQueryServiceTwo);
		}
	}

	@Test
	public void initExecutorReturnsProvidedExecutor() {

		Executor mockExecutor = mock(Executor.class);

		cqListenerContainer.setTaskExecutor(mockExecutor);

		assertThat(cqListenerContainer.initExecutor()).isSameAs(mockExecutor);

		verifyZeroInteractions(mockExecutor);
	}

	@Test
	public void initializesDefaultTaskExecutor() {
		assertThat(cqListenerContainer.getTaskExecutor()).isNull();
		assertThat(cqListenerContainer.initExecutor()).isInstanceOf(Executor.class);
	}

	@Test
	public void setAndGetAutoStartup() {

		assertThat(cqListenerContainer.isAutoStartup()).isTrue();

		cqListenerContainer.setAutoStartup(false);

		assertThat(cqListenerContainer.isAutoStartup()).isFalse();

		cqListenerContainer.setAutoStartup(true);

		assertThat(cqListenerContainer.isAutoStartup()).isTrue();
	}

	@Test
	public void setCacheSetsQueryService() {

		QueryService mockQueryService = mock(QueryService.class);

		RegionService mockRegionService = mock(RegionService.class);

		when(mockRegionService.getQueryService()).thenReturn(mockQueryService);

		cqListenerContainer.setCache(mockRegionService);

		assertThat(cqListenerContainer.initQueryService(null)).isEqualTo(mockQueryService);

		verify(mockRegionService, times(1)).getQueryService();
		verifyZeroInteractions(mockQueryService);
	}

	@Test
	public void addListenerCreatesCqQueryAndExecutesQueryWhenRunning() throws Exception {

		ContinuousQueryListener mockListener = mock(ContinuousQueryListener.class);

		ContinuousQueryDefinition definition =
			new ContinuousQueryDefinition("TestQuery", "SELECT * FROM /Utilization u WHERE u.value > 100", mockListener);

		CqQuery mockQuery = mock(CqQuery.class);

		doReturn(mockQuery).when(cqListenerContainer).addContinuousQuery(eq(definition));
		when(cqListenerContainer.isRunning()).thenReturn(true);

		cqListenerContainer.addListener(definition);

		verify(cqListenerContainer, times(1)).addContinuousQuery(eq(definition));
		verify(cqListenerContainer, times(1)).isRunning();
		verify(mockQuery, times(1)).execute();
	}

	@Test(expected = GemfireQueryException.class)
	public void addContinuousQueryThrowsQueryException() throws Exception {

		QueryService mockQueryService = mock(QueryService.class);

		when(mockQueryService.newCq(anyString(), any(CqAttributes.class), anyBoolean()))
			.thenThrow(new CqException("TEST"));

		ContinuousQueryListener mockListener = mock(ContinuousQueryListener.class);

		ContinuousQueryDefinition definition =
			new ContinuousQueryDefinition("SELECT * FROM /Utilization u WHERE u.value > 100",
				mockListener, false);

		cqListenerContainer.setQueryService(mockQueryService);

		try {
			cqListenerContainer.addContinuousQuery(definition);
		}
		catch (GemfireQueryException expected) {

			assertThat(expected).hasMessageStartingWith("Unable to create query [SELECT * FROM /Utilization u WHERE u.value > 100]");
			assertThat(expected).hasCauseInstanceOf(QueryException.class);
			assertThat(expected.getCause()).hasMessage("TEST");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;

		}
		finally {
			assertThat(cqListenerContainer.getContinuousQueries()).isEmpty();
		}
	}

	@Test
	public void addManagedNamedContinuousQuery() throws Exception {

		QueryService mockQueryService = mock(QueryService.class);

		when(mockQueryService.newCq(anyString(), anyString(), any(CqAttributes.class), anyBoolean()))
			.thenAnswer(invocation -> mockCqQuery(invocation.getArgument(0), invocation.getArgument(1),
				invocation.getArgument(2), invocation.getArgument(3)));

		ContinuousQueryListener mockListener = mock(ContinuousQueryListener.class);

		ContinuousQueryDefinition definition =
			new ContinuousQueryDefinition("TestQuery", "SELECT * FROM /Utilization u WHERE u.value > 100",
				mockListener, true);

		cqListenerContainer.setQueryService(mockQueryService);

		CqQuery query = cqListenerContainer.addContinuousQuery(definition);

		assertThat(query).isNotNull();
		assertThat(query.isDurable()).isTrue();
		assertThat(query.getName()).isEqualTo("TestQuery");
		assertThat(query.getQueryString()).isEqualTo("SELECT * FROM /Utilization u WHERE u.value > 100");
		assertThat(query.isRunning()).isFalse();

		CqAttributes attributes = query.getCqAttributes();

		assertThat(attributes).isNotNull();
		assertThat(attributes.getCqListener()).isInstanceOf(ContinuousQueryListenerContainer.EventDispatcherAdapter.class);

		ContinuousQueryListenerContainer.EventDispatcherAdapter eventDispatcherAdapter =
			(ContinuousQueryListenerContainer.EventDispatcherAdapter) attributes.getCqListener();

		assertThat(eventDispatcherAdapter.getListener()).isEqualTo(mockListener);
		assertThat(cqListenerContainer.getContinuousQueries().peek()).isEqualTo(query);
	}

	@Test
	public void addManagedUnnamedContinuousQuery() throws Exception {

		QueryService mockQueryService = mock(QueryService.class);

		when(mockQueryService.newCq(anyString(), any(CqAttributes.class), anyBoolean()))
			.thenAnswer(invocation -> mockCqQuery(null, invocation.getArgument(0), invocation.getArgument(1),
				invocation.getArgument(2)));

		ContinuousQueryListener mockListener = mock(ContinuousQueryListener.class);

		ContinuousQueryDefinition definition =
			new ContinuousQueryDefinition("SELECT * FROM /Utilization u WHERE u.value > 100",
				mockListener, false);

		cqListenerContainer.setQueryService(mockQueryService);

		CqQuery query = cqListenerContainer.addContinuousQuery(definition);

		assertThat(query).isNotNull();
		assertThat(query.isDurable()).isFalse();
		assertThat(query.getName()).isNull();
		assertThat(query.getQueryString()).isEqualTo("SELECT * FROM /Utilization u WHERE u.value > 100");
		assertThat(query.isRunning()).isFalse();

		CqAttributes attributes = query.getCqAttributes();

		assertThat(attributes).isNotNull();
		assertThat(attributes.getCqListener()).isInstanceOf(ContinuousQueryListenerContainer.EventDispatcherAdapter.class);

		ContinuousQueryListenerContainer.EventDispatcherAdapter eventDispatcherAdapter =
			(ContinuousQueryListenerContainer.EventDispatcherAdapter) attributes.getCqListener();

		assertThat(eventDispatcherAdapter.getListener()).isEqualTo(mockListener);
		assertThat(cqListenerContainer.getContinuousQueries().peek()).isEqualTo(query);
	}

	@Test
	public void cqListenerContainerStartsWhenNotRunning() throws Exception {

		CqQuery mockQueryOne = mock(CqQuery.class);
		CqQuery mockQueryTwo = mock(CqQuery.class);

		cqListenerContainer.getContinuousQueries().add(mockQueryOne);
		cqListenerContainer.getContinuousQueries().add(mockQueryTwo);

		assertThat(cqListenerContainer.isRunning()).isFalse();

		cqListenerContainer.start();

		assertThat(cqListenerContainer.isRunning()).isTrue();

		verify(mockQueryOne, times(1)).execute();
		verify(mockQueryTwo, times(1)).execute();
	}

	@Test(expected = GemfireQueryException.class)
	public void cqListenerContainerStartHandlesCqException() throws Exception {

		CqQuery mockQueryOne = mock(CqQuery.class);
		CqQuery mockQueryTwo = mock(CqQuery.class);

		CqState mockQueryState = mock(CqState.class);

		cqListenerContainer.getContinuousQueries().add(mockQueryOne);
		cqListenerContainer.getContinuousQueries().add(mockQueryTwo);

		when(mockQueryOne.getName()).thenReturn("ONE");
		when(mockQueryOne.getState()).thenReturn(mockQueryState);
		when(mockQueryState.toString()).thenReturn("FAILED");
		doThrow(new CqException("ONE")).when(mockQueryOne).execute();

		try {
			cqListenerContainer.start();
		}
		catch (GemfireQueryException cause) {

			assertThat(cause).hasMessageStartingWith("Could not execute query [ONE]; state is [FAILED]");
			assertThat(cause).hasCauseInstanceOf(CqException.class);
			assertThat(cause.getCause()).hasMessage("ONE");
			assertThat(cause.getCause()).hasNoCause();

			throw cause;
		}
		finally {
			assertThat(cqListenerContainer.isRunning()).isFalse();

			verify(mockQueryOne, times(1)).execute();
			verify(mockQueryOne, times(1)).getName();
			verify(mockQueryOne, times(1)).getState();
			verify(mockQueryTwo, never()).execute();
		}
	}

	@Test
	public void cqListenerContainerDoesNotStartWhenAlreadyRunning() {

		when(cqListenerContainer.isRunning()).thenReturn(true);

		cqListenerContainer.start();

		verify(cqListenerContainer, never()).doStart();
	}

	@Test
	public void dispatchEventNotifiesListenerOfCqEvent() {

		Executor mockExecutor = mock(Executor.class);

		doAnswer(invocation -> {
			invocation.<Runnable>getArgument(0).run();
			return null;
		}).when(mockExecutor).execute(any());

		ContinuousQueryListener mockListener = mock(ContinuousQueryListener.class);

		CqEvent mockEvent = mock(CqEvent.class);

		cqListenerContainer.setTaskExecutor(mockExecutor);
		cqListenerContainer.dispatchEvent(mockListener, mockEvent);

		verify(mockExecutor, times(1)).execute(isA(Runnable.class));
		verify(mockListener, times(1)).onEvent(eq(mockEvent));
	}

	@Test
	public void dispatchEventInvokesConfiguredErrorHandlerOnListenerException() {

		RuntimeException expectedCause = new RuntimeException("TEST");

		Executor mockExecutor = mock(Executor.class);

		doAnswer(invocation -> {
			invocation.<Runnable>getArgument(0).run();
			return null;
		}).when(mockExecutor).execute(any());

		ErrorHandler mockErrorHandler = mock(ErrorHandler.class);

		ContinuousQueryListener mockListener = mock(ContinuousQueryListener.class);

		doThrow(expectedCause).when(mockListener).onEvent(any());

		CqEvent mockEvent = mock(CqEvent.class);

		cqListenerContainer.setErrorHandler(mockErrorHandler);
		cqListenerContainer.setTaskExecutor(mockExecutor);

		doReturn(true).when(cqListenerContainer).isActive();

		cqListenerContainer.dispatchEvent(mockListener, mockEvent);

		verify(mockExecutor, times(1)).execute(isA(Runnable.class));
		verify(mockListener, times(1)).onEvent(eq(mockEvent));
		verify(mockErrorHandler, times(1)).handleError(eq(expectedCause));
	}

	@Test
	public void stopStopsCqsCallsRunnableHandlesExceptionsOnCqQueryStopWhenRunning() throws Exception {

		CqQuery mockQueryOne = mock(CqQuery.class);
		CqQuery mockQueryTwo = mock(CqQuery.class);
		CqQuery mockQueryThree = mock(CqQuery.class);

		doThrow(new CqException("TWO")).when(mockQueryTwo).stop();

		Runnable mockRunnable = mock(Runnable.class);

		cqListenerContainer.getContinuousQueries().add(mockQueryOne);
		cqListenerContainer.getContinuousQueries().add(mockQueryTwo);
		cqListenerContainer.getContinuousQueries().add(mockQueryThree);

		doReturn(true).when(cqListenerContainer).isRunning();

		cqListenerContainer.stop(mockRunnable);

		verify(cqListenerContainer, times(1)).stop();
		verify(mockQueryOne, times(1)).stop();
		verify(mockQueryTwo, times(1)).stop();
		verify(mockQueryThree, times(1)).stop();
		verify(mockRunnable, times(1)).run();
	}

	@Test
	public void stopDoesNothingWhenContainerIsNotRunning() {

		when(cqListenerContainer.isRunning()).thenReturn(false);

		cqListenerContainer.stop();

		verify(cqListenerContainer, never()).doStop();
	}

	@Test
	public void destroyIsSuccessful() throws Exception {

		CqQuery mockQueryOne = mock(CqQuery.class);
		CqQuery mockQueryTwo = mock(CqQuery.class);
		CqQuery mockQueryThree = mock(CqQuery.class);
		CqQuery mockQueryFour = mock(CqQuery.class);

		when(mockQueryTwo.isClosed()).thenReturn(true);
		doThrow(new RuntimeException("THREE")).when(mockQueryThree).close();

		List<CqQuery> queries = Arrays.asList(mockQueryOne, mockQueryTwo, mockQueryThree, mockQueryFour);

		cqListenerContainer.getContinuousQueries().addAll(queries);

		DisposableExecutorBean mockDisposableExecutorBean = mock(DisposableExecutorBean.class);

		doReturn(mockDisposableExecutorBean).when(cqListenerContainer).createDefaultTaskExecutor();

		assertThat(cqListenerContainer.getContinuousQueries()).hasSize(queries.size());

		cqListenerContainer.initExecutor();
		cqListenerContainer.destroy();

		assertThat(cqListenerContainer.getContinuousQueries()).isEmpty();

		verify(mockQueryOne, times(1)).isClosed();
		verify(mockQueryOne, times(1)).close();
		verify(mockQueryTwo, times(1)).isClosed();
		verify(mockQueryTwo, never()).close();
		verify(mockQueryThree, times(1)).isClosed();
		verify(mockQueryThree, times(1)).close();
		verify(mockQueryFour, times(1)).isClosed();
		verify(mockQueryFour, times(1)).close();
		verify(mockDisposableExecutorBean, times(1)).destroy();
	}

	interface DisposableExecutorBean extends Executor, DisposableBean { }

}
