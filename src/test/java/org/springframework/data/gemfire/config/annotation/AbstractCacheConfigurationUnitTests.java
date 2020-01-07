/*
 * Copyright 2016-2020 the original author or authors.
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
package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration.DEFAULT_MCAST_PORT;

import java.util.Collections;
import java.util.List;
import java.util.Properties;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;

import org.apache.geode.cache.TransactionListener;
import org.apache.geode.cache.TransactionWriter;
import org.apache.geode.cache.util.GatewayConflictResolver;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.CacheFactoryBean;

/**
 * Unit Tests for {@link AbstractCacheConfiguration}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration
 * @since 1.9.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractCacheConfigurationUnitTests {

	@Spy
	private AbstractCacheConfiguration cacheConfiguration;

	@Test
	public void gemfirePropertiesContainsEssentialProperties() {

		this.cacheConfiguration.setName("TestName");
		this.cacheConfiguration.setMcastPort(-1);
		this.cacheConfiguration.setLogLevel("DEBUG");
		this.cacheConfiguration.setLocators("skullbox[11235]");
		this.cacheConfiguration.setStartLocator("boombox[12480]");

		Properties gemfireProperties = this.cacheConfiguration.gemfireProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties).hasSize(5);
		assertThat(gemfireProperties.getProperty("name")).isEqualTo("TestName");
		assertThat(gemfireProperties.getProperty("mcast-port")).isEqualTo(String.valueOf(DEFAULT_MCAST_PORT));
		assertThat(gemfireProperties.getProperty("log-level")).isEqualTo("DEBUG");
		assertThat(gemfireProperties.getProperty("locators")).isEqualTo("skullbox[11235]");
		assertThat(gemfireProperties.getProperty("start-locator")).isEqualTo("boombox[12480]");
	}

	@Test
	public void cacheFactoryBeanConfigurationIsCorrect() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		CacheFactoryBean cacheFactoryBean = mock(CacheFactoryBean.class);

		ClassLoader testBeanClassLoader = Thread.currentThread().getContextClassLoader();

		GatewayConflictResolver mockGatewayConflictResolver = mock(GatewayConflictResolver.class);

		List<CacheFactoryBean.JndiDataSource> jndiDataSources = Collections.emptyList();
		List<TransactionListener> transactionListeners = Collections.singletonList(mock(TransactionListener.class));

		Properties gemfireProperties = new Properties();

		Resource mockResource = mock(Resource.class);

		TransactionWriter mockTransactionWriter = mock(TransactionWriter.class);

		doReturn(gemfireProperties).when(this.cacheConfiguration).gemfireProperties();

		this.cacheConfiguration.setBeanClassLoader(testBeanClassLoader);
		this.cacheConfiguration.setBeanFactory(mockBeanFactory);
		this.cacheConfiguration.setCacheXml(mockResource);
		this.cacheConfiguration.setClose(false);
		this.cacheConfiguration.setCopyOnRead(true);
		this.cacheConfiguration.setCriticalHeapPercentage(0.90f);
		this.cacheConfiguration.setCriticalOffHeapPercentage(0.85f);
		this.cacheConfiguration.setEvictionHeapPercentage(0.75f);
		this.cacheConfiguration.setEvictionOffHeapPercentage(0.55f);
		this.cacheConfiguration.setGatewayConflictResolver(mockGatewayConflictResolver);
		this.cacheConfiguration.setJndiDataSources(jndiDataSources);
		this.cacheConfiguration.setTransactionListeners(transactionListeners);
		this.cacheConfiguration.setTransactionWriter(mockTransactionWriter);
		this.cacheConfiguration.setUseBeanFactoryLocator(true);

		assertThat(this.cacheConfiguration.configureCacheFactoryBean(cacheFactoryBean)).isEqualTo(cacheFactoryBean);

		verify(cacheFactoryBean, times(1)).setBeanClassLoader(eq(testBeanClassLoader));
		verify(cacheFactoryBean, times(1)).setBeanFactory(eq(mockBeanFactory));
		verify(cacheFactoryBean, times(1)).setClose(eq(false));
		verify(cacheFactoryBean, times(1)).setCopyOnRead(eq(true));
		verify(cacheFactoryBean, times(1)).setCriticalHeapPercentage(eq(0.90f));
		verify(cacheFactoryBean, times(1)).setCriticalOffHeapPercentage(eq(0.85f));
		verify(cacheFactoryBean, times(1)).setEvictionHeapPercentage(eq(0.75f));
		verify(cacheFactoryBean, times(1)).setEvictionOffHeapPercentage(eq(0.55f));
		verify(cacheFactoryBean, times(1)).setGatewayConflictResolver(eq(mockGatewayConflictResolver));
		verify(cacheFactoryBean, times(1)).setJndiDataSources(eq(jndiDataSources));
		verify(cacheFactoryBean, times(1)).setTransactionListeners(eq(transactionListeners));
		verify(cacheFactoryBean, times(1)).setTransactionWriter(eq(mockTransactionWriter));
		verify(cacheFactoryBean, times(1)).setUseBeanFactoryLocator(eq(true));
	}
}
