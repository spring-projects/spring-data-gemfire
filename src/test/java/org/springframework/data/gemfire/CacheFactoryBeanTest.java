/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.io.InputStream;
import java.util.Collections;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.CacheTransactionManager;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.TransactionListener;
import org.apache.geode.cache.TransactionWriter;
import org.apache.geode.cache.control.ResourceManager;
import org.apache.geode.cache.util.GatewayConflictResolver;
import org.apache.geode.distributed.DistributedMember;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.pdx.PdxSerializer;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.support.GemfireBeanFactoryLocator;

/**
 * Unit tests for {@link CacheFactoryBean}.
 *
 * @author John Blum
 * @see java.io.InputStream
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.CacheFactory
 * @see org.apache.geode.cache.CacheTransactionManager
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.control.ResourceManager
 * @see org.apache.geode.cache.util.GatewayConflictResolver
 * @see org.apache.geode.distributed.DistributedMember
 * @see org.apache.geode.distributed.DistributedSystem
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.core.io.Resource
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @since 1.7.0
 */
@RunWith(MockitoJUnitRunner.class)
public class CacheFactoryBeanTest {

	@Mock
	private Cache mockCache;

	@Test
	public void afterPropertiesSetAppliesCacheConfigurersAndThenInitializesBeanFactoryLocator() throws Exception {

		CacheFactoryBean cacheFactoryBean = spy(new CacheFactoryBean());

		cacheFactoryBean.afterPropertiesSet();

		InOrder orderVerifier = inOrder(cacheFactoryBean);

		orderVerifier.verify(cacheFactoryBean, times(1)).applyCacheConfigurers();
		orderVerifier.verify(cacheFactoryBean, times(1)).initBeanFactoryLocator();
	}

	@Test
	public void applyingCacheConfigurersDisablesAutoReconnectAndDoesNotUseClusterConfigurationByDefault() {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.applyCacheConfigurers();

		Properties gemfireProperties = cacheFactoryBean.getProperties();

		assertThat(gemfireProperties.size(), is(equalTo(2)));
		assertThat(gemfireProperties.containsKey("disable-auto-reconnect"), is(true));
		assertThat(gemfireProperties.containsKey("use-cluster-configuration"), is(true));
		assertThat(gemfireProperties.getProperty("disable-auto-reconnect"), is(equalTo("true")));
		assertThat(gemfireProperties.getProperty("use-cluster-configuration"), is(equalTo("false")));
	}

	@Test
	public void applyCacheConfigurersWithAutoReconnectAndClusterConfigurationDisabled() {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setEnableAutoReconnect(false);
		cacheFactoryBean.setUseClusterConfiguration(false);
		cacheFactoryBean.applyCacheConfigurers();

		Properties gemfireProperties = cacheFactoryBean.getProperties();

		assertThat(gemfireProperties.size(), is(equalTo(2)));
		assertThat(gemfireProperties.containsKey("disable-auto-reconnect"), is(true));
		assertThat(gemfireProperties.containsKey("use-cluster-configuration"), is(true));
		assertThat(gemfireProperties.getProperty("disable-auto-reconnect"), is(equalTo("true")));
		assertThat(gemfireProperties.getProperty("use-cluster-configuration"), is(equalTo("false")));
	}

	@Test
	public void applyCacheConfigurersWithAutoReconnectAndClusterConfigurationEnabled() {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setEnableAutoReconnect(true);
		cacheFactoryBean.setUseClusterConfiguration(true);
		cacheFactoryBean.applyCacheConfigurers();

		Properties gemfireProperties = cacheFactoryBean.getProperties();

		assertThat(gemfireProperties.size(), is(equalTo(2)));
		assertThat(gemfireProperties.containsKey("disable-auto-reconnect"), is(true));
		assertThat(gemfireProperties.containsKey("use-cluster-configuration"), is(true));
		assertThat(gemfireProperties.getProperty("disable-auto-reconnect"), is(equalTo("false")));
		assertThat(gemfireProperties.getProperty("use-cluster-configuration"), is(equalTo("true")));
	}

	@Test
	public void applyCacheConfigurersWithAutoReconnectDisabledAndClusterConfigurationEnabled() {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setEnableAutoReconnect(false);
		cacheFactoryBean.setUseClusterConfiguration(true);
		cacheFactoryBean.applyCacheConfigurers();

		Properties gemfireProperties = cacheFactoryBean.getProperties();

		assertThat(gemfireProperties.size(), is(equalTo(2)));
		assertThat(gemfireProperties.containsKey("disable-auto-reconnect"), is(true));
		assertThat(gemfireProperties.containsKey("use-cluster-configuration"), is(true));
		assertThat(gemfireProperties.getProperty("disable-auto-reconnect"), is(equalTo("true")));
		assertThat(gemfireProperties.getProperty("use-cluster-configuration"), is(equalTo("true")));
	}

	@Test
	public void applyCacheConfigurersWithAutoReconnectEnabledAndClusterConfigurationDisabled() {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setEnableAutoReconnect(true);
		cacheFactoryBean.setUseClusterConfiguration(false);
		cacheFactoryBean.applyCacheConfigurers();

		Properties gemfireProperties = cacheFactoryBean.getProperties();

		assertThat(gemfireProperties.size(), is(equalTo(2)));
		assertThat(gemfireProperties.containsKey("disable-auto-reconnect"), is(true));
		assertThat(gemfireProperties.containsKey("use-cluster-configuration"), is(true));
		assertThat(gemfireProperties.getProperty("disable-auto-reconnect"), is(equalTo("false")));
		assertThat(gemfireProperties.getProperty("use-cluster-configuration"), is(equalTo("false")));
	}

	@Test
	public void getObjectCallsInit() throws Exception {

		AtomicBoolean initCalled = new AtomicBoolean(false);

		Cache mockCache = mock(Cache.class);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean() {
			@Override Cache init() {
				initCalled.set(true);
				return mockCache;
			}
		};

		assertThat(cacheFactoryBean.getObject(), is(sameInstance(mockCache)));
		assertThat(initCalled.get(), is(true));

		verifyZeroInteractions(mockCache);
	}

	@Test
	public void getObjectReturnsExistingCache() throws Exception {

		Cache mockCache = mock(Cache.class);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setCache(mockCache);

		assertThat(cacheFactoryBean.getObject(), is(sameInstance(mockCache)));

		verifyZeroInteractions(mockCache);
	}

	@Test
	@SuppressWarnings("deprecation")
	public void init() throws Exception {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		Cache mockCache = mock(Cache.class);

		CacheTransactionManager mockCacheTransactionManager = mock(CacheTransactionManager.class);

		DistributedMember mockDistributedMember = mock(DistributedMember.class, withSettings().lenient());

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class, withSettings().lenient());

		GatewayConflictResolver mockGatewayConflictResolver = mock(GatewayConflictResolver.class);

		PdxSerializer mockPdxSerializer = mock(PdxSerializer.class);

		Resource mockCacheXml = mock(Resource.class);

		ResourceManager mockResourceManager = mock(ResourceManager.class);

		TransactionListener mockTransactionLister = mock(TransactionListener.class);

		TransactionWriter mockTransactionWriter = mock(TransactionWriter.class);

		CacheFactory mockCacheFactory = mock(CacheFactory.class);

		when(mockBeanFactory.getAliases(anyString())).thenReturn(new String[0]);
		when(mockCacheFactory.create()).thenReturn(mockCache);
		when(mockCache.getCacheTransactionManager()).thenReturn(mockCacheTransactionManager);
		when(mockCache.getDistributedSystem()).thenReturn(mockDistributedSystem);
		when(mockCache.getResourceManager()).thenReturn(mockResourceManager);
		when(mockCacheXml.getInputStream()).thenReturn(mock(InputStream.class));
		when(mockDistributedSystem.getDistributedMember()).thenReturn(mockDistributedMember);
		when(mockDistributedSystem.getName()).thenReturn("MockDistributedSystem");
		when(mockDistributedMember.getId()).thenReturn("MockDistributedMember");
		when(mockDistributedMember.getGroups()).thenReturn(Collections.emptyList());
		when(mockDistributedMember.getRoles()).thenReturn(Collections.emptySet());
		when(mockDistributedMember.getHost()).thenReturn("skullbox");
		when(mockDistributedMember.getProcessId()).thenReturn(12345);

		ClassLoader expectedThreadContextClassLoader = Thread.currentThread().getContextClassLoader();

		Properties gemfireProperties = new Properties();

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean() {

			@Override
			protected Object createFactory(Properties actualGemfireProperties) {

				assertThat(actualGemfireProperties, is(equalTo(gemfireProperties)));
				assertThat(getBeanClassLoader(), is(equalTo(ClassLoader.getSystemClassLoader())));

				return mockCacheFactory;
			}
		};

		cacheFactoryBean.setBeanClassLoader(ClassLoader.getSystemClassLoader());
		cacheFactoryBean.setBeanFactory(mockBeanFactory);
		cacheFactoryBean.setBeanName("TestGemFireCache");
		cacheFactoryBean.setCacheXml(mockCacheXml);
		cacheFactoryBean.setCopyOnRead(true);
		cacheFactoryBean.setCriticalHeapPercentage(0.90f);
		cacheFactoryBean.setCriticalOffHeapPercentage(0.95f);
		cacheFactoryBean.setDynamicRegionSupport(null);
		cacheFactoryBean.setEnableAutoReconnect(false);
		cacheFactoryBean.setEvictionHeapPercentage(0.75f);
		cacheFactoryBean.setEvictionOffHeapPercentage(0.90f);
		cacheFactoryBean.setGatewayConflictResolver(mockGatewayConflictResolver);
		cacheFactoryBean.setJndiDataSources(null);
		cacheFactoryBean.setLockLease(15000);
		cacheFactoryBean.setLockTimeout(5000);
		cacheFactoryBean.setMessageSyncInterval(20000);
		cacheFactoryBean.setPdxDiskStoreName("TestPdxDiskStore");
		cacheFactoryBean.setPdxIgnoreUnreadFields(false);
		cacheFactoryBean.setPdxPersistent(true);
		cacheFactoryBean.setPdxReadSerialized(true);
		cacheFactoryBean.setPdxSerializer(mockPdxSerializer);
		cacheFactoryBean.setProperties(gemfireProperties);
		cacheFactoryBean.setSearchTimeout(45000);
		cacheFactoryBean.setTransactionListeners(Collections.singletonList(mockTransactionLister));
		cacheFactoryBean.setTransactionWriter(mockTransactionWriter);
		cacheFactoryBean.setUseBeanFactoryLocator(true);

		cacheFactoryBean.afterPropertiesSet();
		cacheFactoryBean.init();

		assertThat(Thread.currentThread().getContextClassLoader(), is(sameInstance(expectedThreadContextClassLoader)));

		GemfireBeanFactoryLocator beanFactoryLocator = cacheFactoryBean.getBeanFactoryLocator();

		assertThat(beanFactoryLocator, is(notNullValue()));

		BeanFactory beanFactoryReference = beanFactoryLocator.useBeanFactory("TestGemFireCache");

		assertThat(beanFactoryReference, is(sameInstance(mockBeanFactory)));

		verify(mockBeanFactory, times(1)).getAliases(anyString());
		verify(mockCacheFactory, times(1)).setPdxDiskStore(eq("TestPdxDiskStore"));
		verify(mockCacheFactory, times(1)).setPdxIgnoreUnreadFields(eq(false));
		verify(mockCacheFactory, times(1)).setPdxPersistent(eq(true));
		verify(mockCacheFactory, times(1)).setPdxReadSerialized(eq(true));
		verify(mockCacheFactory, times(1)).setPdxSerializer(eq(mockPdxSerializer));
		verify(mockCacheFactory, times(1)).create();
		verify(mockCache, times(2)).getCacheTransactionManager();
		verify(mockCache, times(1)).loadCacheXml(any(InputStream.class));
		verify(mockCache, times(1)).setCopyOnRead(eq(true));
		verify(mockCache, times(1)).setGatewayConflictResolver(same(mockGatewayConflictResolver));
		verify(mockCache, times(1)).setLockLease(eq(15000));
		verify(mockCache, times(1)).setLockTimeout(eq(5000));
		verify(mockCache, times(1)).setMessageSyncInterval(eq(20000));
		verify(mockCache, times(4)).getResourceManager();
		verify(mockCache, times(1)).setSearchTimeout(eq(45000));
		verify(mockCacheTransactionManager, times(1)).addListener(same(mockTransactionLister));
		verify(mockCacheTransactionManager, times(1)).setWriter(same(mockTransactionWriter));
		verify(mockResourceManager, times(1)).setCriticalHeapPercentage(eq(0.90f));
		verify(mockResourceManager, times(1)).setCriticalOffHeapPercentage(eq(0.95f));
		verify(mockResourceManager, times(1)).setEvictionHeapPercentage(eq(0.75f));
		verify(mockResourceManager, times(1)).setEvictionOffHeapPercentage(eq(0.90f));
	}

	@Test
	public void resolveCacheCallsFetchCacheReturnsMock() {

		Cache mockCache = mock(Cache.class);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean() {

			@Override @SuppressWarnings("unchecked ")
			protected <T extends GemFireCache> T fetchCache() {
				return (T) mockCache;
			}
		};

		assertThat(cacheFactoryBean.resolveCache(), is(sameInstance(mockCache)));

		verifyZeroInteractions(mockCache);
	}

	@Test
	public void resolveCacheCreatesCacheWhenFetchCacheThrowsCacheClosedException() {

		Cache mockCache = mock(Cache.class);

		CacheFactory mockCacheFactory = mock(CacheFactory.class);

		when(mockCacheFactory.create()).thenReturn(mockCache);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				throw new CacheClosedException("test");
			}

			@Override
			protected Object createFactory(final Properties gemfireProperties) {
				assertThat(gemfireProperties, is(sameInstance(getProperties())));
				return mockCacheFactory;
			}
		};

		assertThat(cacheFactoryBean.resolveCache(), is(equalTo(mockCache)));

		verify(mockCacheFactory, times(1)).create();
		verifyZeroInteractions(mockCache);
	}

	@Test
	public void fetchExistingCache() {

		Cache mockCache = mock(Cache.class);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setCache(mockCache);

		Cache actualCache = cacheFactoryBean.fetchCache();

		assertThat(actualCache, is(sameInstance(mockCache)));

		verifyZeroInteractions(mockCache);
	}

	@Test
	public void resolveProperties() {

		Properties gemfireProperties = new Properties();

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setProperties(gemfireProperties);

		assertThat(cacheFactoryBean.resolveProperties(), is(sameInstance(gemfireProperties)));
	}

	@Test
	public void resolvePropertiesWhenNull() {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setProperties(null);

		Properties gemfireProperties = cacheFactoryBean.resolveProperties();

		assertThat(gemfireProperties, is(notNullValue()));
		assertThat(gemfireProperties.isEmpty(), is(true));
	}

	@Test
	public void createFactory() {

		Properties gemfireProperties = new Properties();

		Object cacheFactoryReference = new CacheFactoryBean().createFactory(gemfireProperties);

		assertThat(cacheFactoryReference, is(instanceOf(CacheFactory.class)));
		assertThat(gemfireProperties.isEmpty(), is(true));

		CacheFactory cacheFactory = (CacheFactory) cacheFactoryReference;

		cacheFactory.set("name", "TestCreateCacheFactory");

		assertThat(gemfireProperties.containsKey("name"), is(true));
		assertThat(gemfireProperties.getProperty("name"), is(equalTo("TestCreateCacheFactory")));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void initializesFactoryWitCacheFactoryInitializer() {

		CacheFactory mockCacheFactory = mock(CacheFactory.class);

		CacheFactoryBean.CacheFactoryInitializer<Object> mockCacheFactoryInitializer =
			mock(CacheFactoryBean.CacheFactoryInitializer.class);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setCacheFactoryInitializer(mockCacheFactoryInitializer);

		assertThat(cacheFactoryBean.getCacheFactoryInitializer(), is(equalTo(mockCacheFactoryInitializer)));
		assertThat(cacheFactoryBean.initializeFactory(mockCacheFactory), is(sameInstance(mockCacheFactory)));

		verify(mockCacheFactoryInitializer, times(1)).initialize(eq(mockCacheFactory));
		verifyZeroInteractions(mockCacheFactory);
	}

	@Test
	public void initializeFactoryWhenNoCacheFactoryInitializerIsPresentIsNullSafe() {

		CacheFactory mockCacheFactory = mock(CacheFactory.class);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		assertThat(cacheFactoryBean.getCacheFactoryInitializer(),
			is(nullValue(CacheFactoryBean.CacheFactoryInitializer.class)));
		assertThat(cacheFactoryBean.initializeFactory(mockCacheFactory), is(sameInstance(mockCacheFactory)));

		verifyZeroInteractions(mockCacheFactory);
	}

	@Test
	public void configureFactoryWithUnspecifiedPdxOptions() {

		CacheFactory mockCacheFactory = mock(CacheFactory.class);

		assertThat(new CacheFactoryBean().configureFactory(mockCacheFactory), is(sameInstance(mockCacheFactory)));

		verify(mockCacheFactory, never()).setPdxDiskStore(any(String.class));
		verify(mockCacheFactory, never()).setPdxIgnoreUnreadFields(any(Boolean.class));
		verify(mockCacheFactory, never()).setPdxPersistent(any(Boolean.class));
		verify(mockCacheFactory, never()).setPdxReadSerialized(any(Boolean.class));
		verify(mockCacheFactory, never()).setPdxSerializer(any(PdxSerializer.class));
	}

	@Test
	public void configureFactoryWithSpecificPdxOptions() {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setPdxSerializer(mock(PdxSerializer.class));
		cacheFactoryBean.setPdxReadSerialized(true);
		cacheFactoryBean.setPdxIgnoreUnreadFields(false);

		CacheFactory mockCacheFactory = mock(CacheFactory.class);

		assertThat(cacheFactoryBean.configureFactory(mockCacheFactory), is(sameInstance(mockCacheFactory)));

		verify(mockCacheFactory, never()).setPdxDiskStore(any(String.class));
		verify(mockCacheFactory, times(1)).setPdxIgnoreUnreadFields(eq(false));
		verify(mockCacheFactory, never()).setPdxPersistent(any(Boolean.class));
		verify(mockCacheFactory, times(1)).setPdxReadSerialized(eq(true));
		verify(mockCacheFactory, times(1)).setPdxSerializer(any(PdxSerializer.class));
	}

	@Test
	public void configureFactoryWithAllPdxOptions() {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setPdxDiskStoreName("testPdxDiskStoreName");
		cacheFactoryBean.setPdxIgnoreUnreadFields(false);
		cacheFactoryBean.setPdxPersistent(true);
		cacheFactoryBean.setPdxReadSerialized(true);
		cacheFactoryBean.setPdxSerializer(mock(PdxSerializer.class));

		CacheFactory mockCacheFactory = mock(CacheFactory.class);

		assertThat(cacheFactoryBean.configureFactory(mockCacheFactory), is(sameInstance(mockCacheFactory)));

		verify(mockCacheFactory, times(1)).setPdxDiskStore(eq("testPdxDiskStoreName"));
		verify(mockCacheFactory, times(1)).setPdxIgnoreUnreadFields(eq(false));
		verify(mockCacheFactory, times(1)).setPdxPersistent(eq(true));
		verify(mockCacheFactory, times(1)).setPdxReadSerialized(eq(true));
		verify(mockCacheFactory, times(1)).setPdxSerializer(any(PdxSerializer.class));
	}

	@Test
	public void configureFactoryWithSecurityManager() {

		CacheFactory mockCacheFactory = mock(CacheFactory.class);

		org.apache.geode.security.SecurityManager mockSecurityManager =
			mock(org.apache.geode.security.SecurityManager.class);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setSecurityManager(mockSecurityManager);

		assertThat(cacheFactoryBean.getSecurityManager(), is(sameInstance(mockSecurityManager)));
		assertThat(cacheFactoryBean.configureFactory(mockCacheFactory), is(sameInstance(mockCacheFactory)));

		verify(mockCacheFactory, times(1)).setSecurityManager(eq(mockSecurityManager));
		verifyZeroInteractions(mockSecurityManager);
	}

	@Test
	public void createCacheWithCacheFactory() {

		CacheFactory mockCacheFactory = mock(CacheFactory.class);

		when(mockCacheFactory.create()).thenReturn(mockCache);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		Cache actualCache = cacheFactoryBean.createCache(mockCacheFactory);

		assertThat(actualCache, is(equalTo(mockCache)));

		verify(mockCacheFactory, times(1)).create();
		verifyZeroInteractions(mockCache);
	}

	@Test(expected = IllegalArgumentException.class)
	public void postProcessCacheWithInvalidCriticalHeapPercentage() {

		try {
			CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

			cacheFactoryBean.setCriticalHeapPercentage(200.0f);
			cacheFactoryBean.postProcess(mockCache);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("criticalHeapPercentage [200.0] is not valid; must be >= 0.0 and <= 100.0",
				expected.getMessage());

			throw expected;
		}
		finally {
			verifyZeroInteractions(mockCache);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void postProcessCacheWithInvalidCriticalOffHeapPercentage() {

		try {
			CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

			cacheFactoryBean.setCriticalOffHeapPercentage(200.0f);
			cacheFactoryBean.postProcess(mockCache);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("criticalOffHeapPercentage [200.0] is not valid; must be >= 0.0 and <= 100.0",
				expected.getMessage());

			throw expected;
		}
		finally {
			verifyZeroInteractions(mockCache);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void postProcessCacheWithInvalidEvictionHeapPercentage() {

		try {
			CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

			cacheFactoryBean.setEvictionHeapPercentage(-75.0f);
			cacheFactoryBean.postProcess(mockCache);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("evictionHeapPercentage [-75.0] is not valid; must be >= 0.0 and <= 100.0",
				expected.getMessage());

			throw expected;
		}
		finally {
			verifyZeroInteractions(mockCache);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void postProcessCacheWithInvalidEvictionOffHeapPercentage() {

		try {
			CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

			cacheFactoryBean.setEvictionOffHeapPercentage(-75.0f);
			cacheFactoryBean.postProcess(mockCache);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("evictionOffHeapPercentage [-75.0] is not valid; must be >= 0.0 and <= 100.0",
				expected.getMessage());

			throw expected;
		}
		finally {
			verifyZeroInteractions(mockCache);
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getObjectType() {
		assertThat(new CacheFactoryBean().getObjectType(), is(equalTo(Cache.class)));
	}

	@Test
	public void getObjectTypeWithExistingCache() {

		Cache mockCache = mock(Cache.class);

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setCache(mockCache);

		assertThat(cacheFactoryBean.getObjectType(), is(equalTo((Class) mockCache.getClass())));
	}

	@Test
	public void isSingleton() {
		assertTrue(new CacheFactoryBean().isSingleton());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void destroy() throws Exception {

		AtomicBoolean fetchCacheCalled = new AtomicBoolean(false);

		Cache mockCache = mock(Cache.class, "GemFireCache");

		GemfireBeanFactoryLocator mockGemfireBeanFactoryLocator = mock(GemfireBeanFactoryLocator.class);

		when(mockCache.isClosed()).thenReturn(false);

		CacheFactoryBean cacheFactoryBean = spy(new CacheFactoryBean());

		doAnswer(invocation -> {
			fetchCacheCalled.set(true);
			return mockCache;
		}).when(cacheFactoryBean).fetchCache();

		doReturn(mockGemfireBeanFactoryLocator).when(cacheFactoryBean).getBeanFactoryLocator();

		cacheFactoryBean.setClose(true);
		cacheFactoryBean.setUseBeanFactoryLocator(true);
		cacheFactoryBean.destroy();

		assertThat(fetchCacheCalled.get(), is(true));

		verify(mockCache, times(1)).isClosed();
		verify(mockCache, times(1)).close();
		verify(mockGemfireBeanFactoryLocator, times(1)).destroy();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void destroyWhenCacheIsNull() throws Exception {

		AtomicBoolean fetchCacheCalled = new AtomicBoolean(false);

		CacheFactoryBean cacheFactoryBean = spy(new CacheFactoryBean());

		doAnswer(invocation -> {
			fetchCacheCalled.set(true);
			return null;
		}).when(cacheFactoryBean).fetchCache();

		cacheFactoryBean.setClose(true);
		cacheFactoryBean.setUseBeanFactoryLocator(true);
		cacheFactoryBean.destroy();

		assertTrue(fetchCacheCalled.get());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void destroyWhenCloseIsFalse() throws Exception {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setClose(false);
		cacheFactoryBean.setUseBeanFactoryLocator(false);
		cacheFactoryBean.destroy();
	}

	@Test
	public void closeCache() {

		GemFireCache mockCache = mock(GemFireCache.class, "testCloseCache.MockCache");

		new CacheFactoryBean().close(mockCache);

		verify(mockCache, times(1)).close();
	}

	@Test
	@SuppressWarnings("all")
	public void setAndGetCacheFactoryBeanProperties() throws Exception {

		BeanFactory mockBeanFactory = mock(BeanFactory.class, "SpringBeanFactory");

		GatewayConflictResolver mockGatewayConflictResolver = mock(GatewayConflictResolver.class, "GemFireGatewayConflictResolver");

		PdxSerializer mockPdxSerializer = mock(PdxSerializer.class, "GemFirePdxSerializer");

		Resource mockCacheXml = mock(Resource.class, "GemFireCacheXml");

		TransactionListener mockTransactionListener = mock(TransactionListener.class, "GemFireTransactionListener");

		TransactionWriter mockTransactionWriter = mock(TransactionWriter.class, "GemFireTransactionWriter");

		Properties gemfireProperties = new Properties();

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		cacheFactoryBean.setBeanClassLoader(Thread.currentThread().getContextClassLoader());
		cacheFactoryBean.setBeanFactory(mockBeanFactory);
		cacheFactoryBean.setBeanName("TestCache");
		cacheFactoryBean.setCacheXml(mockCacheXml);
		cacheFactoryBean.setProperties(gemfireProperties);
		cacheFactoryBean.setUseBeanFactoryLocator(false);
		cacheFactoryBean.setClose(false);
		cacheFactoryBean.setCopyOnRead(true);
		cacheFactoryBean.setDynamicRegionSupport(new CacheFactoryBean.DynamicRegionSupport());
		cacheFactoryBean.setEnableAutoReconnect(true);
		cacheFactoryBean.setCriticalHeapPercentage(0.95f);
		cacheFactoryBean.setCriticalOffHeapPercentage(0.99f);
		cacheFactoryBean.setEvictionHeapPercentage(0.70f);
		cacheFactoryBean.setEvictionOffHeapPercentage(0.80f);
		cacheFactoryBean.setGatewayConflictResolver(mockGatewayConflictResolver);
		cacheFactoryBean.setJndiDataSources(Collections.singletonList(new CacheFactoryBean.JndiDataSource()));
		cacheFactoryBean.setLockLease(15000);
		cacheFactoryBean.setLockTimeout(5000);
		cacheFactoryBean.setMessageSyncInterval(10000);
		cacheFactoryBean.setPdxSerializer(mockPdxSerializer);
		cacheFactoryBean.setPdxReadSerialized(false);
		cacheFactoryBean.setPdxPersistent(true);
		cacheFactoryBean.setPdxIgnoreUnreadFields(true);
		cacheFactoryBean.setPdxDiskStoreName("TestPdxDiskStore");
		cacheFactoryBean.setSearchTimeout(30000);
		cacheFactoryBean.setTransactionListeners(Collections.singletonList(mockTransactionListener));
		cacheFactoryBean.setTransactionWriter(mockTransactionWriter);
		cacheFactoryBean.setUseClusterConfiguration(true);

		assertEquals(Thread.currentThread().getContextClassLoader(), cacheFactoryBean.getBeanClassLoader());
		assertSame(mockBeanFactory, cacheFactoryBean.getBeanFactory());
		assertNull(cacheFactoryBean.getBeanFactoryLocator());
		assertEquals("TestCache", cacheFactoryBean.getBeanName());
		assertSame(mockCacheXml, cacheFactoryBean.getCacheXml());
		assertSame(gemfireProperties, cacheFactoryBean.getProperties());
		assertTrue(Boolean.FALSE.equals(TestUtils.readField("useBeanFactoryLocator", cacheFactoryBean)));
		assertTrue(Boolean.FALSE.equals(TestUtils.readField("close", cacheFactoryBean)));
		assertTrue(cacheFactoryBean.getCopyOnRead());
		assertEquals(0.95f, cacheFactoryBean.getCriticalHeapPercentage(), 0.0f);
		assertEquals(0.99f, cacheFactoryBean.getCriticalOffHeapPercentage(), 0.0f);
		assertNotNull(cacheFactoryBean.getDynamicRegionSupport());
		assertTrue(cacheFactoryBean.getEnableAutoReconnect());
		assertEquals(0.70f, cacheFactoryBean.getEvictionHeapPercentage(), 0.0f);
		assertEquals(0.80f, cacheFactoryBean.getEvictionOffHeapPercentage(), 0.0f);
		assertSame(mockGatewayConflictResolver, cacheFactoryBean.getGatewayConflictResolver());
		assertNotNull(cacheFactoryBean.getJndiDataSources());
		assertEquals(1, cacheFactoryBean.getJndiDataSources().size());
		assertEquals(15000, cacheFactoryBean.getLockLease().intValue());
		assertEquals(5000, cacheFactoryBean.getLockTimeout().intValue());
		assertEquals(10000, cacheFactoryBean.getMessageSyncInterval().intValue());
		assertSame(mockPdxSerializer, cacheFactoryBean.getPdxSerializer());
		assertFalse(cacheFactoryBean.getPdxReadSerialized());
		assertTrue(cacheFactoryBean.getPdxPersistent());
		assertTrue(cacheFactoryBean.getPdxIgnoreUnreadFields());
		assertEquals("TestPdxDiskStore", cacheFactoryBean.getPdxDiskStoreName());
		assertEquals(30000, cacheFactoryBean.getSearchTimeout().intValue());
		assertNotNull(cacheFactoryBean.getTransactionListeners());
		assertEquals(1, cacheFactoryBean.getTransactionListeners().size());
		assertSame(mockTransactionListener, cacheFactoryBean.getTransactionListeners().get(0));
		assertSame(mockTransactionWriter, cacheFactoryBean.getTransactionWriter());
		assertTrue(cacheFactoryBean.getUseClusterConfiguration());
	}
}
