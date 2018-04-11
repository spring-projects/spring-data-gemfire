/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.test.mock.config;

import static org.mockito.Mockito.when;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.client.PoolFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.test.mock.GemFireMockObjectsSupport;
import org.springframework.lang.Nullable;

/**
 * The {@link GemFireMockObjectsBeanPostProcessor} class is a Spring {@link BeanPostProcessor} that applies
 * mocks and spies to Spring Data GemFire / Spring Data Geode and Pivotal GemFire / Apache Geode objects
 * and components.
 *
 * @author John Blum
 * @see org.apache.geode.cache.CacheFactory
 * @see org.apache.geode.cache.client.ClientCacheFactory
 * @see org.apache.geode.cache.client.PoolFactory
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see GemFireMockObjectsSupport
 * @since 2.0.0
 */
public class GemFireMockObjectsBeanPostProcessor implements BeanPostProcessor {

	private static final boolean DEFAULT_USE_SINGLETON_CACHE = false;

	private static final String GEMFIRE_PROPERTIES_BEAN_NAME = "gemfireProperties";

	private volatile boolean useSingletonCache;

	private final AtomicReference<Properties> gemfireProperties = new AtomicReference<>(new Properties());

	public static GemFireMockObjectsBeanPostProcessor newInstance() {
		return newInstance(DEFAULT_USE_SINGLETON_CACHE);
	}

	public static GemFireMockObjectsBeanPostProcessor newInstance(boolean useSingletonCache) {

		GemFireMockObjectsBeanPostProcessor beanPostProcessor = new GemFireMockObjectsBeanPostProcessor();

		beanPostProcessor.useSingletonCache = useSingletonCache;

		return beanPostProcessor;
	}

	@Nullable @Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

		return (isGemFireProperties(bean, beanName) ?  set((Properties) bean)
			: (bean instanceof CacheFactoryBean ? spyOnCacheFactoryBean((CacheFactoryBean) bean, this.useSingletonCache)
			: (bean instanceof PoolFactoryBean ? mockThePoolFactoryBean((PoolFactoryBean) bean)
			: bean)));
	}

	@Nullable @Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {

		if (bean instanceof GemFireCache) {

			GemFireCache gemfireCache = (GemFireCache) bean;

			when(gemfireCache.getDistributedSystem().getProperties()).thenReturn(this.gemfireProperties.get());
		}

		return bean;
	}

	private boolean isGemFireProperties(Object bean, String beanName) {
		return (bean instanceof Properties && GEMFIRE_PROPERTIES_BEAN_NAME.equals(beanName));
	}

	private Object set(Properties gemfireProperties) {
		this.gemfireProperties.set(gemfireProperties);
		return gemfireProperties;
	}

	private Object spyOnCacheFactoryBean(CacheFactoryBean bean, boolean useSingletonCache) {

		return (bean instanceof ClientCacheFactoryBean
			? SpyingClientCacheFactoryInitializer.spyOn((ClientCacheFactoryBean) bean, useSingletonCache)
			: SpyingCacheFactoryInitializer.spyOn(bean, useSingletonCache));
	}

	private Object mockThePoolFactoryBean(PoolFactoryBean bean) {
		return MockingPoolFactoryInitializer.mock(bean);
	}

	protected static class SpyingCacheFactoryInitializer
			implements CacheFactoryBean.CacheFactoryInitializer<CacheFactory> {

		public static CacheFactoryBean spyOn(CacheFactoryBean cacheFactoryBean, boolean useSingletonCache) {
			cacheFactoryBean.setCacheFactoryInitializer(new SpyingCacheFactoryInitializer(useSingletonCache));
			return cacheFactoryBean;
		}

		private final boolean useSingletonCache;

		protected SpyingCacheFactoryInitializer(boolean useSingletonCache) {
			this.useSingletonCache = useSingletonCache;
		}

		@Override
		public CacheFactory initialize(CacheFactory cacheFactory) {
			return GemFireMockObjectsSupport.spyOn(cacheFactory, useSingletonCache);
		}
	}

	protected static class SpyingClientCacheFactoryInitializer
			implements CacheFactoryBean.CacheFactoryInitializer<ClientCacheFactory> {

		public static ClientCacheFactoryBean spyOn(ClientCacheFactoryBean clientCacheFactoryBean,
				boolean useSingletonCache) {

			clientCacheFactoryBean.setCacheFactoryInitializer(
				new SpyingClientCacheFactoryInitializer(useSingletonCache));

			return clientCacheFactoryBean;
		}

		private final boolean useSingletonCache;

		protected SpyingClientCacheFactoryInitializer(boolean useSingletonCache) {
			this.useSingletonCache = useSingletonCache;
		}

		@Override
		public ClientCacheFactory initialize(ClientCacheFactory clientCacheFactory) {
			return GemFireMockObjectsSupport.spyOn(clientCacheFactory, this.useSingletonCache);
		}
	}

	protected static class MockingPoolFactoryInitializer implements PoolFactoryBean.PoolFactoryInitializer {

		public static PoolFactoryBean mock(PoolFactoryBean poolFactoryBean) {
			poolFactoryBean.setPoolFactoryInitializer(new MockingPoolFactoryInitializer());
			return poolFactoryBean;
		}

		@Override
		public PoolFactory initialize(PoolFactory poolFactory) {
			return GemFireMockObjectsSupport.mockPoolFactory();
		}
	}
}
