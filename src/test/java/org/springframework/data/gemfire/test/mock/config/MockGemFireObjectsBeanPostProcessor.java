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

import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.client.PoolFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.test.mock.MockGemFireObjectsSupport;
import org.springframework.lang.Nullable;

/**
 * The {@link MockGemFireObjectsBeanPostProcessor} class is a Spring {@link BeanPostProcessor} that applies
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
 * @see org.springframework.data.gemfire.test.mock.MockGemFireObjectsSupport
 * @since 2.0.0
 */
public class MockGemFireObjectsBeanPostProcessor implements BeanPostProcessor {

	public static final MockGemFireObjectsBeanPostProcessor INSTANCE = new MockGemFireObjectsBeanPostProcessor();

	@Nullable @Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

		return (bean instanceof CacheFactoryBean ?  spyOnCacheFactoryBean((CacheFactoryBean) bean)
			: (bean instanceof PoolFactoryBean ? mockThePoolFactoryBean((PoolFactoryBean) bean)
			: bean));
	}

	private Object spyOnCacheFactoryBean(CacheFactoryBean bean) {

		return (bean instanceof ClientCacheFactoryBean
			? SpyingClientCacheFactoryInitializer.spyOn((ClientCacheFactoryBean) bean)
			: SpyingCacheFactoryInitializer.spyOn(bean));
	}

	private Object mockThePoolFactoryBean(PoolFactoryBean bean) {
		return MockingPoolFactoryInitializer.mock(bean);
	}

	protected static class SpyingCacheFactoryInitializer
			implements CacheFactoryBean.CacheFactoryInitializer<CacheFactory> {

		public static CacheFactoryBean spyOn(CacheFactoryBean cacheFactoryBean) {
			cacheFactoryBean.setCacheFactoryInitializer(new SpyingCacheFactoryInitializer());
			return cacheFactoryBean;
		}

		@Override
		public CacheFactory initialize(CacheFactory cacheFactory) {
			return MockGemFireObjectsSupport.spyOn(cacheFactory);
		}
	}

	protected static class SpyingClientCacheFactoryInitializer
			implements CacheFactoryBean.CacheFactoryInitializer<ClientCacheFactory> {

		public static ClientCacheFactoryBean spyOn(ClientCacheFactoryBean clientCacheFactoryBean) {
			clientCacheFactoryBean.setCacheFactoryInitializer(new SpyingClientCacheFactoryInitializer());
			return clientCacheFactoryBean;
		}

		@Override
		public ClientCacheFactory initialize(ClientCacheFactory clientCacheFactory) {
			return MockGemFireObjectsSupport.spyOn(clientCacheFactory);
		}
	}

	protected static class MockingPoolFactoryInitializer implements PoolFactoryBean.PoolFactoryInitializer {

		public static PoolFactoryBean mock(PoolFactoryBean poolFactoryBean) {
			poolFactoryBean.setPoolFactoryInitializer(new MockingPoolFactoryInitializer());
			return poolFactoryBean;
		}

		@Override
		public PoolFactory initialize(PoolFactory poolFactory) {
			return MockGemFireObjectsSupport.mockPoolFactory();
		}
	}
}
