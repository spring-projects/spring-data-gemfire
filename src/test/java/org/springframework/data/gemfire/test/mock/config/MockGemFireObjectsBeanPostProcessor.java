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
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.test.mock.MockGemFireObjectsSupport;
import org.springframework.lang.Nullable;

/**
 * The MockGemFireObjectsBeanPostProcessor class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class MockGemFireObjectsBeanPostProcessor implements BeanPostProcessor {

	public static final MockGemFireObjectsBeanPostProcessor INSTANCE = new MockGemFireObjectsBeanPostProcessor();

	@Nullable @Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
		return (bean instanceof CacheFactoryBean ?  spyOnCacheFactoryBean((CacheFactoryBean) bean) : bean);
	}

	private Object spyOnCacheFactoryBean(CacheFactoryBean bean) {

		return (bean instanceof ClientCacheFactoryBean
			? SpyingClientCacheFactoryInitializer.spyOn((ClientCacheFactoryBean) bean)
			: SpyingCacheFactoryInitializer.spyOn(bean));
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
}
