/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.test;

import java.util.Properties;

import org.springframework.data.gemfire.CacheFactoryBean;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.GemFireCache;

/**
 * @author David Turanski
 *
 */
public class MockCacheFactoryBean extends CacheFactoryBean {

	public MockCacheFactoryBean() {
		this.cache = new StubCache();
		this.useBeanFactoryLocator = false;
	}

	/**
	 * @param bean
	 */
	public MockCacheFactoryBean(CacheFactoryBean cacheFactoryBean) {
		this();
		if (cacheFactoryBean != null) {
			this.factoryLocator = cacheFactoryBean.getBeanFactoryLocator();
			this.lazyInitialize = cacheFactoryBean.isLazyInitialize();
			this.beanClassLoader = cacheFactoryBean.getBeanClassLoader();
			this.beanFactory = cacheFactoryBean.getBeanFactory();
			this.beanName = cacheFactoryBean.getBeanName();
			this.cacheXml = cacheFactoryBean.getCacheXml();
			this.properties = cacheFactoryBean.getProperties();
			this.copyOnRead = cacheFactoryBean.getCopyOnRead();
			this.criticalHeapPercentage = cacheFactoryBean.getCriticalHeapPercentage();
			this.evictionHeapPercentage = cacheFactoryBean.getEvictionHeapPercentage();
			this.dynamicRegionSupport = cacheFactoryBean.getDynamicRegionSupport();
			this.enableAutoReconnect = cacheFactoryBean.getEnableAutoReconnect();
			this.gatewayConflictResolver = cacheFactoryBean.getGatewayConflictResolver();
			this.jndiDataSources = cacheFactoryBean.getJndiDataSources();
			this.lockLease = cacheFactoryBean.getLockLease();
			this.lockTimeout = cacheFactoryBean.getLockTimeout();
			this.messageSyncInterval = cacheFactoryBean.getMessageSyncInterval();
			this.pdxDiskStoreName = cacheFactoryBean.getPdxDiskStoreName();
			this.pdxIgnoreUnreadFields = cacheFactoryBean.getPdxIgnoreUnreadFields();
			this.pdxPersistent = cacheFactoryBean.getPdxPersistent();
			this.pdxReadSerialized = cacheFactoryBean.getPdxReadSerialized();
			this.pdxSerializer = cacheFactoryBean.getPdxSerializer();
			this.searchTimeout = cacheFactoryBean.getSearchTimeout();
			this.transactionListeners = cacheFactoryBean.getTransactionListeners();
			this.transactionWriter = cacheFactoryBean.getTransactionWriter();
		}
	}

	@Override
	protected Object createFactory(Properties gemfireProperties) {
		setProperties(gemfireProperties);
		return new CacheFactory(gemfireProperties);
	}

	protected GemFireCache fetchCache() {
		((StubCache) cache).setProperties(this.properties);
		return cache;
	}
 
}
