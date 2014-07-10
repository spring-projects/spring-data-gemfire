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

import org.springframework.data.gemfire.client.ClientCacheFactoryBean;

import com.gemstone.gemfire.cache.client.ClientCacheFactory;

/**
 * @author David Turanski
 * @author Lyndon Adams
 *
 */
public class MockClientCacheFactoryBean extends ClientCacheFactoryBean {
	 
	public MockClientCacheFactoryBean() {
		this.cache = new StubCache();
	}

	/**
	 * @param bean
	 */
	public MockClientCacheFactoryBean(ClientCacheFactoryBean cacheFactoryBean) {
		this();
		if (cacheFactoryBean != null) {
			this.factoryLocator = cacheFactoryBean.getBeanFactoryLocator();
			this.beanFactory = cacheFactoryBean.getBeanFactory();
			this.beanName = cacheFactoryBean.getBeanName();
			this.beanClassLoader = cacheFactoryBean.getBeanClassLoader();
			this.cacheXml = cacheFactoryBean.getCacheXml();
			this.copyOnRead = cacheFactoryBean.getCopyOnRead();
			this.criticalHeapPercentage = cacheFactoryBean.getCriticalHeapPercentage();
			this.dynamicRegionSupport = cacheFactoryBean.getDynamicRegionSupport();
			this.evictionHeapPercentage = cacheFactoryBean.getEvictionHeapPercentage();
			this.gatewayConflictResolver = cacheFactoryBean.getGatewayConflictResolver();
			this.jndiDataSources = cacheFactoryBean.getJndiDataSources();
			this.lockLease = cacheFactoryBean.getLockLease();
			this.lockTimeout = cacheFactoryBean.getLockTimeout();
			this.messageSyncInterval = cacheFactoryBean.getMessageSyncInterval();
			this.pdxDiskStoreName = cacheFactoryBean.getPdxDiskStoreName();
			this.pdxIgnoreUnreadFields = cacheFactoryBean.getPdxIgnoreUnreadFields();
			this.pdxPersistent = cacheFactoryBean.getPdxPersistent();
			this.pdxSerializer = cacheFactoryBean.getPdxSerializer();
			this.properties = cacheFactoryBean.getProperties();
			this.searchTimeout = cacheFactoryBean.getSearchTimeout();
			this.transactionListeners = cacheFactoryBean.getTransactionListeners();
			this.transactionWriter = cacheFactoryBean.getTransactionWriter();
			this.readyForEvents = cacheFactoryBean.getReadyForEvents();
		}
	}
 
	@Override
	protected Object createFactory(Properties gemfireProperties) {
		((StubCache)cache).setProperties(gemfireProperties);
		return new ClientCacheFactory(gemfireProperties);
	}
}
