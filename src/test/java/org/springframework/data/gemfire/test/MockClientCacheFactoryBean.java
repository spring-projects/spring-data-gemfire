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

import com.gemstone.gemfire.cache.GemFireCache;
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

	public MockClientCacheFactoryBean(ClientCacheFactoryBean clientCacheFactoryBean) {
		this();

		if (clientCacheFactoryBean != null) {
			this.beanFactoryLocator = clientCacheFactoryBean.getBeanFactoryLocator();
			this.beanClassLoader = clientCacheFactoryBean.getBeanClassLoader();
			this.beanFactory = clientCacheFactoryBean.getBeanFactory();
			this.beanName = clientCacheFactoryBean.getBeanName();
			this.cacheXml = clientCacheFactoryBean.getCacheXml();
			this.copyOnRead = clientCacheFactoryBean.getCopyOnRead();
			this.criticalHeapPercentage = clientCacheFactoryBean.getCriticalHeapPercentage();
			this.durableClientId = clientCacheFactoryBean.getDurableClientId();
			this.durableClientTimeout = clientCacheFactoryBean.getDurableClientTimeout();
			this.dynamicRegionSupport = clientCacheFactoryBean.getDynamicRegionSupport();
			this.evictionHeapPercentage = clientCacheFactoryBean.getEvictionHeapPercentage();
			this.gatewayConflictResolver = clientCacheFactoryBean.getGatewayConflictResolver();
			this.jndiDataSources = clientCacheFactoryBean.getJndiDataSources();
			this.keepAlive = clientCacheFactoryBean.isKeepAlive();
			this.lockLease = clientCacheFactoryBean.getLockLease();
			this.lockTimeout = clientCacheFactoryBean.getLockTimeout();
			this.messageSyncInterval = clientCacheFactoryBean.getMessageSyncInterval();
			this.pdxDiskStoreName = clientCacheFactoryBean.getPdxDiskStoreName();
			this.pdxIgnoreUnreadFields = clientCacheFactoryBean.getPdxIgnoreUnreadFields();
			this.pdxPersistent = clientCacheFactoryBean.getPdxPersistent();
			this.pdxReadSerialized = clientCacheFactoryBean.getPdxReadSerialized();
			this.pdxSerializer = clientCacheFactoryBean.getPdxSerializer();
			this.poolName = clientCacheFactoryBean.getPoolName();
			this.properties = clientCacheFactoryBean.getProperties();
			this.readyForEvents = clientCacheFactoryBean.getReadyForEvents();
			this.searchTimeout = clientCacheFactoryBean.getSearchTimeout();
			this.transactionListeners = clientCacheFactoryBean.getTransactionListeners();
			this.transactionWriter = clientCacheFactoryBean.getTransactionWriter();
		}
	}

	@Override
	protected Object createFactory(Properties gemfireProperties) {
		setProperties(gemfireProperties);
		return new ClientCacheFactory(gemfireProperties);
	}

	@Override
	protected GemFireCache fetchCache() {
		((StubCache) cache).setProperties(getProperties());
		return cache;
	}

}
