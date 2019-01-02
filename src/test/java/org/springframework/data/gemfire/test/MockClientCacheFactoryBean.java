/*
 * Copyright 2002-2019 the original author or authors.
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

import org.springframework.data.gemfire.client.ClientCacheFactoryBean;

import com.gemstone.gemfire.cache.GemFireCache;

/**
 * @author David Turanski
 * @author John Blum
 */
public class MockClientCacheFactoryBean extends ClientCacheFactoryBean {

	public MockClientCacheFactoryBean(ClientCacheFactoryBean clientCacheFactoryBean) {
		setUseBeanFactoryLocator(false);

		if (clientCacheFactoryBean != null) {
			this.beanFactoryLocator = clientCacheFactoryBean.getBeanFactoryLocator();
			setBeanClassLoader(clientCacheFactoryBean.getBeanClassLoader());
			setBeanFactory(clientCacheFactoryBean.getBeanFactory());
			setBeanName(clientCacheFactoryBean.getBeanName());
			setCacheXml(clientCacheFactoryBean.getCacheXml());
			setCopyOnRead(clientCacheFactoryBean.getCopyOnRead());
			setCriticalHeapPercentage(clientCacheFactoryBean.getCriticalHeapPercentage());
			setDurableClientId(clientCacheFactoryBean.getDurableClientId());
			setDurableClientTimeout(clientCacheFactoryBean.getDurableClientTimeout());
			setDynamicRegionSupport(clientCacheFactoryBean.getDynamicRegionSupport());
			setEvictionHeapPercentage(clientCacheFactoryBean.getEvictionHeapPercentage());
			setGatewayConflictResolver(clientCacheFactoryBean.getGatewayConflictResolver());
			setJndiDataSources(clientCacheFactoryBean.getJndiDataSources());
			setKeepAlive(clientCacheFactoryBean.isKeepAlive());
			setLockLease(clientCacheFactoryBean.getLockLease());
			setLockTimeout(clientCacheFactoryBean.getLockTimeout());
			setMessageSyncInterval(clientCacheFactoryBean.getMessageSyncInterval());
			setPdxDiskStoreName(clientCacheFactoryBean.getPdxDiskStoreName());
			setPdxIgnoreUnreadFields(clientCacheFactoryBean.getPdxIgnoreUnreadFields());
			setPdxPersistent(clientCacheFactoryBean.getPdxPersistent());
			setPdxReadSerialized(clientCacheFactoryBean.getPdxReadSerialized());
			setPdxSerializer(clientCacheFactoryBean.getPdxSerializer());
			setPoolName(clientCacheFactoryBean.getPoolName());
			setProperties(clientCacheFactoryBean.getProperties());
			setReadyForEvents(clientCacheFactoryBean.getReadyForEvents());
			setSearchTimeout(clientCacheFactoryBean.getSearchTimeout());
			setTransactionListeners(clientCacheFactoryBean.getTransactionListeners());
			setTransactionWriter(clientCacheFactoryBean.getTransactionWriter());
		}
	}

	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T fetchCache() {
		StubCache stubCache = new StubCache();
		stubCache.setProperties(getProperties());
		return (T) stubCache;
	}


}
