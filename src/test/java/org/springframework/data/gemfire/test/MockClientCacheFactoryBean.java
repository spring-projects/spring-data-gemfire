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

import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;

/**
 * @author David Turanski
 * @author John Blum
 */
public class MockClientCacheFactoryBean extends ClientCacheFactoryBean {

	public MockClientCacheFactoryBean(ClientCacheFactoryBean clientCacheFactoryBean) {

		setUseBeanFactoryLocator(false);

		Optional.ofNullable(clientCacheFactoryBean).ifPresent(it -> {
			this.beanFactoryLocator = it.getBeanFactoryLocator();
			setBeanClassLoader(it.getBeanClassLoader());
			setBeanFactory(it.getBeanFactory());
			setBeanName(it.getBeanName());
			setCacheXml(it.getCacheXml());
			setCopyOnRead(it.getCopyOnRead());
			setCriticalHeapPercentage(it.getCriticalHeapPercentage());
			setDurableClientId(it.getDurableClientId());
			setDurableClientTimeout(it.getDurableClientTimeout());
			setDynamicRegionSupport(it.getDynamicRegionSupport());
			setEvictionHeapPercentage(it.getEvictionHeapPercentage());
			setGatewayConflictResolver(it.getGatewayConflictResolver());
			setJndiDataSources(it.getJndiDataSources());
			setKeepAlive(it.isKeepAlive());
			setLockLease(it.getLockLease());
			setLockTimeout(it.getLockTimeout());
			setMessageSyncInterval(it.getMessageSyncInterval());
			setPdxDiskStoreName(it.getPdxDiskStoreName());
			setPdxIgnoreUnreadFields(it.getPdxIgnoreUnreadFields());
			setPdxPersistent(it.getPdxPersistent());
			setPdxReadSerialized(it.getPdxReadSerialized());
			setPdxSerializer(it.getPdxSerializer());
			setPoolName(it.getPoolName());
			setProperties(it.getProperties());
			setReadyForEvents(it.getReadyForEvents());
			setSearchTimeout(it.getSearchTimeout());
			setTransactionListeners(it.getTransactionListeners());
			setTransactionWriter(it.getTransactionWriter());
		});

		applyClientCacheConfigurers(clientCacheFactoryBean.getCompositeClientCacheConfigurer());
	}

	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T fetchCache() {
		StubCache stubCache = new StubCache();
		stubCache.setProperties(getProperties());
		return (T) stubCache;
	}
}
