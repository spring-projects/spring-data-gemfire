/*
 * Copyright 2002-2018 the original author or authors.
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
import org.springframework.data.gemfire.CacheFactoryBean;

/**
 * Mock {@link CacheFactoryBean} used in Unit Tests.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.CacheFactoryBean
 */
public class MockCacheFactoryBean extends CacheFactoryBean {

	public MockCacheFactoryBean(CacheFactoryBean cacheFactoryBean) {

		setUseBeanFactoryLocator(false);

		Optional.ofNullable(cacheFactoryBean).ifPresent(it -> {
			setBeanClassLoader(it.getBeanClassLoader());
			setBeanFactory(it.getBeanFactory());
			setBeanName(it.getBeanName());
			setCacheXml(it.getCacheXml());
			setPhase(it.getPhase());
			setProperties(it.getProperties());
			setClose(it.isClose());
			setCopyOnRead(it.getCopyOnRead());
			setCriticalHeapPercentage(it.getCriticalHeapPercentage());
			setCriticalOffHeapPercentage(it.getCriticalOffHeapPercentage());
			setDynamicRegionSupport(it.getDynamicRegionSupport());
			setEnableAutoReconnect(it.getEnableAutoReconnect());
			setEvictionHeapPercentage(it.getEvictionHeapPercentage());
			setEvictionOffHeapPercentage(it.getEvictionOffHeapPercentage());
			setGatewayConflictResolver(it.getGatewayConflictResolver());
			setJndiDataSources(it.getJndiDataSources());
			setLockLease(it.getLockLease());
			setLockTimeout(it.getLockTimeout());
			setMessageSyncInterval(it.getMessageSyncInterval());
			setPdxDiskStoreName(it.getPdxDiskStoreName());
			setPdxIgnoreUnreadFields(it.getPdxIgnoreUnreadFields());
			setPdxPersistent(it.getPdxPersistent());
			setPdxReadSerialized(it.getPdxReadSerialized());
			setPdxSerializer(it.getPdxSerializer());
			setSearchTimeout(it.getSearchTimeout());
			setTransactionListeners(it.getTransactionListeners());
			setTransactionWriter(it.getTransactionWriter());
			setUseBeanFactoryLocator(it.isUseBeanFactoryLocator());
			setUseClusterConfiguration(it.getUseClusterConfiguration());
		});

		applyPeerCacheConfigurers(cacheFactoryBean.getCompositePeerCacheConfigurer());
	}

	@Override
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T fetchCache() {
		StubCache stubCache = new StubCache();
		stubCache.setProperties(getProperties());
		return (T) stubCache;
	}
}
