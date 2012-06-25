/*
 * Copyright 2010-2011 the original author or authors.
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

package org.springframework.data.gemfire.config;

import org.springframework.beans.factory.xml.NamespaceHandlerSupport;
import org.springframework.data.gemfire.repository.config.GemfireRepositoryParser;

/**
 * Namespace handler for GemFire definitions.
 * 
 * @author Costin Leau
 * @author David Turanski
 */
class GemfireNamespaceHandler extends NamespaceHandlerSupport {

	@Override
	public void init() {
		registerBeanDefinitionParser("cache", new CacheParser());
		registerBeanDefinitionParser("client-cache", new ClientCacheParser());

		registerBeanDefinitionParser("lookup-region", new LookupRegionParser());
		registerBeanDefinitionParser("replicated-region", new ReplicatedRegionParser());
		registerBeanDefinitionParser("partitioned-region", new PartitionedRegionParser());
		registerBeanDefinitionParser("local-region", new LocalRegionParser());
		registerBeanDefinitionParser("client-region", new ClientRegionParser());
		registerBeanDefinitionParser("pool", new PoolParser());
		registerBeanDefinitionParser("index", new IndexParser());
		registerBeanDefinitionParser("disk-store", new DiskStoreParser());
		registerBeanDefinitionParser("cache-server", new CacheServerParser());
		registerBeanDefinitionParser("transaction-manager", new TransactionManagerParser());
		registerBeanDefinitionParser("cq-listener-container", new GemfireListenerContainerParser());
		registerBeanDefinitionParser("repositories", new GemfireRepositoryParser());
	}
}