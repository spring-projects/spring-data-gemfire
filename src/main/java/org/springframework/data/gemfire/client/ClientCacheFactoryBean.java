/*
 * Copyright 2011 the original author or authors.
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

package org.springframework.data.gemfire.client;

import java.util.Properties;

import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.Pool;

/**
 * FactoryBean dedicated to creating client caches (caches for client JVMs).
 * Acts an utility class (as client caches are a subset with a particular configuration of the generic cache).
 * 
 * @author Costin Leau
 */
public class ClientCacheFactoryBean extends CacheFactoryBean {

	private String poolName;

	@Override
	protected Cache createCache(Object factory) {
		return (Cache) ((ClientCacheFactory) factory).create();
	}

	@Override
	protected Object createFactory(Properties props) {
		return new ClientCacheFactory(props);
	}

	@Override
	protected Cache fetchCache() {
		return (Cache) ClientCacheFactory.getAnyInstance();
	}

	//	private void initPool() {
	//		BeanFactory beanFactory = getBeanFactory();
	//
	//		// try to eagerly initialize the pool name, if defined as a bean
	//		if (beanFactory.isTypeMatch(poolName, Pool.class)) {
	//			if (log.isDebugEnabled()) {
	//				log.debug("Found bean definition for pool '" + poolName + "'. Eagerly initializing it...");
	//			}
	//			beanFactory.getBean(poolName, Pool.class);
	//		}
	//	}

	/**
	 * Sets the pool name used by this client.
	 * 
	 * @param poolName
	 */
	public void setPoolName(String poolName) {
		Assert.hasText(poolName, "pool name is required");
		this.poolName = poolName;
	}

	/**
	 * Sets the pool used by this client.
	 * 
	 * @param pool
	 */
	public void setPool(Pool pool) {
		Assert.notNull(pool, "pool cannot be null");
		setPoolName(pool.getName());
	}
}