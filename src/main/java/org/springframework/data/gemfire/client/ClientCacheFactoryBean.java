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

import java.util.concurrent.Callable;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.distributed.DistributedSystem;

/**
 * FactoryBean dedicated to creating client caches (caches for client JVMs).
 * Intended mainly for usage with GemFire 6.5 (while preserving 6.0 compatibility).
 * 
 * @author Costin Leau
 */
public class ClientCacheFactoryBean extends CacheFactoryBean {

	private String poolName;
	private boolean GEMFIRE65 = false;

	@Override
	protected Cache createCache(final DistributedSystem system) throws Exception {
		initPool();

		if (GEMFIRE65) {
			Callable<Cache> call = new Callable<Cache>() {
				public Cache call() throws Exception {
					return (Cache) new ClientCacheFactory(system.getProperties()).create();
				}
			};
			return call.call();
		}
		return super.createCache(system);
	}

	private void initPool() {
		BeanFactory beanFactory = getBeanFactory();

		// try to eagerly initialize the pool name, if defined as a bean
		if (beanFactory.isTypeMatch(poolName, Pool.class)) {
			if (log.isDebugEnabled()) {
				log.debug("Found bean definition for pool '" + poolName + "'. Eagerly initializing it...");
			}
			beanFactory.getBean(poolName, Pool.class);
		}
	}

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
