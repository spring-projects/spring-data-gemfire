/*
 * Copyright 2011-2013 the original author or authors.
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

import java.net.InetSocketAddress;
import java.util.Properties;

import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.config.GemfireConstants;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.pdx.PdxSerializer;

/**
 * FactoryBean dedicated to creating client caches (caches for client JVMs).
 * Acts an utility class (as client caches are a subset with a particular
 * configuration of the generic cache).
 * 
 * @author Costin Leau
 * @author Lyndon Adams
 * @author John Blum
 */
@SuppressWarnings("unused")
public class ClientCacheFactoryBean extends CacheFactoryBean {

	/**
	 * Inner class to avoid a hard dependency on the GemFire 6.6 API.
	 *
	 * @author Costin Leau
	 */
	private class PdxOptions implements Runnable {

		private ClientCacheFactory factory;

		private PdxOptions(ClientCacheFactory factory) {
			this.factory = factory;
		}

		public void run() {
			if (pdxSerializer != null) {
				Assert.isAssignable(PdxSerializer.class, pdxSerializer.getClass(), "Invalid pdx serializer used");
				factory.setPdxSerializer((PdxSerializer) pdxSerializer);
			}
			if (pdxDiskStoreName != null) {
				factory.setPdxDiskStore(pdxDiskStoreName);
			}
			if (pdxIgnoreUnreadFields != null) {
				factory.setPdxIgnoreUnreadFields(pdxIgnoreUnreadFields);
			}
			if (pdxPersistent != null) {
				factory.setPdxPersistent(pdxPersistent);
			}
			if (pdxReadSerialized != null) {
				factory.setPdxReadSerialized(pdxReadSerialized);
			}

		}
	}

	protected Boolean readyForEvents = false;

	private Pool pool;

	private String poolName;

	@Override
	protected void postProcessPropertiesBeforeInitialization(Properties gemfireProperties) {
	}

	@Override
	protected GemFireCache createCache(Object factory) {
		ClientCacheFactory clientCacheFactory = (ClientCacheFactory) factory;

		initializePool(clientCacheFactory);

		GemFireCache cache = clientCacheFactory.create();

		// register for events after Pool and Regions been created and iff non-durable client...
		readyForEvents();

		return cache;
	}

	@Override
	protected Object createFactory(Properties gemfireProperties) {
		return new ClientCacheFactory(gemfireProperties);
	}

	@Override
	protected GemFireCache fetchCache() {
		return ClientCacheFactory.getAnyInstance();
	}

	/**
	 * Initialize the Pool settings on the ClientCacheFactory.
	 *
	 * @param clientCacheFactory the GemFire ClientCacheFactory used to configure and create a GemFire ClientCache.
	 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
	 */
	private void initializePool(ClientCacheFactory clientCacheFactory) {
		initializeClientCacheFactoryPoolSettings(clientCacheFactory, resolvePool(this.pool));
	}

	/**
	 * Resolves the appropriate GemFire Pool from configuration used to configure the ClientCache.
	 *
	 * @param pool the preferred GemFire Pool to use in the configuration of the ClientCache.
	 * @return the resolved GemFire Pool.
	 * @see com.gemstone.gemfire.cache.client.Pool
	 * @see com.gemstone.gemfire.cache.client.PoolManager#find(String)
	 */
	private Pool resolvePool(final Pool pool) {
		Pool localPool = pool;

		if (localPool == null) {
			localPool = PoolManager.find(poolName);

			if (localPool == null) {
				try {
					if (StringUtils.hasText(poolName) && getBeanFactory().isTypeMatch(poolName, Pool.class)) {
						localPool = getBeanFactory().getBean(poolName, Pool.class);
					}
					else {
						localPool = getBeanFactory().getBean(Pool.class);
						this.poolName = localPool.getName();
					}
				}
				catch (Exception e) {
					throw new BeanInitializationException(String.format(
						"No bean of type '%1$s' having name '%2$s' was found.%3$s", Pool.class.getName(), poolName,
						(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME.equals(poolName)
							? " A client cache requires a pool." : "")), e);
				}
			}
		}

		return localPool;
	}

	/**
	 * Copy the Pool settings to the ClientCacheFactory so the ClientCache will have a matching configuration.
	 *
	 * @param clientCacheFactory the GemFire ClientCacheFactory used to create an instance of the ClientCache.
	 * @param pool the GemFire Pool from which to copy the pool settings.
	 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
	 * @see com.gemstone.gemfire.cache.client.Pool
	 */
	private void initializeClientCacheFactoryPoolSettings(ClientCacheFactory clientCacheFactory, Pool pool) {
		if (pool != null) {
			clientCacheFactory.setPoolFreeConnectionTimeout(pool.getFreeConnectionTimeout());
			clientCacheFactory.setPoolIdleTimeout(pool.getIdleTimeout());
			clientCacheFactory.setPoolLoadConditioningInterval(pool.getLoadConditioningInterval());
			clientCacheFactory.setPoolMaxConnections(pool.getMaxConnections());
			clientCacheFactory.setPoolMinConnections(pool.getMinConnections());
			clientCacheFactory.setPoolMultiuserAuthentication(pool.getMultiuserAuthentication());
			clientCacheFactory.setPoolPingInterval(pool.getPingInterval());
			clientCacheFactory.setPoolPRSingleHopEnabled(pool.getPRSingleHopEnabled());
			clientCacheFactory.setPoolReadTimeout(pool.getReadTimeout());
			clientCacheFactory.setPoolRetryAttempts(pool.getRetryAttempts());
			clientCacheFactory.setPoolServerGroup(pool.getServerGroup());
			clientCacheFactory.setPoolSocketBufferSize(pool.getSocketBufferSize());
			clientCacheFactory.setPoolStatisticInterval(pool.getStatisticInterval());
			clientCacheFactory.setPoolSubscriptionAckInterval(pool.getSubscriptionAckInterval());
			clientCacheFactory.setPoolSubscriptionEnabled(pool.getSubscriptionEnabled());
			clientCacheFactory.setPoolSubscriptionMessageTrackingTimeout(pool.getSubscriptionMessageTrackingTimeout());
			clientCacheFactory.setPoolSubscriptionRedundancy(pool.getSubscriptionRedundancy());
			clientCacheFactory.setPoolThreadLocalConnections(pool.getThreadLocalConnections());

			for (InetSocketAddress socketAddress : nullSafeCollection(pool.getLocators())) {
				clientCacheFactory.addPoolLocator(socketAddress.getHostName(), socketAddress.getPort());
			}

			for (InetSocketAddress socketAddress : nullSafeCollection(pool.getServers())) {
				clientCacheFactory.addPoolServer(socketAddress.getHostName(), socketAddress.getPort());
			}
		}
	}

	@Override
	protected void setPdxOptions(Object factory) {
		if (factory instanceof ClientCacheFactory) {
			new PdxOptions((ClientCacheFactory) factory).run();
		}
	}

	/**
	 * Inform the GemFire cluster that this client cache is ready to receive events.
	 */
	private void readyForEvents(){
		ClientCache clientCache = ClientCacheFactory.getAnyInstance();

		if (Boolean.TRUE.equals(readyForEvents) && !clientCache.isClosed()) {
			try {
				clientCache.readyForEvents();
			}
			catch (IllegalStateException ignore) {
				// cannot be called for a non-durable client so exception is thrown
			}
		}
	}

	/**
	 * Sets the pool used by this client.
	 *
	 * @param pool the GemFire pool used by the Client Cache to obtain connections to the GemFire cluster.
	 */
	public void setPool(Pool pool) {
		Assert.notNull(pool, "pool cannot be null");
		this.pool = pool;
	}

	/**
	 * Sets the pool name used by this client.
	 *
	 * @param poolName set the name of the GemFire Pool used by the GemFire Client Cache.
	 */
	public void setPoolName(String poolName) {
		Assert.hasText(poolName, "pool name is required");
		this.poolName = poolName;
	}

	/**
	 * Set the readyForEvents flag.
	 *
	 * @param readyForEvents sets a boolean flag to notify the server that this durable client is ready
	 * to receive updates.
	 * @see #getReadyForEvents()
	 */
	public void setReadyForEvents(Boolean readyForEvents){
		this.readyForEvents = readyForEvents;
	}
	
	public Boolean getReadyForEvents(){
		return this.readyForEvents;
	}

}
