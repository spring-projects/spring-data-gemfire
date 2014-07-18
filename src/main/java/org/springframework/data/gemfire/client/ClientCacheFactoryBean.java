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
import java.util.List;
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
public class ClientCacheFactoryBean extends CacheFactoryBean {

	/**
	 * Inner class to avoid a hard dependency on the GemFire 6.6 API.
	 * 
	 * @author Costin Leau
	 */
	private class PdxOptions implements Runnable {

		private ClientCacheFactory factory;

		PdxOptions(ClientCacheFactory factory) {
			this.factory = factory;
		}

		public void run() {
			if (pdxSerializer != null) {
				Assert.isAssignable(PdxSerializer.class,
						pdxSerializer.getClass(), "Invalid pdx serializer used");
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

	private String poolName;
	private Pool pool;
	
	protected Boolean readyForEvents = false;

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
	
	private void initializePool(ClientCacheFactory clientCacheFactory) {
		Pool localPool = pool;

		if (localPool == null) {
			if (StringUtils.hasText(poolName)) {
				localPool = PoolManager.find(poolName);
			}
			
		   // Bind this client cache to a pool that hasn't been created yet.
			if (localPool == null) {
				PoolFactoryBean.connectToTemporaryDs(this.properties);
			}
			
			if (StringUtils.hasText(poolName)) {
				try {
					getBeanFactory().isTypeMatch(poolName, Pool.class);
					localPool = getBeanFactory().getBean(poolName, Pool.class);
				}
				catch (Exception e) {
					String message = String.format("No bean found with name '%1$s' of type '%2$s'.%3$s", poolName,
						Pool.class.getName(), (GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME.equals(poolName)
							? " A client cache requires a pool." : ""));

					throw new BeanInitializationException(message);
				}
			}
			else {
				if (log.isDebugEnabled()) {
					log.debug("Checking for a unique pool...");
				}

				localPool = getBeanFactory().getBean(Pool.class);
				this.poolName = localPool.getName();
			}
		 
		}

		if (localPool != null) {
			// copy the pool settings - this way if the pool is not found, at
			// least the cache will have a similar config
			clientCacheFactory.setPoolFreeConnectionTimeout(localPool.getFreeConnectionTimeout());
			clientCacheFactory.setPoolIdleTimeout(localPool.getIdleTimeout());
			clientCacheFactory.setPoolLoadConditioningInterval(localPool.getLoadConditioningInterval());
			clientCacheFactory.setPoolMaxConnections(localPool.getMaxConnections());
			clientCacheFactory.setPoolMinConnections(localPool.getMinConnections());
			clientCacheFactory.setPoolMultiuserAuthentication(localPool.getMultiuserAuthentication());
			clientCacheFactory.setPoolPingInterval(localPool.getPingInterval());
			clientCacheFactory.setPoolPRSingleHopEnabled(localPool.getPRSingleHopEnabled());
			clientCacheFactory.setPoolReadTimeout(localPool.getReadTimeout());
			clientCacheFactory.setPoolRetryAttempts(localPool.getRetryAttempts());
			clientCacheFactory.setPoolServerGroup(localPool.getServerGroup());
			clientCacheFactory.setPoolSocketBufferSize(localPool.getSocketBufferSize());
			clientCacheFactory.setPoolStatisticInterval(localPool.getStatisticInterval());
			clientCacheFactory.setPoolSubscriptionAckInterval(localPool.getSubscriptionAckInterval());
			clientCacheFactory.setPoolSubscriptionEnabled(localPool.getSubscriptionEnabled());
			clientCacheFactory.setPoolSubscriptionMessageTrackingTimeout(localPool.getSubscriptionMessageTrackingTimeout());
			clientCacheFactory.setPoolSubscriptionRedundancy(localPool.getSubscriptionRedundancy());
			clientCacheFactory.setPoolThreadLocalConnections(localPool.getThreadLocalConnections());

			List<InetSocketAddress> locators = localPool.getLocators();

			if (locators != null) {
				for (InetSocketAddress socketAddress : locators) {
					clientCacheFactory.addPoolLocator(socketAddress.getHostName(), socketAddress.getPort());
				}
			}

			List<InetSocketAddress> servers = localPool.getServers();

			if (servers != null) {
				for (InetSocketAddress socketAddress : servers) {
					clientCacheFactory.addPoolServer(socketAddress.getHostName(), socketAddress.getPort());
				}
			}
		}
	}

	@Override
	protected void applyPdxOptions(Object factory) {
		if (factory instanceof ClientCacheFactory) {
			new PdxOptions((ClientCacheFactory) factory).run();
		}
	}

	@Override
	protected void postProcessPropertiesBeforeInitialization(final Properties gemfireProperties) {
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

	@Override
	public final Boolean getEnableAutoReconnect() {
		return Boolean.FALSE;
	}

	@Override
	public final void setEnableAutoReconnect(final Boolean enableAutoReconnect) {
		throw new UnsupportedOperationException("Auto-reconnect is not supported on ClientCache.");
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

	/**
	 * Gets the value for the readyForEvents property.
	 *
	 * @return a boolean value indicating the state of the 'readyForEvents' property.
	 */
	public Boolean getReadyForEvents(){
		return this.readyForEvents;
	}

	@Override
	public final Boolean getUseSharedConfiguration() {
		return Boolean.FALSE;
	}

	@Override
	public final void setUseSharedConfiguration(Boolean useSharedConfiguration) {
		throw new UnsupportedOperationException("Shared, cluster configuration is not applicable to clients.");
	}

}
