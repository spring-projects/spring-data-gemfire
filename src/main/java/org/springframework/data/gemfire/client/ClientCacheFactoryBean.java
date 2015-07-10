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

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.Properties;

import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.config.GemfireConstants;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.pdx.PdxSerializer;

/**
 * FactoryBean dedicated to creating GemFire client caches.
 * 
 * @author Costin Leau
 * @author Lyndon Adams
 * @author John Blum
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see com.gemstone.gemfire.cache.GemFireCache
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
 */
@SuppressWarnings("unused")
public class ClientCacheFactoryBean extends CacheFactoryBean {

	protected Boolean readyForEvents = false;

	private Pool pool;

	private String poolName;

	@Override
	protected void postProcessPropertiesBeforeInitialization(Properties gemfireProperties) {
	}

	/**
	 * Fetches an existing GemFire ClientCache instance from the ClientCacheFactory.
	 *
	 * @param <T> is Class type extension of GemFireCache.
	 * @return the existing GemFire ClientCache instance if available.
	 * @throws com.gemstone.gemfire.cache.CacheClosedException if an existing GemFire Cache instance does not exist.
	 * @throws java.lang.IllegalStateException if the GemFire cache instance is not a ClientCache.
	 * @see com.gemstone.gemfire.cache.GemFireCache
	 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory#getAnyInstance()
	 */
	@Override
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T fetchCache() {
		return (T) ClientCacheFactory.getAnyInstance();
	}

	/**
	 * Resolves the GemFire System properties used to configure the GemFire ClientCache instance.
	 *
	 * @return a Properties object containing GemFire System properties used to configure
	 * the GemFire ClientCache instance.
	 */
	@Override
	protected Properties resolveProperties() {
		Properties gemfireProperties = super.resolveProperties();

		DistributedSystem distributedSystem = getDistributedSystem();

		if (GemfireUtils.isConnected(distributedSystem)) {
			Properties distributedSystemProperties = (Properties) distributedSystem.getProperties().clone();
			distributedSystemProperties.putAll(gemfireProperties);
			gemfireProperties = distributedSystemProperties;
		}

		return gemfireProperties;
	}

	/* (non-Javadoc) */
	<T extends DistributedSystem> T getDistributedSystem() {
		return GemfireUtils.getDistributedSystem();
	}

	/**
	 * Creates an instance of GemFire factory initialized with the given GemFire System Properties
	 * to create an instance of a GemFire cache.
	 *
	 * @param gemfireProperties a Properties object containing GemFire System properties.
	 * @return an instance of a GemFire factory used to create a GemFire cache instance.
	 * @see java.util.Properties
	 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
	 */
	@Override
	protected Object createFactory(Properties gemfireProperties) {
		return new ClientCacheFactory(gemfireProperties);
	}

	/**
	 * Initializes the GemFire factory used to create the GemFire client cache instance.  Sets PDX options
	 * specified by the user.
	 *
	 * @param factory the GemFire factory used to create an instance of the GemFire client cache.
	 * @return the initialized GemFire client cache factory.
	 * @see #isPdxOptionsSpecified()
	 */
	@Override
	protected Object prepareFactory(final Object factory) {
		if (isPdxOptionsSpecified()) {
			ClientCacheFactory clientCacheFactory = (ClientCacheFactory) factory;

			if (pdxSerializer != null) {
				Assert.isAssignable(PdxSerializer.class, pdxSerializer.getClass(), "Invalid pdx serializer used");
				clientCacheFactory.setPdxSerializer((PdxSerializer) pdxSerializer);
			}
			if (pdxDiskStoreName != null) {
				clientCacheFactory.setPdxDiskStore(pdxDiskStoreName);
			}
			if (pdxIgnoreUnreadFields != null) {
				clientCacheFactory.setPdxIgnoreUnreadFields(pdxIgnoreUnreadFields);
			}
			if (pdxPersistent != null) {
				clientCacheFactory.setPdxPersistent(pdxPersistent);
			}
			if (pdxReadSerialized != null) {
				clientCacheFactory.setPdxReadSerialized(pdxReadSerialized);
			}
		}

		return factory;
	}

	/**
	 * Creates a new GemFire cache instance using the provided factory.
	 *
	 * @param <T> parameterized Class type extension of GemFireCache.
	 * @param factory the appropriate GemFire factory used to create a cache instance.
	 * @return an instance of the GemFire cache.
	 * @see com.gemstone.gemfire.cache.GemFireCache
	 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory#create()
	 */
	@Override
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T createCache(Object factory) {
		return (T) initializePool((ClientCacheFactory) factory).create();
	}

	/**
	 * Initialize the Pool settings on the ClientCacheFactory.
	 *
	 * @param clientCacheFactory the GemFire ClientCacheFactory used to configure and create a GemFire ClientCache.
	 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
	 */
	private ClientCacheFactory initializePool(ClientCacheFactory clientCacheFactory) {
		return initializeClientCacheFactory(clientCacheFactory, resolvePool(this.pool));
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
						"no Bean of type '%1$s' having name '%2$s' was found%3$s", Pool.class.getName(), poolName,
							(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME.equals(poolName)
								? "; a ClientCache requires a Pool" : "")), e);
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
	private ClientCacheFactory initializeClientCacheFactory(ClientCacheFactory clientCacheFactory, Pool pool) {
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

			for (InetSocketAddress socketAddress : CollectionUtils.nullSafeCollection(pool.getLocators())) {
				clientCacheFactory.addPoolLocator(socketAddress.getHostName(), socketAddress.getPort());
			}

			for (InetSocketAddress socketAddress : CollectionUtils.nullSafeCollection(pool.getServers())) {
				clientCacheFactory.addPoolServer(socketAddress.getHostName(), socketAddress.getPort());
			}
		}

		return clientCacheFactory;
	}

	/**
	 * Register for events after Pool and Regions have been created and iff non-durable client...
	 *
	 * @param <T> parameterized Class type extension of GemFireCache.
	 * @param cache the GemFire cache instance to process.
	 * @return the processed cache instance after ready for events.
	 * @throws java.io.IOException if an error occurs during post processing.
	 * @see org.springframework.data.gemfire.CacheFactoryBean#postProcess(com.gemstone.gemfire.cache.GemFireCache)
	 * @see #readyForEvents(com.gemstone.gemfire.cache.GemFireCache)
	 * @see com.gemstone.gemfire.cache.GemFireCache
	 * @see com.gemstone.gemfire.cache.client.ClientCache
	 */
	@Override
	protected <T extends GemFireCache> T postProcess(T cache) throws IOException {
		return readyForEvents(super.postProcess(cache));
	}

	/**
	 * Inform the GemFire cluster that this client cache is ready to receive events.
	 */
	private <T extends GemFireCache> T readyForEvents(T clientCache) {
		if (Boolean.TRUE.equals(getReadyForEvents()) && !clientCache.isClosed()) {
			try {
				((ClientCache) clientCache).readyForEvents();
			}
			catch (IllegalStateException ignore) {
				// cannot be called for a non-durable client so exception is thrown
			}
		}

		return clientCache;
	}

	@Override
	public final void setEnableAutoReconnect(final Boolean enableAutoReconnect) {
		throw new UnsupportedOperationException("Auto-reconnect is not supported on ClientCache.");
	}

	@Override
	public final Boolean getEnableAutoReconnect() {
		return Boolean.FALSE;
	}

	@Override
	public Class<? extends GemFireCache> getObjectType() {
		return (cache != null ? cache.getClass() : ClientCache.class);
	}

	/**
	 * Sets the pool used by this client.
	 *
	 * @param pool the GemFire pool used by the Client Cache to obtain connections to the GemFire cluster.
	 */
	public void setPool(Pool pool) {
		Assert.notNull(pool, "GemFire Pool must not be null");
		this.pool = pool;
	}

	/**
	 * Sets the pool name used by this client.
	 *
	 * @param poolName set the name of the GemFire Pool used by the GemFire Client Cache.
	 */
	public void setPoolName(String poolName) {
		Assert.hasText(poolName, "Pool 'name' is required");
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
		return readyForEvents;
	}

	@Override
	public final void setUseClusterConfiguration(Boolean useClusterConfiguration) {
		throw new UnsupportedOperationException("Shared, cluster configuration is not applicable to clients.");
	}

	@Override
	public final Boolean getUseClusterConfiguration() {
		return Boolean.FALSE;
	}

}
