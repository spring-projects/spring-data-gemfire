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
import java.util.Map;
import java.util.Properties;

import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.pdx.PdxSerializer;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.client.support.DefaultableDelegatingPoolAdapter;
import org.springframework.data.gemfire.client.support.DelegatingPoolAdapter;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * FactoryBean dedicated to creating GemFire client caches.
 *
 * @author Costin Leau
 * @author Lyndon Adams
 * @author John Blum
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.context.event.ContextRefreshedEvent
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @see org.springframework.data.gemfire.support.ConnectionEndpointList
 * @see com.gemstone.gemfire.cache.GemFireCache
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
 * @see com.gemstone.gemfire.cache.client.Pool
 * @see com.gemstone.gemfire.cache.client.PoolManager
 * @see com.gemstone.gemfire.distributed.DistributedSystem
 * @see com.gemstone.gemfire.pdx.PdxSerializer
 */
@SuppressWarnings("unused")
public class ClientCacheFactoryBean extends CacheFactoryBean implements ApplicationListener<ContextRefreshedEvent> {

	private Boolean keepAlive = false;
	private Boolean multiUserAuthentication;
	private Boolean prSingleHopEnabled;
	private Boolean readyForEvents;
	private Boolean subscriptionEnabled;
	private Boolean threadLocalConnections;

	private ConnectionEndpointList locators = new ConnectionEndpointList();
	private ConnectionEndpointList servers = new ConnectionEndpointList();

	private Integer durableClientTimeout;
	private Integer freeConnectionTimeout;
	private Integer loadConditioningInterval;
	private Integer maxConnections;
	private Integer minConnections;
	private Integer readTimeout;
	private Integer retryAttempts;
	private Integer socketBufferSize;
	private Integer statisticsInterval;
	private Integer subscriptionAckInterval;
	private Integer subscriptionMessageTrackingTimeout;
	private Integer subscriptionRedundancy;

	private Long idleTimeout;
	private Long pingInterval;

	private Pool pool;

	private String durableClientId;
	private String poolName;
	private String serverGroup;

	@Override
	protected void postProcessBeforeCacheInitialization(Properties gemfireProperties) {
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
		ClientCache cache = getCache();
		return (T) (cache != null ? cache : ClientCacheFactory.getAnyInstance());
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

		GemfireUtils.configureDurableClient(gemfireProperties, durableClientId, durableClientTimeout);

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
		return initializePool(initializePdx((ClientCacheFactory) factory));
	}

	/**
	 * Initialize the PDX settings on the {@link ClientCacheFactory}.
	 *
	 * @param clientCacheFactory the GemFire {@link ClientCacheFactory} used to configure and create
	 * a GemFire {@link ClientCache}.
	 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
	 */
	ClientCacheFactory initializePdx(ClientCacheFactory clientCacheFactory) {
		if (isPdxOptionsSpecified()) {
			if (getPdxSerializer() != null) {
				Assert.isInstanceOf(PdxSerializer.class, getPdxSerializer(),
					String.format("[%1$s] of type [%2$s] is not a PdxSerializer;", getPdxSerializer(),
						ObjectUtils.nullSafeClassName(getPdxSerializer())));

				clientCacheFactory.setPdxSerializer((PdxSerializer) getPdxSerializer());
			}
			if (getPdxDiskStoreName() != null) {
				clientCacheFactory.setPdxDiskStore(getPdxDiskStoreName());
			}
			if (getPdxIgnoreUnreadFields() != null) {
				clientCacheFactory.setPdxIgnoreUnreadFields(getPdxIgnoreUnreadFields());
			}
			if (getPdxPersistent() != null) {
				clientCacheFactory.setPdxPersistent(getPdxPersistent());
			}
			if (getPdxReadSerialized() != null) {
				clientCacheFactory.setPdxReadSerialized(getPdxReadSerialized());
			}
		}

		return clientCacheFactory;
	}

	/**
	 * Initialize the {@link Pool} settings on the {@link ClientCacheFactory} with a given {@link Pool} instance
	 * or named {@link Pool}.
	 *
	 * @param clientCacheFactory the GemFire {@link ClientCacheFactory} used to configure and create
	 * a GemFire {@link ClientCache}.
	 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
	 * @see com.gemstone.gemfire.cache.client.Pool
	 */
	ClientCacheFactory initializePool(ClientCacheFactory clientCacheFactory) {
		DefaultableDelegatingPoolAdapter pool = DefaultableDelegatingPoolAdapter.from(
			DelegatingPoolAdapter.from(resolvePool())).preferDefault();

		clientCacheFactory.setPoolFreeConnectionTimeout(pool.getFreeConnectionTimeout(getFreeConnectionTimeout()));
		clientCacheFactory.setPoolIdleTimeout(pool.getIdleTimeout(getIdleTimeout()));
		clientCacheFactory.setPoolLoadConditioningInterval(pool.getLoadConditioningInterval(getLoadConditioningInterval()));
		clientCacheFactory.setPoolMaxConnections(pool.getMaxConnections(getMaxConnections()));
		clientCacheFactory.setPoolMinConnections(pool.getMinConnections(getMinConnections()));
		clientCacheFactory.setPoolMultiuserAuthentication(pool.getMultiuserAuthentication(getMultiUserAuthentication()));
		clientCacheFactory.setPoolPRSingleHopEnabled(pool.getPRSingleHopEnabled(getPrSingleHopEnabled()));
		clientCacheFactory.setPoolPingInterval(pool.getPingInterval(getPingInterval()));
		clientCacheFactory.setPoolReadTimeout(pool.getReadTimeout(getReadTimeout()));
		clientCacheFactory.setPoolRetryAttempts(pool.getRetryAttempts(getRetryAttempts()));
		clientCacheFactory.setPoolServerGroup(pool.getServerGroup(getServerGroup()));
		clientCacheFactory.setPoolSocketBufferSize(pool.getSocketBufferSize(getSocketBufferSize()));
		clientCacheFactory.setPoolStatisticInterval(pool.getStatisticInterval(getStatisticsInterval()));
		clientCacheFactory.setPoolSubscriptionAckInterval(pool.getSubscriptionAckInterval(getSubscriptionAckInterval()));
		clientCacheFactory.setPoolSubscriptionEnabled(pool.getSubscriptionEnabled(getSubscriptionEnabled()));
		clientCacheFactory.setPoolSubscriptionMessageTrackingTimeout(pool.getSubscriptionMessageTrackingTimeout(getSubscriptionMessageTrackingTimeout()));
		clientCacheFactory.setPoolSubscriptionRedundancy(pool.getSubscriptionRedundancy(getSubscriptionRedundancy()));
		clientCacheFactory.setPoolThreadLocalConnections(pool.getThreadLocalConnections(getThreadLocalConnections()));

		boolean noServers = getServers().isEmpty();
		boolean hasServers = !noServers;
		boolean noLocators = getLocators().isEmpty();
		boolean hasLocators = !noLocators;

		if (hasServers || noLocators) {
			Iterable<InetSocketAddress> servers = pool.getServers(getServers().toInetSocketAddresses());

			for (InetSocketAddress server : servers) {
				clientCacheFactory.addPoolServer(server.getHostName(), server.getPort());
				noServers = false;
			}
		}

		if (hasLocators || noServers) {
			Iterable<InetSocketAddress> locators = pool.getLocators(getLocators().toInetSocketAddresses());

			for (InetSocketAddress locator : locators) {
				clientCacheFactory.addPoolLocator(locator.getHostName(), locator.getPort());
			}
		}

		return clientCacheFactory;
	}

	/**
	 * Resolves the appropriate GemFire {@link Pool} from Spring configuration that will be used to configure
	 * the GemFire {@link ClientCache}.
	 *
	 * @return the resolved GemFire {@link Pool}.
	 * @see com.gemstone.gemfire.cache.client.PoolManager#find(String)
	 * @see com.gemstone.gemfire.cache.client.Pool
	 */
	Pool resolvePool() {
		Pool localPool = getPool();

		if (localPool == null) {
			String poolName = SpringUtils.defaultIfNull(getPoolName(), GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);

			localPool = findPool(poolName);

			if (localPool == null) {
				BeanFactory beanFactory = getBeanFactory();

				if (beanFactory instanceof ListableBeanFactory) {
					try {
						Map<String, PoolFactoryBean> poolFactoryBeanMap =
							((ListableBeanFactory) beanFactory).getBeansOfType(PoolFactoryBean.class, false, false);

						String dereferencedPoolName = SpringUtils.dereferenceBean(poolName);

						if (poolFactoryBeanMap.containsKey(dereferencedPoolName)) {
							return poolFactoryBeanMap.get(dereferencedPoolName).getPool();
						}
					}
					catch (BeansException e) {
						log.info(String.format("unable to resolve bean of type [%1$s] with name [%2$s]",
							PoolFactoryBean.class.getName(), poolName));
					}
				}
			}
		}

		return localPool;
	}

	/**
	 * Attempts to find a GemFire {@link Pool} with the given name.
	 *
	 * @param name a String indicating the name of the GemFire {@link Pool} to find.
	 * @return a {@link Pool} instance with the given name registered in GemFire or <code>null</code> if no {@link Pool}
	 * with name exists.
	 * @see com.gemstone.gemfire.cache.client.PoolManager#find(String)
	 * @see com.gemstone.gemfire.cache.client.Pool
	 */
	Pool findPool(String name) {
		return PoolManager.find(name);
	}

	/**
	 * Creates a new GemFire cache instance using the provided factory.
	 *
	 * @param <T> parameterized Class type extension of {@link GemFireCache}.
	 * @param factory the appropriate GemFire factory used to create a cache instance.
	 * @return an instance of the GemFire cache.
	 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory#create()
	 * @see com.gemstone.gemfire.cache.GemFireCache
	 */
	@Override
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T createCache(Object factory) {
		return (T) ((ClientCacheFactory) factory).create();
	}

	/**
	 * Inform the GemFire cluster that this client cache is ready to receive events iff the client is non-durable.
	 *
	 * @param event the ApplicationContextEvent fired when the ApplicationContext is refreshed.
	 * @see com.gemstone.gemfire.cache.client.ClientCache#readyForEvents()
	 * @see #getReadyForEvents()
	 * @see #getObject()
	 */
	@Override
	public void onApplicationEvent(final ContextRefreshedEvent event) {
		if (isReadyForEvents()) {
			try {
				((ClientCache) fetchCache()).readyForEvents();
			}
			catch (IllegalStateException ignore) {
				// thrown if clientCache.readyForEvents() is called on a non-durable client
			}
			catch (CacheClosedException ignore) {
				// cache is closed or was shutdown so ready-for-events is moot
			}
		}
	}

	/* (non-Javadoc) */
	@Override
	protected void close(GemFireCache cache) {
		((ClientCache) cache).close(isKeepAlive());
	}

	/* (non-Javadoc) */
	@Override
	public Class<? extends GemFireCache> getObjectType() {
		ClientCache cache = getCache();
		return (cache != null ? cache.getClass() : ClientCache.class);
	}

	/* (non-Javadoc) */
	public void addLocators(ConnectionEndpoint... locators) {
		this.locators.add(locators);
	}

	/* (non-Javadoc) */
	public void addLocators(Iterable<ConnectionEndpoint> locators) {
		this.locators.add(locators);
	}

	/* (non-Javadoc) */
	public void addServers(ConnectionEndpoint... servers) {
		this.servers.add(servers);
	}

	/* (non-Javadoc) */
	public void addServers(Iterable<ConnectionEndpoint> servers) {
		this.servers.add(servers);
	}

	/**
	 * Set the GemFire System property 'durable-client-id' to indicate to the server that this client is durable.
	 *
	 * @param durableClientId a String value indicating the durable client id.
	 */
	public void setDurableClientId(String durableClientId) {
		this.durableClientId = durableClientId;
	}

	/**
	 * Gets the value of the GemFire System property 'durable-client-id' indicating to the server whether
	 * this client is durable.
	 *
	 * @return a String value indicating the durable client id.
	 */
	public String getDurableClientId() {
		return durableClientId;
	}

	/**
	 * Set the GemFire System property 'durable-client-timeout' indicating to the server how long to track events
	 * for the durable client when disconnected.
	 *
	 * @param durableClientTimeout an Integer value indicating the timeout in seconds for the server to keep
	 * the durable client's queue around.
	 */
	public void setDurableClientTimeout(Integer durableClientTimeout) {
		this.durableClientTimeout = durableClientTimeout;
	}

	/**
	 * Get the value of the GemFire System property 'durable-client-timeout' indicating to the server how long
	 * to track events for the durable client when disconnected.
	 *
	 * @return an Integer value indicating the timeout in seconds for the server to keep
	 * the durable client's queue around.
	 */
	public Integer getDurableClientTimeout() {
		return durableClientTimeout;
	}

	/* (non-Javadoc) */
	@Override
	public final void setEnableAutoReconnect(final Boolean enableAutoReconnect) {
		throw new UnsupportedOperationException("Auto-reconnect does not apply to clients.");
	}

	/* (non-Javadoc) */
	@Override
	public final Boolean getEnableAutoReconnect() {
		return Boolean.FALSE;
	}

	/* (non-Javadoc) */
	public void setFreeConnectionTimeout(Integer freeConnectionTimeout) {
		this.freeConnectionTimeout = freeConnectionTimeout;
	}

	/* (non-Javadoc) */
	public Integer getFreeConnectionTimeout() {
		return freeConnectionTimeout;
	}

	/* (non-Javadoc) */
	public void setIdleTimeout(Long idleTimeout) {
		this.idleTimeout = idleTimeout;
	}

	/* (non-Javadoc) */
	public Long getIdleTimeout() {
		return idleTimeout;
	}

	/**
	 * Sets whether the server(s) should keep the durable client's queue alive for the duration of the timeout
	 * when the client voluntarily disconnects.
	 *
	 * @param keepAlive a boolean value indicating to the server to keep the durable client's queues alive.
	 */
	public void setKeepAlive(Boolean keepAlive) {
		this.keepAlive = keepAlive;
	}

	/**
	 * Gets the user specified value for whether the server(s) should keep the durable client's queue alive
	 * for the duration of the timeout when the client voluntarily disconnects.
	 *
	 * @return a boolean value indicating whether the server should keep the durable client's queues alive.
	 */
	public Boolean getKeepAlive() {
		return keepAlive;
	}

	/**
	 * Determines whether the server(s) should keep the durable client's queue alive for the duration of the timeout
	 * when the client voluntarily disconnects.
	 *
	 * @return a boolean value indicating whether the server should keep the durable client's queues alive.
	 */
	public boolean isKeepAlive() {
		return Boolean.TRUE.equals(getKeepAlive());
	}

	/* (non-Javadoc) */
	public void setLoadConditioningInterval(Integer loadConditioningInterval) {
		this.loadConditioningInterval = loadConditioningInterval;
	}

	/* (non-Javadoc) */
	public Integer getLoadConditioningInterval() {
		return loadConditioningInterval;
	}

	/* (non-Javadoc) */
	public void setLocators(ConnectionEndpoint[] locators) {
		setLocators(ConnectionEndpointList.from(locators));
	}

	/* (non-Javadoc) */
	public void setLocators(Iterable<ConnectionEndpoint> locators) {
		getLocators().clear();
		addLocators(locators);
	}

	/* (non-Javadoc) */
	protected ConnectionEndpointList getLocators() {
		return locators;
	}

	/* (non-Javadoc) */
	public void setMaxConnections(Integer maxConnections) {
		this.maxConnections = maxConnections;
	}

	/* (non-Javadoc) */
	public Integer getMaxConnections() {
		return maxConnections;
	}

	/* (non-Javadoc) */
	public void setMinConnections(Integer minConnections) {
		this.minConnections = minConnections;
	}

	/* (non-Javadoc) */
	public Integer getMinConnections() {
		return minConnections;
	}

	/* (non-Javadoc) */
	public void setMultiUserAuthentication(Boolean multiUserAuthentication) {
		this.multiUserAuthentication = multiUserAuthentication;
	}

	/* (non-Javadoc) */
	public Boolean getMultiUserAuthentication() {
		return multiUserAuthentication;
	}

	/**
	 * Sets the {@link Pool} used by this cache client to obtain connections to the GemFire cluster.
	 *
	 * @param pool the GemFire {@link Pool} used by this {@link ClientCache} to obtain connections
	 * to the GemFire cluster.
	 * @throws IllegalArgumentException if the {@link Pool} is null.
	 */
	public void setPool(Pool pool) {
		this.pool = pool;
	}

	/**
	 * Gets the {@link Pool} used by this cache client to obtain connections to the GemFire cluster.
	 *
	 * @return the GemFire {@link Pool} used by this {@link ClientCache} to obtain connections
	 * to the GemFire cluster.
	 */
	public Pool getPool() {
		return pool;
	}

	/**
	 * Sets the name of the {@link Pool} used by this cache client to obtain connections to the GemFire cluster.
	 *
	 * @param poolName set the name of the GemFire {@link Pool} used by this GemFire {@link ClientCache}.
	 * @throws IllegalArgumentException if the {@link Pool} name is unspecified.
	 */
	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	/**
	 * Gets the name of the GemFire {@link Pool} used by this GemFire cache client.
	 *
	 * @return the name of the GemFire {@link Pool} used by this GemFire cache client.
	 */
	public String getPoolName() {
		return poolName;
	}

	/* (non-Javadoc) */
	public void setPingInterval(Long pingInterval) {
		this.pingInterval = pingInterval;
	}

	/* (non-Javadoc) */
	public Long getPingInterval() {
		return pingInterval;
	}

	/* (non-Javadoc) */
	public void setPrSingleHopEnabled(Boolean prSingleHopEnabled) {
		this.prSingleHopEnabled = prSingleHopEnabled;
	}

	/* (non-Javadoc) */
	public Boolean getPrSingleHopEnabled() {
		return prSingleHopEnabled;
	}

	/* (non-Javadoc) */
	public void setReadTimeout(Integer readTimeout) {
		this.readTimeout = readTimeout;
	}

	/* (non-Javadoc) */
	public Integer getReadTimeout() {
		return readTimeout;
	}

	/**
	 * Sets the readyForEvents property to indicate whether the cache client should notify the server
	 * that it is ready to receive updates.
	 *
	 * @param readyForEvents sets a boolean flag to notify the server that this durable client
	 * is ready to receive updates.
	 * @see #getReadyForEvents()
	 */
	public void setReadyForEvents(Boolean readyForEvents){
		this.readyForEvents = readyForEvents;
	}

	/**
	 * Gets the user-specified value for the readyForEvents property.
	 *
	 * @return a boolean value indicating the state of the 'readyForEvents' property.
	 */
	public Boolean getReadyForEvents(){
		return readyForEvents;
	}

	/**
	 * Determines whether this GemFire cache client is ready for events.  If 'readyForEvents' was explicitly set,
	 * then it takes precedence over all other considerations (e.g. durability).
	 *
	 * @return a boolean value indicating whether this GemFire cache client is ready for events.
	 * @see org.springframework.data.gemfire.GemfireUtils#isDurable(ClientCache)
	 * @see #getReadyForEvents()
	 */
	public boolean isReadyForEvents() {
		Boolean readyForEvents = getReadyForEvents();

		if (readyForEvents != null) {
			return Boolean.TRUE.equals(readyForEvents);
		}
		else {
			try {
				return GemfireUtils.isDurable((ClientCache) fetchCache());
			}
			catch (Throwable ignore) {
				return false;
			}
		}
	}

	/* (non-Javadoc) */
	public void setRetryAttempts(Integer retryAttempts) {
		this.retryAttempts = retryAttempts;
	}

	/* (non-Javadoc) */
	public Integer getRetryAttempts() {
		return retryAttempts;
	}

	/* (non-Javadoc) */
	public void setServerGroup(String serverGroup) {
		this.serverGroup = serverGroup;
	}

	/* (non-Javadoc) */
	public String getServerGroup() {
		return serverGroup;
	}

	/* (non-Javadoc) */
	public void setServers(ConnectionEndpoint[] servers) {
		setServers(ConnectionEndpointList.from(servers));
	}

	/* (non-Javadoc) */
	public void setServers(Iterable<ConnectionEndpoint> servers) {
		getServers().clear();
		addServers(servers);
	}

	/* (non-Javadoc) */
	protected ConnectionEndpointList getServers() {
		return servers;
	}

	/* (non-Javadoc) */
	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	/* (non-Javadoc) */
	public Integer getSocketBufferSize() {
		return socketBufferSize;
	}

	/* (non-Javadoc) */
	public void setStatisticsInterval(Integer statisticsInterval) {
		this.statisticsInterval = statisticsInterval;
	}

	/* (non-Javadoc) */
	public Integer getStatisticsInterval() {
		return statisticsInterval;
	}

	/* (non-Javadoc) */
	public void setSubscriptionAckInterval(Integer subscriptionAckInterval) {
		this.subscriptionAckInterval = subscriptionAckInterval;
	}

	/* (non-Javadoc) */
	public Integer getSubscriptionAckInterval() {
		return subscriptionAckInterval;
	}

	/* (non-Javadoc) */
	public void setSubscriptionEnabled(Boolean subscriptionEnabled) {
		this.subscriptionEnabled = subscriptionEnabled;
	}

	/* (non-Javadoc) */
	public Boolean getSubscriptionEnabled() {
		return subscriptionEnabled;
	}

	/* (non-Javadoc) */
	public void setSubscriptionMessageTrackingTimeout(Integer subscriptionMessageTrackingTimeout) {
		this.subscriptionMessageTrackingTimeout = subscriptionMessageTrackingTimeout;
	}

	/* (non-Javadoc) */
	public Integer getSubscriptionMessageTrackingTimeout() {
		return subscriptionMessageTrackingTimeout;
	}

	/* (non-Javadoc) */
	public void setSubscriptionRedundancy(Integer subscriptionRedundancy) {
		this.subscriptionRedundancy = subscriptionRedundancy;
	}

	/* (non-Javadoc) */
	public Integer getSubscriptionRedundancy() {
		return subscriptionRedundancy;
	}

	/* (non-Javadoc) */
	public void setThreadLocalConnections(Boolean threadLocalConnections) {
		this.threadLocalConnections = threadLocalConnections;
	}

	/* (non-Javadoc) */
	public Boolean getThreadLocalConnections() {
		return threadLocalConnections;
	}

	/* (non-Javadoc) */
	@Override
	public final void setUseClusterConfiguration(Boolean useClusterConfiguration) {
		throw new UnsupportedOperationException("Shared, cluster-based configuration is not applicable for clients.");
	}

	/* (non-Javadoc) */
	@Override
	public final Boolean getUseClusterConfiguration() {
		return Boolean.FALSE;
	}
}
