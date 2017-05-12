/*
 * Copyright 2010-2013 the original author or authors.
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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.client.PoolManager;
import org.apache.geode.cache.query.QueryService;
import org.apache.geode.distributed.DistributedSystem;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.util.DistributedSystemUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * FactoryBean for easy declaration and configuration of a GemFire {@link Pool}. If a new {@link Pool} is created,
 * its lifecycle is bound to that of this declaring factory.
 *
 * Note, if a {@link Pool} having the configured name already exists, then the existing {@link Pool} will be returned
 * as is without any modifications and its lifecycle will be unaffected by this factory.
 *
 * @author Costin Leau
 * @author John Blum
 * @see java.net.InetSocketAddress
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @see org.springframework.data.gemfire.support.ConnectionEndpointList
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.client.PoolFactory
 * @see org.apache.geode.cache.client.PoolManager
 */
@SuppressWarnings("unused")
public class PoolFactoryBean implements FactoryBean<Pool>, InitializingBean, DisposableBean,
		BeanNameAware, BeanFactoryAware {

	protected static final int DEFAULT_LOCATOR_PORT = DistributedSystemUtils.DEFAULT_LOCATOR_PORT;
	protected static final int DEFAULT_SERVER_PORT = DistributedSystemUtils.DEFAULT_CACHE_SERVER_PORT;

	private static final Log log = LogFactory.getLog(PoolFactoryBean.class);

	// indicates whether the Pool has been created internally (by this FactoryBean) or not
	volatile boolean springBasedPool = true;

	private BeanFactory beanFactory;

	private ConnectionEndpointList locators = new ConnectionEndpointList();
	private ConnectionEndpointList servers = new ConnectionEndpointList();

	private volatile Pool pool;

	private String beanName;
	private String name;

	// GemFire Pool Configuration Settings
	private boolean keepAlive = false;
	private boolean multiUserAuthentication = PoolFactory.DEFAULT_MULTIUSER_AUTHENTICATION;
	private boolean prSingleHopEnabled = PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED;
	private boolean subscriptionEnabled = PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED;
	private boolean threadLocalConnections = PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS;

	private int freeConnectionTimeout = PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT;
	private int loadConditioningInterval = PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL;
	private int maxConnections = PoolFactory.DEFAULT_MAX_CONNECTIONS;
	private int minConnections = PoolFactory.DEFAULT_MIN_CONNECTIONS;
	private int readTimeout = PoolFactory.DEFAULT_READ_TIMEOUT;
	private int retryAttempts = PoolFactory.DEFAULT_RETRY_ATTEMPTS;
	private int socketBufferSize = PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE;
	private int statisticInterval = PoolFactory.DEFAULT_STATISTIC_INTERVAL;
	private int subscriptionAckInterval = PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL;
	private int subscriptionMessageTrackingTimeout = PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT;
	private int subscriptionRedundancy = PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY;

	private long idleTimeout = PoolFactory.DEFAULT_IDLE_TIMEOUT;
	private long pingInterval = PoolFactory.DEFAULT_PING_INTERVAL;

	private String serverGroup = PoolFactory.DEFAULT_SERVER_GROUP;

	/**
	 * Constructs and initializes a GemFire {@link Pool}.
	 *
	 * @throws Exception if the {@link Pool} creation and initialization fails.
	 * @see org.apache.geode.cache.client.Pool
	 * @see org.apache.geode.cache.client.PoolFactory
	 * @see org.apache.geode.cache.client.PoolManager
	 * @see #createPoolFactory()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		if (!StringUtils.hasText(name)) {
			Assert.hasText(beanName, "Pool 'name' is required");
			this.name = beanName;
		}

		// check for an existing, configured Pool with name first
		Pool existingPool = PoolManager.find(name);

		if (existingPool != null) {
			if (log.isDebugEnabled()) {
				log.debug(String.format("A Pool with name [%1$s] already exists; using existing Pool.", name));
			}

			this.springBasedPool = false;
			this.pool = existingPool;
		}
		else {
			if (log.isDebugEnabled()) {
				log.debug(String.format("No Pool with name [%1$s] was found. Creating new Pool.", name));
			}

			this.springBasedPool = true;
		}
	}

	/**
	 * Destroys the GemFire {@link Pool} if created by this {@link PoolFactoryBean} and releases all system resources
	 * used by the {@link Pool}.
	 *
	 * @throws Exception if the {@link Pool} destruction caused an error.
	 * @see DisposableBean#destroy()
	 */
	@Override
	public void destroy() throws Exception {
		if (springBasedPool && pool != null && !pool.isDestroyed()) {
			pool.releaseThreadLocalConnection();
			pool.destroy(keepAlive);
			pool = null;

			if (log.isDebugEnabled()) {
				log.debug(String.format("Destroyed Pool [%1$s]", name));
			}
		}
	}

	/* (non-Javadoc) */
	@Override
	public Pool getObject() throws Exception {
		if (this.pool == null) {
			eagerlyInitializeClientCacheIfNotPresent();

			PoolFactory poolFactory = createPoolFactory();

			poolFactory.setFreeConnectionTimeout(freeConnectionTimeout);
			poolFactory.setIdleTimeout(idleTimeout);
			poolFactory.setLoadConditioningInterval(loadConditioningInterval);
			poolFactory.setMaxConnections(maxConnections);
			poolFactory.setMinConnections(minConnections);
			poolFactory.setMultiuserAuthentication(multiUserAuthentication);
			poolFactory.setPingInterval(pingInterval);
			poolFactory.setPRSingleHopEnabled(prSingleHopEnabled);
			poolFactory.setReadTimeout(readTimeout);
			poolFactory.setRetryAttempts(retryAttempts);
			poolFactory.setServerGroup(serverGroup);
			poolFactory.setSocketBufferSize(socketBufferSize);
			poolFactory.setStatisticInterval(statisticInterval);
			poolFactory.setSubscriptionAckInterval(subscriptionAckInterval);
			poolFactory.setSubscriptionEnabled(subscriptionEnabled);
			poolFactory.setSubscriptionMessageTrackingTimeout(subscriptionMessageTrackingTimeout);
			poolFactory.setSubscriptionRedundancy(subscriptionRedundancy);
			poolFactory.setThreadLocalConnections(threadLocalConnections);

			for (ConnectionEndpoint locator : this.locators) {
				poolFactory.addLocator(locator.getHost(), locator.getPort());
			}

			for (ConnectionEndpoint server : this.servers) {
				poolFactory.addServer(server.getHost(), server.getPort());
			}

			pool = poolFactory.create(name);
		}

		return pool;
	}

	/**
	 * Determines whether the GemFire DistributedSystem exists yet or not.
	 *
	 * @return a boolean value indicating whether the single, GemFire DistributedSystem has been created already.
	 * @see org.springframework.data.gemfire.GemfireUtils#getDistributedSystem()
	 * @see org.springframework.data.gemfire.GemfireUtils#isConnected(DistributedSystem)
	 * @see org.apache.geode.distributed.DistributedSystem
	 */
	boolean isDistributedSystemPresent() {
		return GemfireUtils.isConnected(GemfireUtils.getDistributedSystem());
	}

	/**
	 * Attempts to eagerly initialize the GemFire {@link ClientCache} if not already present so that the single
	 * {@link org.apache.geode.distributed.DistributedSystem} will exists, which is required to create
	 * a {@link Pool} instance.
	 *
	 * @see org.springframework.beans.factory.BeanFactory#getBean(Class)
	 * @see org.apache.geode.cache.client.ClientCache
	 */
	void eagerlyInitializeClientCacheIfNotPresent() {
		if (!isDistributedSystemPresent()) {
			getBeanFactory().getBean(ClientCache.class);
		}
	}

	/**
	 * Creates an instance of the GemFire {@link PoolFactory} interface to construct, configure and initialize
	 * a GemFire {@link Pool}.
	 *
	 * @return a {@link PoolFactory} implementation to create a {@link Pool}.
	 * @see org.apache.geode.cache.client.PoolManager#createFactory()
	 * @see org.apache.geode.cache.client.PoolFactory
	 */
	protected PoolFactory createPoolFactory() {
		return PoolManager.createFactory();
	}

	/* (non-Javadoc) */
	@Override
	public Class<?> getObjectType() {
		return (this.pool != null ? this.pool.getClass() : Pool.class);
	}

	/* (non-Javadoc) */
	@Override
	public boolean isSingleton() {
		return true;
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

	/* (non-Javadoc) */
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/* (non-Javadoc) */
	protected BeanFactory getBeanFactory() {
		return beanFactory;
	}

	/* (non-Javadoc) */
	public void setBeanName(String name) {
		this.beanName = name;
	}

	/* (non-Javadoc) */
	public void setName(String name) {
		this.name = name;
	}

	/* (non-Javadoc) */
	String getName() {
		return name;
	}

	/* (non-Javadoc) */
	public void setPool(Pool pool) {
		this.pool = pool;
	}

	/* (non-Javadoc) */
	public Pool getPool() {
		return (pool != null ? pool : new PoolAdapter() {
			@Override
			public boolean isDestroyed() {
				Pool pool = PoolFactoryBean.this.pool;
				return (pool != null && pool.isDestroyed());
			}

			@Override
			public int getFreeConnectionTimeout() {
				return PoolFactoryBean.this.freeConnectionTimeout;
			}

			@Override
			public long getIdleTimeout() {
				return PoolFactoryBean.this.idleTimeout;
			}

			@Override
			public int getLoadConditioningInterval() {
				return PoolFactoryBean.this.loadConditioningInterval;
			}

			@Override
			public List<InetSocketAddress> getLocators() {
				return PoolFactoryBean.this.locators.toInetSocketAddresses();
			}

			@Override
			public int getMaxConnections() {
				return PoolFactoryBean.this.maxConnections;
			}

			@Override
			public int getMinConnections() {
				return PoolFactoryBean.this.minConnections;
			}

			@Override
			public boolean getMultiuserAuthentication() {
				return PoolFactoryBean.this.multiUserAuthentication;
			}

			@Override
			public String getName() {
				String name = PoolFactoryBean.this.name;
				name = (StringUtils.hasText(name) ? name : PoolFactoryBean.this.beanName);
				return name;
			}

			@Override
			public int getPendingEventCount() {
				Pool pool = PoolFactoryBean.this.pool;
				if (pool != null) {
					return pool.getPendingEventCount();
				}
				throw new IllegalStateException("The Pool is not initialized");
			}

			@Override
			public long getPingInterval() {
				return PoolFactoryBean.this.pingInterval;
			}

			@Override
			public boolean getPRSingleHopEnabled() {
				return PoolFactoryBean.this.prSingleHopEnabled;
			}

			@Override
			public QueryService getQueryService() {
				Pool pool = PoolFactoryBean.this.pool;
				if (pool != null) {
					return pool.getQueryService();
				}
				throw new IllegalStateException("The Pool is not initialized");
			}

			@Override
			public int getReadTimeout() {
				return PoolFactoryBean.this.readTimeout;
			}

			@Override
			public int getRetryAttempts() {
				return PoolFactoryBean.this.retryAttempts;
			}

			@Override
			public String getServerGroup() {
				return PoolFactoryBean.this.serverGroup;
			}

			@Override
			public List<InetSocketAddress> getServers() {
				return PoolFactoryBean.this.servers.toInetSocketAddresses();
			}

			@Override
			public int getSocketBufferSize() {
				return PoolFactoryBean.this.socketBufferSize;
			}

			@Override
			public int getStatisticInterval() {
				return PoolFactoryBean.this.statisticInterval;
			}

			@Override
			public int getSubscriptionAckInterval() {
				return PoolFactoryBean.this.subscriptionAckInterval;
			}

			@Override
			public boolean getSubscriptionEnabled() {
				return PoolFactoryBean.this.subscriptionEnabled;
			}

			@Override
			public int getSubscriptionMessageTrackingTimeout() {
				return PoolFactoryBean.this.subscriptionMessageTrackingTimeout;
			}

			@Override
			public int getSubscriptionRedundancy() {
				return PoolFactoryBean.this.subscriptionRedundancy;
			}

			@Override
			public boolean getThreadLocalConnections() {
				return PoolFactoryBean.this.threadLocalConnections;
			}

			@Override
			public void destroy() {
				destroy(false);
			}

			@Override
			public void destroy(final boolean keepAlive) {
				try {
					PoolFactoryBean.this.destroy();
				}
				catch (Exception ignore) {
					Pool pool = PoolFactoryBean.this.pool;
					if (pool != null) {
						pool.destroy(keepAlive);
					}
				}
			}

			@Override
			public void releaseThreadLocalConnection() {
				Pool pool = PoolFactoryBean.this.pool;
				if (pool != null) {
					pool.releaseThreadLocalConnection();
				}
				else {
					throw new IllegalStateException("The Pool is not initialized");
				}
			}
		});
	}

	/* (non-Javadoc) */
	public void setFreeConnectionTimeout(int freeConnectionTimeout) {
		this.freeConnectionTimeout = freeConnectionTimeout;
	}

	/* (non-Javadoc) */
	public void setIdleTimeout(long idleTimeout) {
		this.idleTimeout = idleTimeout;
	}

	/* (non-Javadoc) */
	public void setKeepAlive(boolean keepAlive) {
		this.keepAlive = keepAlive;
	}

	/* (non-Javadoc) */
	public void setLoadConditioningInterval(int loadConditioningInterval) {
		this.loadConditioningInterval = loadConditioningInterval;
	}

	/* (non-Javadoc) */
	public void setLocators(ConnectionEndpoint[] connectionEndpoints) {
		setLocators(ConnectionEndpointList.from(connectionEndpoints));
	}

	/* (non-Javadoc) */
	public void setLocators(Iterable<ConnectionEndpoint> connectionEndpoints) {
		getLocators().clear();
		getLocators().add(connectionEndpoints);
	}

	/* (non-Javadoc) */
	ConnectionEndpointList getLocators() {
		return locators;
	}

	/* (non-Javadoc) */
	public void setMaxConnections(int maxConnections) {
		this.maxConnections = maxConnections;
	}

	/* (non-Javadoc) */
	public void setMinConnections(int minConnections) {
		this.minConnections = minConnections;
	}

	/* (non-Javadoc) */
	public void setMultiUserAuthentication(boolean multiUserAuthentication) {
		this.multiUserAuthentication = multiUserAuthentication;
	}

	/* (non-Javadoc) */
	public void setPingInterval(long pingInterval) {
		this.pingInterval = pingInterval;
	}

	/* (non-Javadoc) */
	public void setPrSingleHopEnabled(boolean prSingleHopEnabled) {
		this.prSingleHopEnabled = prSingleHopEnabled;
	}

	/* (non-Javadoc) */
	public void setReadTimeout(int readTimeout) {
		this.readTimeout = readTimeout;
	}

	/* (non-Javadoc) */
	public void setRetryAttempts(int retryAttempts) {
		this.retryAttempts = retryAttempts;
	}

	/* (non-Javadoc) */
	public void setServerGroup(String serverGroup) {
		this.serverGroup = serverGroup;
	}

	/* (non-Javadoc) */
	public void setServers(ConnectionEndpoint[] connectionEndpoints) {
		setServers(ConnectionEndpointList.from(connectionEndpoints));
	}

	/* (non-Javadoc) */
	public void setServers(Iterable<ConnectionEndpoint> connectionEndpoints) {
		getServers().clear();
		getServers().add(connectionEndpoints);
	}

	/* (non-Javadoc) */
	ConnectionEndpointList getServers() {
		return servers;
	}

	/* (non-Javadoc) */
	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	/* (non-Javadoc) */
	public void setStatisticInterval(int statisticInterval) {
		this.statisticInterval = statisticInterval;
	}

	/* (non-Javadoc) */
	public void setSubscriptionAckInterval(int subscriptionAckInterval) {
		this.subscriptionAckInterval = subscriptionAckInterval;
	}

	/* (non-Javadoc) */
	public void setSubscriptionEnabled(boolean subscriptionEnabled) {
		this.subscriptionEnabled = subscriptionEnabled;
	}

	/* (non-Javadoc) */
	public void setSubscriptionMessageTrackingTimeout(int subscriptionMessageTrackingTimeout) {
		this.subscriptionMessageTrackingTimeout = subscriptionMessageTrackingTimeout;
	}

	/* (non-Javadoc) */
	public void setSubscriptionRedundancy(int subscriptionRedundancy) {
		this.subscriptionRedundancy = subscriptionRedundancy;
	}

	/* (non-Javadoc) */
	public void setThreadLocalConnections(boolean threadLocalConnections) {
		this.threadLocalConnections = threadLocalConnections;
	}

	/* (non-Javadoc; internal framework use only) */
	public final void setLocatorsConfiguration(Object locatorsConfiguration) {
	}

	/* (non-Javadoc; internal framework use only) */
	public final void setServersConfiguration(Object serversConfiguration) {
	}
}
