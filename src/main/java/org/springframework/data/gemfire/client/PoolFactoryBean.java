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
import java.util.Collection;
import java.util.Collections;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.distributed.internal.InternalDistributedSystem;

/**
 * FactoryBean for easy declaration and configuration of a GemFire Pool. If a new Pool is created,
 * its lifecycle is bound to that of the declaring container.
 *
 * Note, if the Pool already exists, the existing Pool will be returned as is without any modifications
 * and its lifecycle will be unaffected by this factory.
 *
 * @author Costin Leau
 * @author John Blum
 * @see com.gemstone.gemfire.cache.client.Pool
 * @see com.gemstone.gemfire.cache.client.PoolFactory
 * @see com.gemstone.gemfire.cache.client.PoolManager
 */
@SuppressWarnings("unused")
public class PoolFactoryBean implements FactoryBean<Pool>, InitializingBean, DisposableBean, BeanNameAware,
		BeanFactoryAware {

	private static final Log log = LogFactory.getLog(PoolFactoryBean.class);

	// indicates whether the Pool has been created internally (by this FactoryBean) or not
	private volatile boolean internalPool = true;

	private BeanFactory beanFactory;

	private Collection<InetSocketAddress> locators;
	private Collection<InetSocketAddress> servers;

	private Pool pool;

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

	public Pool getObject() throws Exception {
		return pool;
	}

	public Class<?> getObjectType() {
		return (pool != null ? pool.getClass() : Pool.class);
	}

	public boolean isSingleton() {
		return true;
	}

	public void afterPropertiesSet() throws Exception {
		if (!StringUtils.hasText(name)) {
			Assert.hasText(beanName, "The Pool name is required!");
			name = beanName;
		}

		// first check the configured pools
		Pool existingPool = PoolManager.find(name);

		if (existingPool != null) {
			if (log.isDebugEnabled()) {
				log.debug(String.format("A Pool with name '%1$s' already exists; using existing Pool.", name));
			}

			internalPool = false;
			pool = existingPool;
		}
		else {
			if (log.isDebugEnabled()) {
				log.debug(String.format("No Pool with name '%1$s' was found. Creating a new Pool...", name));
			}

			if (CollectionUtils.isEmpty(locators) && CollectionUtils.isEmpty(servers)) {
				throw new IllegalArgumentException("At least one locator or server is required!");
			}

			internalPool = true;

			PoolFactory poolFactory = PoolManager.createFactory();

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

			for (InetSocketAddress connection : nullSafeCollection(locators)) {
				poolFactory.addLocator(connection.getHostName(), connection.getPort());
			}

			for (InetSocketAddress connection : nullSafeCollection(servers)) {
				poolFactory.addServer(connection.getHostName(), connection.getPort());
			}

			// eagerly initialize ClientCache (if needed)
			if (InternalDistributedSystem.getAnyInstance() == null) {
				doDistributedSystemConnect(resolveGemfireProperties());
			}

			pool = poolFactory.create(name);
		}
	}

	public void destroy() throws Exception {
		if (internalPool && pool != null) {
			if (!pool.isDestroyed()) {
				pool.releaseThreadLocalConnection();
				pool.destroy(keepAlive);

				if (log.isDebugEnabled()) {
					log.debug(String.format("Destroyed Pool '%1$s'.", name));
				}
			}
		}
	}

	/**
	 * A workaround to create a Pool if no ClientCache has been created yet. Initialize a client-like
	 * Distributed System before initializing the Pool.
	 *
	 * @param properties GemFire System Properties.
	 * @see java.util.Properties
	 * @see com.gemstone.gemfire.distributed.DistributedSystem#connect(java.util.Properties)
	 */
	@SuppressWarnings("deprecation")
	static void doDistributedSystemConnect(Properties properties) {
		Properties gemfireProperties = (properties != null ? (Properties) properties.clone() : new Properties());
		gemfireProperties.setProperty("locators", "");
		gemfireProperties.setProperty("mcast-port", "0");
		DistributedSystem.connect(gemfireProperties);
	}

	/* (non-Javadoc) */
	private <T> Collection<T> nullSafeCollection(final Collection<T> list) {
		return (list != null ? list : Collections.<T>emptyList());
	}

	/* (non-Javadoc) */
	private Properties resolveGemfireProperties() {
		try {
			ClientCacheFactoryBean clientCacheFactoryBean = beanFactory.getBean(ClientCacheFactoryBean.class);
			return clientCacheFactoryBean.getProperties();
		}
		catch (Exception ignore) {
			return null;
		}
	}

	public void setBeanFactory(BeanFactory beanFactory) {
		this.beanFactory = beanFactory;
	}

	public void setBeanName(String name) {
		this.beanName = name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setPool(Pool pool) {
		this.pool = pool;
	}

	public void setFreeConnectionTimeout(int freeConnectionTimeout) {
		this.freeConnectionTimeout = freeConnectionTimeout;
	}

	public void setIdleTimeout(long idleTimeout) {
		this.idleTimeout = idleTimeout;
	}

	public void setKeepAlive(boolean keepAlive) {
		this.keepAlive = keepAlive;
	}

	public void setLoadConditioningInterval(int loadConditioningInterval) {
		this.loadConditioningInterval = loadConditioningInterval;
	}

	public void setLocators(Collection<InetSocketAddress> locators) {
		this.locators = locators;
	}

	public void setMaxConnections(int maxConnections) {
		this.maxConnections = maxConnections;
	}

	public void setMinConnections(int minConnections) {
		this.minConnections = minConnections;
	}

	public void setMultiUserAuthentication(boolean multiUserAuthentication) {
		this.multiUserAuthentication = multiUserAuthentication;
	}

	public void setPingInterval(long pingInterval) {
		this.pingInterval = pingInterval;
	}

	public void setReadTimeout(int readTimeout) {
		this.readTimeout = readTimeout;
	}

	public void setRetryAttempts(int retryAttempts) {
		this.retryAttempts = retryAttempts;
	}

	public void setServerGroup(String serverGroup) {
		this.serverGroup = serverGroup;
	}

	public void setServers(Collection<InetSocketAddress> servers) {
		this.servers = servers;
	}

	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public void setStatisticInterval(int statisticInterval) {
		this.statisticInterval = statisticInterval;
	}

	public void setPrSingleHopEnabled(boolean prSingleHopEnabled) {
		this.prSingleHopEnabled = prSingleHopEnabled;
	}

	public void setSubscriptionAckInterval(int subscriptionAckInterval) {
		this.subscriptionAckInterval = subscriptionAckInterval;
	}

	public void setSubscriptionEnabled(boolean subscriptionEnabled) {
		this.subscriptionEnabled = subscriptionEnabled;
	}

	public void setSubscriptionMessageTrackingTimeout(
		int subscriptionMessageTrackingTimeout) {
		this.subscriptionMessageTrackingTimeout = subscriptionMessageTrackingTimeout;
	}

	public void setSubscriptionRedundancy(int subscriptionRedundancy) {
		this.subscriptionRedundancy = subscriptionRedundancy;
	}

	public void setThreadLocalConnections(boolean threadLocalConnections) {
		this.threadLocalConnections = threadLocalConnections;
	}

}
