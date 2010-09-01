/*
 * Copyright 2010 the original author or authors.
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

import java.util.Collection;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
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

/**
 * Factory bean for easy declaration and configuration of a GemFire pool.
 * If a new pool is created, its life-cycle is bound to that of the declaring container.
 * 
 * Note that if the pool already exists, it will be returned as is, without any 
 * modifications and its life cycle untouched by this factory.
 * 
 * @see PoolManager
 * @see PoolFactory
 * @see Pool
 * 
 * @author Costin Leau
 */
public class PoolFactoryBean implements FactoryBean<Pool>, InitializingBean, DisposableBean, BeanNameAware {

	private static final Log log = LogFactory.getLog(PoolFactoryBean.class);

	// whether the pool has been created internaly or not
	private boolean internalPool = true;

	private Pool pool;

	// pool settings
	private String beanName;
	private String name;
	private Collection<PoolConnection> locators;
	private Collection<PoolConnection> servers;
	private boolean keepAlive = false;

	private int freeConnectionTimeout = PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT;
	private long idleTimeout = PoolFactory.DEFAULT_IDLE_TIMEOUT;
	private int loadConditioningInterval = PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL;
	private int maxConnections = PoolFactory.DEFAULT_MAX_CONNECTIONS;
	private int minConnections = PoolFactory.DEFAULT_MIN_CONNECTIONS;
	private long pingInterval = PoolFactory.DEFAULT_PING_INTERVAL;
	private int readTimeout = PoolFactory.DEFAULT_READ_TIMEOUT;
	private int retryAttempts = PoolFactory.DEFAULT_RETRY_ATTEMPTS;
	private String serverGroup = PoolFactory.DEFAULT_SERVER_GROUP;
	private int socketBufferSize = PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE;
	private int statisticInterval = PoolFactory.DEFAULT_STATISTIC_INTERVAL;
	private int subscriptionAckInterval = PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL;
	private boolean subscriptionEnabled = PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED;
	private int subscriptionMessageTrackingTimeout = PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT;
	private int subscriptionRedundancy = PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY;
	private boolean threadLocalConnections = PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS;

	public Class<?> getObjectType() {
		return Pool.class;
	}

	public boolean isSingleton() {
		return true;
	}

	public Pool getObject() throws Exception {
		return pool;
	}

	public void afterPropertiesSet() throws Exception {
		if (!StringUtils.hasText(name)) {
			Assert.hasText(beanName, "the pool name is required");
			name = beanName;
		}

		// first check the configured pools
		Pool existingPool = PoolManager.find(name);
		if (existingPool != null) {
			pool = existingPool;
			internalPool = false;
			if (log.isDebugEnabled())
				log.debug("Pool '" + name + " already exists; using found instance...");
		}
		else {
			if (log.isDebugEnabled())
				log.debug("No pool named '" + name + "' found. Creating a new once...");

			if (CollectionUtils.isEmpty(locators) && CollectionUtils.isEmpty(servers)) {
				throw new IllegalArgumentException("at least one locator or server is required");
			}

			internalPool = true;

			PoolFactory poolFactory = PoolManager.createFactory();

			if (!CollectionUtils.isEmpty(locators)) {
				for (PoolConnection connection : locators) {
					poolFactory.addLocator(connection.getHost(), connection.getPort());
				}
			}

			if (!CollectionUtils.isEmpty(servers)) {
				for (PoolConnection connection : servers) {
					poolFactory.addServer(connection.getHost(), connection.getPort());
				}
			}

			poolFactory.setFreeConnectionTimeout(freeConnectionTimeout);
			poolFactory.setIdleTimeout(idleTimeout);
			poolFactory.setLoadConditioningInterval(loadConditioningInterval);
			poolFactory.setMaxConnections(maxConnections);
			poolFactory.setMinConnections(minConnections);
			poolFactory.setPingInterval(pingInterval);
			poolFactory.setReadTimeout(readTimeout);
			poolFactory.setRetryAttempts(retryAttempts);
			poolFactory.setServerGroup(serverGroup);
			poolFactory.setSocketBufferSize(socketBufferSize);
			poolFactory.setStatisticInterval(statisticInterval);
			poolFactory.setSubscriptionEnabled(subscriptionEnabled);
			poolFactory.setSubscriptionAckInterval(subscriptionAckInterval);
			poolFactory.setSubscriptionMessageTrackingTimeout(subscriptionMessageTrackingTimeout);
			poolFactory.setSubscriptionRedundancy(subscriptionRedundancy);
			poolFactory.setThreadLocalConnections(threadLocalConnections);

			pool = poolFactory.create(name);
		}
	}

	public void destroy() throws Exception {
		if (internalPool && pool != null) {
			if (!pool.isDestroyed()) {
				pool.releaseThreadLocalConnection();
				pool.destroy(keepAlive);
				if (log.isDebugEnabled())
					log.debug("Destroyed pool '" + name + "'...");
			}
		}
	}

	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * @param pool the pool to set
	 */
	public void setPool(Pool pool) {
		this.pool = pool;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @param locators the locators to set
	 */
	public void setLocators(Collection<PoolConnection> locators) {
		this.locators = locators;
	}

	/**
	 * @param servers the servers to set
	 */
	public void setServers(Collection<PoolConnection> servers) {
		this.servers = servers;
	}

	/**
	 * @param keepAlive the keepAlive to set
	 */
	public void setKeepAlive(boolean keepAlive) {
		this.keepAlive = keepAlive;
	}

	/**
	 * @param freeConnectionTimeout the freeConnectionTimeout to set
	 */
	public void setFreeConnectionTimeout(int freeConnectionTimeout) {
		this.freeConnectionTimeout = freeConnectionTimeout;
	}

	/**
	 * @param idleTimeout the idleTimeout to set
	 */
	public void setIdleTimeout(long idleTimeout) {
		this.idleTimeout = idleTimeout;
	}

	/**
	 * @param loadConditioningInterval the loadConditioningInterval to set
	 */
	public void setLoadConditioningInterval(int loadConditioningInterval) {
		this.loadConditioningInterval = loadConditioningInterval;
	}

	/**
	 * @param maxConnections the maxConnections to set
	 */
	public void setMaxConnections(int maxConnections) {
		this.maxConnections = maxConnections;
	}

	/**
	 * @param minConnections the minConnections to set
	 */
	public void setMinConnections(int minConnections) {
		this.minConnections = minConnections;
	}

	/**
	 * @param pingInterval the pingInterval to set
	 */
	public void setPingInterval(long pingInterval) {
		this.pingInterval = pingInterval;
	}

	/**
	 * @param readTimeout the readTimeout to set
	 */
	public void setReadTimeout(int readTimeout) {
		this.readTimeout = readTimeout;
	}

	/**
	 * @param retryAttempts the retryAttempts to set
	 */
	public void setRetryAttempts(int retryAttempts) {
		this.retryAttempts = retryAttempts;
	}

	/**
	 * @param serverGroup the serverGroup to set
	 */
	public void setServerGroup(String serverGroup) {
		this.serverGroup = serverGroup;
	}

	/**
	 * @param socketBufferSize the socketBufferSize to set
	 */
	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	/**
	 * @param statisticInterval the statisticInterval to set
	 */
	public void setStatisticInterval(int statisticInterval) {
		this.statisticInterval = statisticInterval;
	}

	/**
	 * @param subscriptionAckInterval the subscriptionAckInterval to set
	 */
	public void setSubscriptionAckInterval(int subscriptionAckInterval) {
		this.subscriptionAckInterval = subscriptionAckInterval;
	}

	/**
	 * @param subscriptionEnabled the subscriptionEnabled to set
	 */
	public void setSubscriptionEnabled(boolean subscriptionEnabled) {
		this.subscriptionEnabled = subscriptionEnabled;
	}

	/**
	 * @param subscriptionMessageTrackingTimeout the subscriptionMessageTrackingTimeout to set
	 */
	public void setSubscriptionMessageTrackingTimeout(int subscriptionMessageTrackingTimeout) {
		this.subscriptionMessageTrackingTimeout = subscriptionMessageTrackingTimeout;
	}

	/**
	 * @param subscriptionRedundancy the subscriptionRedundancy to set
	 */
	public void setSubscriptionRedundancy(int subscriptionRedundancy) {
		this.subscriptionRedundancy = subscriptionRedundancy;
	}

	/**
	 * @param threadLocalConnections the threadLocalConnections to set
	 */
	public void setThreadLocalConnections(boolean threadLocalConnections) {
		this.threadLocalConnections = threadLocalConnections;
	}
}