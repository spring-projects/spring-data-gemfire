/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.client.support;

import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.List;

import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.query.QueryService;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.client.PoolAdapter;

/**
 * {@link FactoryDefaultsPoolAdapter} is an abstract implementation of the {@link Pool} interface and extension of
 * {@link PoolAdapter} that provides default factory values for all configuration properties
 * (e.g. freeConnectionTimeout, idleTimeout, etc).
 *
 * @author John Blum
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.client.PoolFactory
 * @see org.springframework.data.gemfire.client.PoolAdapter
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public abstract class FactoryDefaultsPoolAdapter extends PoolAdapter {

	protected static final boolean DEFAULT_KEEP_ALIVE = false;

	protected static final String DEFAULT_POOL_NAME = "DEFAULT";
	protected static final String LOCALHOST = "localhost";

	@Override
	public int getFreeConnectionTimeout() {
		return PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT;
	}

	@Override
	public long getIdleTimeout() {
		return PoolFactory.DEFAULT_IDLE_TIMEOUT;
	}

	@Override
	public int getLoadConditioningInterval() {
		return PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL;
	}

	@Override
	public List<InetSocketAddress> getLocators() {
		return Collections.emptyList();
	}

	@Override
	public int getMaxConnections() {
		return PoolFactory.DEFAULT_MAX_CONNECTIONS;
	}

	@Override
	public int getMinConnections() {
		return PoolFactory.DEFAULT_MIN_CONNECTIONS;
	}

	@Override
	public boolean getMultiuserAuthentication() {
		return PoolFactory.DEFAULT_MULTIUSER_AUTHENTICATION;
	}

	@Override
	public String getName() {
		return DEFAULT_POOL_NAME;
	}

	@Override
	public List<InetSocketAddress> getOnlineLocators() {
		return Collections.emptyList();
	}

	@Override
	public long getPingInterval() {
		return PoolFactory.DEFAULT_PING_INTERVAL;
	}

	@Override
	public boolean getPRSingleHopEnabled() {
		return PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED;
	}

	@Override
	public QueryService getQueryService() {
		return null;
	}

	@Override
	public int getReadTimeout() {
		return PoolFactory.DEFAULT_READ_TIMEOUT;
	}

	@Override
	public int getRetryAttempts() {
		return PoolFactory.DEFAULT_RETRY_ATTEMPTS;
	}

	@Override
	public String getServerGroup() {
		return PoolFactory.DEFAULT_SERVER_GROUP;
	}

	@Override
	public List<InetSocketAddress> getServers() {
		return Collections.singletonList(new InetSocketAddress(LOCALHOST, GemfireUtils.DEFAULT_CACHE_SERVER_PORT));
	}

	@Override
	public int getSocketBufferSize() {
		return PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE;
	}

	@Override
	public int getSocketConnectTimeout() {
		return PoolFactory.DEFAULT_SOCKET_CONNECT_TIMEOUT;
	}

	@Override
	public int getStatisticInterval() {
		return PoolFactory.DEFAULT_STATISTIC_INTERVAL;
	}

	@Override
	public int getSubscriptionAckInterval() {
		return PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL;
	}

	@Override
	public boolean getSubscriptionEnabled() {
		return PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED;
	}

	@Override
	public int getSubscriptionMessageTrackingTimeout() {
		return PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT;
	}

	@Override
	public int getSubscriptionRedundancy() {
		return PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY;
	}

	@Override
	public int getSubscriptionTimeoutMultiplier() {
		return PoolFactory.DEFAULT_SUBSCRIPTION_TIMEOUT_MULTIPLIER;
	}

	@Override
	public boolean getThreadLocalConnections() {
		return PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS;
	}

	public void destroy() {
		destroy(DEFAULT_KEEP_ALIVE);
	}
}
