/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.client.support;

import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.List;

import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.query.QueryService;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.client.PoolAdapter;

/**
 * FactoryDefaultsPoolAdapter is an abstract implementation of GemFire's {@link org.apache.geode.cache.client.Pool}
 * interface and extension of {@link PoolAdapter} providing default factory values for all configuration properties
 * (e.g. freeConnectionTimeout, idleTimeout, etc).
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.client.PoolAdapter
 * @see org.apache.geode.cache.client.PoolFactory
 * @see org.apache.geode.cache.client.Pool
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public abstract class FactoryDefaultsPoolAdapter extends PoolAdapter {

	protected static final boolean DEFAULT_KEEP_ALIVE = false;

	protected static final String DEFAULT_POOL_NAME = "DEFAULT";
	protected static final String LOCALHOST = "localhost";

	/* (non-Javadoc) */
	@Override
	public int getFreeConnectionTimeout() {
		return PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT;
	}

	/* (non-Javadoc) */
	@Override
	public long getIdleTimeout() {
		return PoolFactory.DEFAULT_IDLE_TIMEOUT;
	}

	/* (non-Javadoc) */
	@Override
	public int getLoadConditioningInterval() {
		return PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL;
	}

	/* (non-Javadoc) */
	@Override
	public List<InetSocketAddress> getLocators() {
		return Collections.emptyList();
	}

	/* (non-Javadoc) */
	@Override
	public int getMaxConnections() {
		return PoolFactory.DEFAULT_MAX_CONNECTIONS;
	}

	/* (non-Javadoc) */
	@Override
	public int getMinConnections() {
		return PoolFactory.DEFAULT_MIN_CONNECTIONS;
	}

	/* (non-Javadoc) */
	@Override
	public boolean getMultiuserAuthentication() {
		return PoolFactory.DEFAULT_MULTIUSER_AUTHENTICATION;
	}

	/* (non-Javadoc) */
	@Override
	public String getName() {
		return DEFAULT_POOL_NAME;
	}

	/* (non-Javadoc) */
	@Override
	public List<InetSocketAddress> getOnlineLocators() {
		return Collections.emptyList();
	}

	/* (non-Javadoc) */
	@Override
	public long getPingInterval() {
		return PoolFactory.DEFAULT_PING_INTERVAL;
	}

	/* (non-Javadoc) */
	@Override
	public boolean getPRSingleHopEnabled() {
		return PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED;
	}

	/* (non-Javadoc) */
	@Override
	public QueryService getQueryService() {
		return null;
	}

	/* (non-Javadoc) */
	@Override
	public int getReadTimeout() {
		return PoolFactory.DEFAULT_READ_TIMEOUT;
	}

	/* (non-Javadoc) */
	@Override
	public int getRetryAttempts() {
		return PoolFactory.DEFAULT_RETRY_ATTEMPTS;
	}

	/* (non-Javadoc) */
	@Override
	public String getServerGroup() {
		return PoolFactory.DEFAULT_SERVER_GROUP;
	}

	/* (non-Javadoc) */
	@Override
	public List<InetSocketAddress> getServers() {
		return Collections.singletonList(new InetSocketAddress(LOCALHOST, GemfireUtils.DEFAULT_CACHE_SERVER_PORT));
	}

	/* (non-Javadoc) */
	@Override
	public int getSocketBufferSize() {
		return PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE;
	}

	/* (non-Javadoc) */
	@Override
	public int getStatisticInterval() {
		return PoolFactory.DEFAULT_STATISTIC_INTERVAL;
	}

	/* (non-Javadoc) */
	@Override
	public int getSubscriptionAckInterval() {
		return PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL;
	}

	/* (non-Javadoc) */
	@Override
	public boolean getSubscriptionEnabled() {
		return PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED;
	}

	/* (non-Javadoc) */
	@Override
	public int getSubscriptionMessageTrackingTimeout() {
		return PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT;
	}

	/* (non-Javadoc) */
	@Override
	public int getSubscriptionRedundancy() {
		return PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY;
	}

	/* (non-Javadoc) */
	@Override
	public boolean getThreadLocalConnections() {
		return PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS;
	}

	/* (non-Javadoc) */
	public void destroy() {
		destroy(DEFAULT_KEEP_ALIVE);
	}
}
