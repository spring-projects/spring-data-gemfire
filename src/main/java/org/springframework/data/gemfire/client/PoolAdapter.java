/*
 * Copyright 2012-2018 the original author or authors.
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

package org.springframework.data.gemfire.client;

import java.net.InetSocketAddress;
import java.util.List;

import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.query.QueryService;

/**
 * The {@link PoolAdapter} class is an abstract base class and default, no-op implementation of
 * the {@link Pool} interface that conveniently enables implementing classes to extend this adapter
 * and choose which {@link Pool} methods/operations are supported by this implementation.
 *
 * For instance, one possible implementation is Spring Data GemFire's {@link PoolFactoryBean}, which can act as
 * a {@link Pool} in a context where only the {@link Pool}'s "configuration" and meta-data are required,
 * but no actual connections or operating state information (e.g. pendingEventCount) is needed.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see org.apache.geode.cache.client.Pool
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public abstract class PoolAdapter implements Pool {

	public static final String NOT_IMPLEMENTED = "Not Implemented";

	public boolean isDestroyed() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getFreeConnectionTimeout() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public long getIdleTimeout() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getLoadConditioningInterval() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public List<InetSocketAddress> getLocators() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getMaxConnections() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getMinConnections() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public boolean getMultiuserAuthentication() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public String getName() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	@Override
	public List<InetSocketAddress> getOnlineLocators() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getPendingEventCount() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public long getPingInterval() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public boolean getPRSingleHopEnabled() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public QueryService getQueryService() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getReadTimeout() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getRetryAttempts() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public String getServerGroup() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public List<InetSocketAddress> getServers() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getSocketBufferSize() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getSocketConnectTimeout() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getStatisticInterval() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getSubscriptionAckInterval() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public boolean getSubscriptionEnabled() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getSubscriptionMessageTrackingTimeout() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public int getSubscriptionRedundancy() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public boolean getThreadLocalConnections() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public void destroy() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public void destroy(boolean keepAlive) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	public void releaseThreadLocalConnection() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}
}
