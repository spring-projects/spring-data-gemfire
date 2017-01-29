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

package org.springframework.data.gemfire.client.support;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.query.QueryService;

/**
 * DelegatingPoolAdapter is an abstract implementation of GemFire's {@link Pool} interface and extension of
 * {@link FactoryDefaultsPoolAdapter} that delegates operations to the provided {@link Pool} instance.
 *
 * However, this implementation guards against a potentially <code>null</code> {@link Pool} reference by returning
 * default factory settings for the {@link Pool}'s configuration properties along with default behavior for operations
 * when the {@link Pool} reference is <code>null</code>.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.client.support.FactoryDefaultsPoolAdapter
 * @see org.apache.geode.cache.client.Pool
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public abstract class DelegatingPoolAdapter extends FactoryDefaultsPoolAdapter {

	private final Pool delegate;

	/* (non-Javadoc) */
	public static DelegatingPoolAdapter from(Pool delegate) {
		return new DelegatingPoolAdapter(delegate) { };
	}

	/**
	 * Constructs an instance of {@link DelegatingPoolAdapter} initialized with the specified {@link Pool}.
	 *
	 * @param delegate {@link Pool} used as the delegate; can be {@literal null}.
	 * @see org.apache.geode.cache.client.Pool
	 */
	public DelegatingPoolAdapter(Pool delegate) {
		this.delegate = delegate;
	}

	/* (non-Javadoc) */
	protected Pool getDelegate() {
		return delegate;
	}

	/* (non-Javadoc) */
	@Override
	public boolean isDestroyed() {
		return Optional.ofNullable(getDelegate()).map(Pool::isDestroyed).orElseGet(super::isDestroyed);
	}

	/* (non-Javadoc) */
	@Override
	public int getFreeConnectionTimeout() {
		return Optional.ofNullable(getDelegate()).map(Pool::getFreeConnectionTimeout)
			.orElseGet(super::getFreeConnectionTimeout);
	}

	/* (non-Javadoc) */
	@Override
	public long getIdleTimeout() {
		return Optional.ofNullable(getDelegate()).map(Pool::getIdleTimeout).orElseGet(super::getIdleTimeout);
	}

	/* (non-Javadoc) */
	@Override
	public int getLoadConditioningInterval() {
		return Optional.ofNullable(getDelegate()).map(Pool::getLoadConditioningInterval)
			.orElseGet(super::getLoadConditioningInterval);
	}

	/* (non-Javadoc) */
	@Override
	public List<InetSocketAddress> getLocators() {
		return Optional.ofNullable(getDelegate()).map(Pool::getLocators).orElseGet(super::getLocators);
	}

	/* (non-Javadoc) */
	@Override
	public int getMaxConnections() {
		return Optional.ofNullable(getDelegate()).map(Pool::getMaxConnections).orElseGet(super::getMaxConnections);
	}

	/* (non-Javadoc) */
	@Override
	public int getMinConnections() {
		return Optional.ofNullable(getDelegate()).map(Pool::getMinConnections).orElseGet(super::getMinConnections);
	}

	/* (non-Javadoc) */
	@Override
	public boolean getMultiuserAuthentication() {
		return Optional.ofNullable(getDelegate()).map(Pool::getMultiuserAuthentication)
			.orElseGet(super::getMultiuserAuthentication);
	}

	/* (non-Javadoc) */
	@Override
	public String getName() {
		return Optional.ofNullable(getDelegate()).map(Pool::getName).orElseGet(super::getName);
	}

	/* (non-Javadoc) */
	@Override
	public List<InetSocketAddress> getOnlineLocators() {
		return Optional.ofNullable(getDelegate()).map(Pool::getOnlineLocators).orElseGet(super::getOnlineLocators);
	}

	/* (non-Javadoc) */
	@Override
	public int getPendingEventCount() {
		return Optional.ofNullable(getDelegate()).map(Pool::getPendingEventCount).orElse(0);
	}

	/* (non-Javadoc) */
	@Override
	public long getPingInterval() {
		return Optional.ofNullable(getDelegate()).map(Pool::getPingInterval).orElseGet(super::getPingInterval);
	}

	/* (non-Javadoc) */
	@Override
	public boolean getPRSingleHopEnabled() {
		return Optional.ofNullable(getDelegate()).map(Pool::getPRSingleHopEnabled)
			.orElseGet(super::getPRSingleHopEnabled);
	}

	/* (non-Javadoc) */
	@Override
	public QueryService getQueryService() {
		return Optional.ofNullable(getDelegate()).map(Pool::getQueryService).orElseGet(super::getQueryService);
	}

	/* (non-Javadoc) */
	@Override
	public int getReadTimeout() {
		return Optional.ofNullable(getDelegate()).map(Pool::getReadTimeout).orElseGet(super::getReadTimeout);
	}

	/* (non-Javadoc) */
	@Override
	public int getRetryAttempts() {
		return Optional.ofNullable(getDelegate()).map(Pool::getRetryAttempts).orElseGet(super::getRetryAttempts);
	}

	/* (non-Javadoc) */
	@Override
	public String getServerGroup() {
		return Optional.ofNullable(getDelegate()).map(Pool::getServerGroup).orElseGet(super::getServerGroup);
	}

	/* (non-Javadoc) */
	@Override
	public List<InetSocketAddress> getServers() {
		return Optional.ofNullable(getDelegate()).map(Pool::getServers).orElseGet(super::getServers);
	}

	/* (non-Javadoc) */
	@Override
	public int getSocketBufferSize() {
		return Optional.ofNullable(getDelegate()).map(Pool::getSocketBufferSize).orElseGet(super::getSocketBufferSize);
	}

	/* (non-Javadoc) */
	@Override
	public int getSocketConnectTimeout() {
		return Optional.ofNullable(getDelegate()).map(Pool::getSocketConnectTimeout)
			.orElseGet(super::getSocketConnectTimeout);
	}

	/* (non-Javadoc) */
	@Override
	public int getStatisticInterval() {
		return Optional.ofNullable(getDelegate()).map(Pool::getStatisticInterval)
			.orElseGet(super::getStatisticInterval);
	}

	/* (non-Javadoc) */
	@Override
	public int getSubscriptionAckInterval() {
		return Optional.ofNullable(getDelegate()).map(Pool::getSubscriptionAckInterval)
			.orElseGet(super::getSubscriptionAckInterval);
	}

	/* (non-Javadoc) */
	@Override
	public boolean getSubscriptionEnabled() {
		return Optional.ofNullable(getDelegate()).map(Pool::getSubscriptionEnabled)
			.orElseGet(super::getSubscriptionEnabled);
	}

	/* (non-Javadoc) */
	@Override
	public int getSubscriptionMessageTrackingTimeout() {
		return Optional.ofNullable(getDelegate()).map(Pool::getSubscriptionMessageTrackingTimeout)
			.orElseGet(super::getSubscriptionMessageTrackingTimeout);
	}

	/* (non-Javadoc) */
	@Override
	public int getSubscriptionRedundancy() {
		return Optional.ofNullable(getDelegate()).map(Pool::getSubscriptionRedundancy)
			.orElseGet(super::getSubscriptionRedundancy);
	}

	/* (non-Javadoc) */
	@Override
	public boolean getThreadLocalConnections() {
		return Optional.ofNullable(getDelegate()).map(Pool::getThreadLocalConnections)
			.orElseGet(super::getThreadLocalConnections);
	}

	/* (non-Javadoc) */
	@Override
	public void destroy() {
		Optional.ofNullable(getDelegate()).ifPresent(Pool::destroy);
	}

	/* (non-Javadoc) */
	@Override
	public void destroy(boolean keepAlive) {
		Optional.ofNullable(getDelegate()).ifPresent(delegate -> delegate.destroy(keepAlive));
	}

	/* (non-Javadoc) */
	@Override
	public void releaseThreadLocalConnection() {
		Optional.ofNullable(getDelegate()).ifPresent(Pool::releaseThreadLocalConnection);
	}
}
