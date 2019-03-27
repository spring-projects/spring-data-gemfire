/*
 * Copyright 2012 the original author or authors.
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
import java.util.List;

import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.query.QueryService;

/**
 * DelegatingPoolAdapter is an abstract implementation of GemFire's {@link Pool} interface and extension of
 * {@link FactoryDefaultsPoolAdapter} that delegates operations to the provided {@link Pool} instance.
 *
 * However, this implementation guards against a potentially <code>null</code> {@link Pool} reference by returning
 * default factory settings for the {@link Pool}'s configuration properties along with default behavior for operations
 * when the {@link Pool} reference is <code>null</code>.
 *
 * @author John Blum
 * @see FactoryDefaultsPoolAdapter
 * @see com.gemstone.gemfire.cache.client.Pool
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public abstract class DelegatingPoolAdapter extends FactoryDefaultsPoolAdapter {

	private final Pool delegate;

	/* (non-Javadoc) */
	public static DelegatingPoolAdapter from(Pool delegate) {
		return new DelegatingPoolAdapter(delegate) { };
	}

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
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.isDestroyed() : super.isDestroyed());
	}

	/* (non-Javadoc) */
	@Override
	public int getFreeConnectionTimeout() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getFreeConnectionTimeout() : super.getFreeConnectionTimeout());
	}

	/* (non-Javadoc) */
	@Override
	public long getIdleTimeout() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getIdleTimeout() : super.getIdleTimeout());
	}

	/* (non-Javadoc) */
	@Override
	public int getLoadConditioningInterval() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getLoadConditioningInterval() : super.getLoadConditioningInterval());
	}

	/* (non-Javadoc) */
	@Override
	public List<InetSocketAddress> getLocators() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getLocators() : super.getLocators());
	}

	/* (non-Javadoc) */
	@Override
	public int getMaxConnections() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getMaxConnections() : super.getMaxConnections());
	}

	/* (non-Javadoc) */
	@Override
	public int getMinConnections() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getMinConnections() : super.getMinConnections());
	}

	/* (non-Javadoc) */
	@Override
	public boolean getMultiuserAuthentication() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getMultiuserAuthentication() : super.getMultiuserAuthentication());
	}

	/* (non-Javadoc) */
	@Override
	public String getName() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getName() : super.getName());
	}

	/* (non-Javadoc) */
	@Override
	public boolean getPRSingleHopEnabled() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getPRSingleHopEnabled() : super.getPRSingleHopEnabled());
	}

	/* (non-Javadoc) */
	@Override
	public int getPendingEventCount() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getPendingEventCount() : 0);
	}

	/* (non-Javadoc) */
	@Override
	public long getPingInterval() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getPingInterval() : super.getPingInterval());
	}

	/* (non-Javadoc) */
	@Override
	public QueryService getQueryService() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getQueryService() : super.getQueryService());
	}

	/* (non-Javadoc) */
	@Override
	public int getReadTimeout() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getReadTimeout() : super.getReadTimeout());
	}

	/* (non-Javadoc) */
	@Override
	public int getRetryAttempts() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getRetryAttempts() : super.getRetryAttempts());
	}

	/* (non-Javadoc) */
	@Override
	public String getServerGroup() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getServerGroup() : super.getServerGroup());
	}

	/* (non-Javadoc) */
	@Override
	public List<InetSocketAddress> getServers() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getServers() : super.getServers());
	}

	/* (non-Javadoc) */
	@Override
	public int getSocketBufferSize() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getSocketBufferSize() : super.getSocketBufferSize());
	}

	/* (non-Javadoc) */
	@Override
	public int getStatisticInterval() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getStatisticInterval() : super.getStatisticInterval());
	}

	/* (non-Javadoc) */
	@Override
	public int getSubscriptionAckInterval() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getSubscriptionAckInterval() : super.getSubscriptionAckInterval());
	}

	/* (non-Javadoc) */
	@Override
	public boolean getSubscriptionEnabled() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getSubscriptionEnabled() : super.getSubscriptionEnabled());
	}

	/* (non-Javadoc) */
	@Override
	public int getSubscriptionMessageTrackingTimeout() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getSubscriptionMessageTrackingTimeout()
			: super.getSubscriptionMessageTrackingTimeout());
	}

	/* (non-Javadoc) */
	@Override
	public int getSubscriptionRedundancy() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getSubscriptionRedundancy() : super.getSubscriptionRedundancy());
	}

	/* (non-Javadoc) */
	@Override
	public boolean getThreadLocalConnections() {
		Pool delegate = getDelegate();
		return (delegate != null ? delegate.getThreadLocalConnections() : super.getThreadLocalConnections());
	}

	/* (non-Javadoc) */
	@Override
	public void destroy() {
		Pool delegate = getDelegate();
		if (delegate != null) {
			delegate.destroy();
		}
	}

	/* (non-Javadoc) */
	@Override
	public void destroy(final boolean keepAlive) {
		Pool delegate = getDelegate();
		if (delegate != null) {
			delegate.destroy(keepAlive);
		}
	}

	/* (non-Javadoc) */
	@Override
	public void releaseThreadLocalConnection() {
		Pool delegate = getDelegate();
		if (delegate != null) {
			delegate.releaseThreadLocalConnection();
		}
	}

}
