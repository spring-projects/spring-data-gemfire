/*
 * Copyright 2012-2019 the original author or authors.
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
import java.util.Collection;
import java.util.List;

import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.query.QueryService;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;

/**
 * The DefaultableDelegatingPoolAdapter class is a wrapper class around Pool allowing default configuration property
 * values to be providing in the case that the Pool's setting were null.
 *
 * @author John Blum
 * @see org.apache.geode.cache.client.Pool
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public abstract class DefaultableDelegatingPoolAdapter {

	private final Pool delegate;

	private Preference preference = Preference.PREFER_POOL;

	/* (non-Javadoc) */
	public static DefaultableDelegatingPoolAdapter from(Pool delegate) {
		return new DefaultableDelegatingPoolAdapter(delegate) { };
	}

	/* (non-Javadoc) */
	protected DefaultableDelegatingPoolAdapter(Pool delegate) {
		Assert.notNull(delegate, "'delegate' must not be null");
		this.delegate = delegate;
	}

	/* (non-Javadoc) */
	protected Pool getDelegate() {
		return delegate;
	}

	/* (non-Javadoc) */
	protected DefaultableDelegatingPoolAdapter setPreference(Preference preference) {
		this.preference = preference;
		return this;
	}

	/* (non-Javadoc) */
	protected Preference getPreference() {
		return this.preference;
	}

	/* (non-Javadoc) */
	protected <T> T defaultIfNull(T defaultValue, ValueProvider<T> valueProvider) {
		return (prefersPool() ? SpringUtils.defaultIfNull(valueProvider.getValue(), defaultValue) :
			(defaultValue != null ? defaultValue : valueProvider.getValue()));
	}

	/* (non-Javadoc) */
	protected <E, T extends Collection<E>> T defaultIfEmpty(T defaultValue, ValueProvider<T> valueProvider) {
		if (prefersPool()) {
			T value = valueProvider.getValue();
			return (value == null || value.isEmpty() ? defaultValue : value);
		}
		else {
			return (defaultValue == null || defaultValue.isEmpty() ? valueProvider.getValue() : defaultValue);
		}
	}

	/* (non-Javadoc) */
	public DefaultableDelegatingPoolAdapter preferDefault() {
		return setPreference(Preference.PREFER_DEFAULT);
	}

	/* (non-Javadoc) */
	protected boolean prefersDefault() {
		return Preference.PREFER_DEFAULT.equals(getPreference());
	}

	/* (non-Javadoc) */
	public DefaultableDelegatingPoolAdapter preferPool() {
		return setPreference(Preference.PREFER_POOL);
	}

	/* (non-Javadoc) */
	protected boolean prefersPool() {
		return Preference.PREFER_POOL.equals(getPreference());
	}

	/* (non-Javadoc) */
	public boolean isDestroyed() {
		return getDelegate().isDestroyed();
	}

	/* (non-Javadoc) */
	public int getFreeConnectionTimeout(Integer defaultFreeConnectionTimeout) {
		return defaultIfNull(defaultFreeConnectionTimeout, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getFreeConnectionTimeout();
			}
		});
	}

	/* (non-Javadoc) */
	public long getIdleTimeout(Long defaultIdleTimeout) {
		return defaultIfNull(defaultIdleTimeout, new ValueProvider<Long>() {
			@Override public Long getValue() {
				return getDelegate().getIdleTimeout();
			}
		});
	}

	/* (non-Javadoc) */
	public int getLoadConditioningInterval(Integer defaultLoadConditioningInterval) {
		return defaultIfNull(defaultLoadConditioningInterval, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getLoadConditioningInterval();
			}
		});
	}

	/* (non-Javadoc) */
	public List<InetSocketAddress> getLocators(List<InetSocketAddress> defaultLocators) {
		return defaultIfEmpty(defaultLocators, new ValueProvider<List<InetSocketAddress>>() {
			@Override public List<InetSocketAddress> getValue() {
				return getDelegate().getLocators();
			}
		});
	}

	/* (non-Javadoc) */
	public int getMaxConnections(Integer defaultMaxConnections) {
		return defaultIfNull(defaultMaxConnections, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getMaxConnections();
			}
		});
	}

	/* (non-Javadoc) */
	public int getMinConnections(Integer defaultMinConnections) {
		return defaultIfNull(defaultMinConnections, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getMinConnections();
			}
		});
	}

	/* (non-Javadoc) */
	public boolean getMultiuserAuthentication(Boolean defaultMultiUserAuthentication) {
		return defaultIfNull(defaultMultiUserAuthentication, new ValueProvider<Boolean>() {
			@Override public Boolean getValue() {
				return getDelegate().getMultiuserAuthentication();
			}
		});
	}

	/* (non-Javadoc) */
	public String getName() {
		return getDelegate().getName();
	}

	/* (non-Javadoc) */
	public int getPendingEventCount() {
		return getDelegate().getPendingEventCount();
	}

	/* (non-Javadoc) */
	public long getPingInterval(Long defaultPingInterval) {
		return defaultIfNull(defaultPingInterval, new ValueProvider<Long>() {
			@Override public Long getValue() {
				return getDelegate().getPingInterval();
			}
		});
	}

	/* (non-Javadoc) */
	public boolean getPRSingleHopEnabled(Boolean defaultPrSingleHopEnabled) {
		return defaultIfNull(defaultPrSingleHopEnabled, new ValueProvider<Boolean>() {
			@Override public Boolean getValue() {
				return getDelegate().getPRSingleHopEnabled();
			}
		});
	}

	/* (non-Javadoc) */
	public QueryService getQueryService(QueryService defaultQueryService) {
		return defaultIfNull(defaultQueryService, new ValueProvider<QueryService>() {
			@Override public QueryService getValue() {
				return getDelegate().getQueryService();
			}
		});
	}

	/* (non-Javadoc) */
	public int getReadTimeout(Integer defaultReadTimeout) {
		return defaultIfNull(defaultReadTimeout, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getReadTimeout();
			}
		});
	}

	/* (non-Javadoc) */
	public int getRetryAttempts(Integer defaultRetryAttempts) {
		return defaultIfNull(defaultRetryAttempts, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getRetryAttempts();
			}
		});
	}

	/* (non-Javadoc) */
	public String getServerGroup(String defaultServerGroup) {
		return defaultIfNull(defaultServerGroup, new ValueProvider<String>() {
			@Override public String getValue() {
				return getDelegate().getServerGroup();
			}
		});
	}

	/* (non-Javadoc) */
	public List<InetSocketAddress> getServers(List<InetSocketAddress> defaultServers) {
		return defaultIfEmpty(defaultServers, new ValueProvider<List<InetSocketAddress>>() {
			@Override public List<InetSocketAddress> getValue() {
				return getDelegate().getServers();
			}
		});
	}

	/* (non-Javadoc) */
	public int getSocketBufferSize(Integer defaultSocketBufferSize) {
		return defaultIfNull(defaultSocketBufferSize, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getSocketBufferSize();
			}
		});
	}

	/* (non-Javadoc) */
	public int getStatisticInterval(Integer defaultStatisticInterval) {
		return defaultIfNull(defaultStatisticInterval, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getStatisticInterval();
			}
		});
	}

	/* (non-Javadoc) */
	public int getSubscriptionAckInterval(Integer defaultSubscriptionAckInterval) {
		return defaultIfNull(defaultSubscriptionAckInterval, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getSubscriptionAckInterval();
			}
		});
	}

	/* (non-Javadoc) */
	public boolean getSubscriptionEnabled(Boolean defaultSubscriptionEnabled) {
		return defaultIfNull(defaultSubscriptionEnabled, new ValueProvider<Boolean>() {
			@Override public Boolean getValue() {
				return getDelegate().getSubscriptionEnabled();
			}
		});
	}

	/* (non-Javadoc) */
	public int getSubscriptionMessageTrackingTimeout(Integer defaultSubscriptionMessageTrackingTimeout) {
		return defaultIfNull(defaultSubscriptionMessageTrackingTimeout, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getSubscriptionMessageTrackingTimeout();
			}
		});
	}

	/* (non-Javadoc) */
	public int getSubscriptionRedundancy(Integer defaultSubscriptionRedundancy) {
		return defaultIfNull(defaultSubscriptionRedundancy, new ValueProvider<Integer>() {
			@Override public Integer getValue() {
				return getDelegate().getSubscriptionRedundancy();
			}
		});
	}

	/* (non-Javadoc) */
	public boolean getThreadLocalConnections(Boolean defaultThreadLocalConnections) {
		return defaultIfNull(defaultThreadLocalConnections, new ValueProvider<Boolean>() {
			@Override public Boolean getValue() {
				return getDelegate().getThreadLocalConnections();
			}
		});
	}

	/* (non-Javadoc) */
	public void destroy() {
		getDelegate().destroy();
	}

	/* (non-Javadoc) */
	public void destroy(final boolean keepAlive) {
		getDelegate().destroy(keepAlive);
	}

	/* (non-Javadoc) */
	public void releaseThreadLocalConnection() {
		getDelegate().releaseThreadLocalConnection();
	}

	/* (non-Javadoc) */
	enum Preference {
		PREFER_DEFAULT,
		PREFER_POOL;
	}

	/* (non-Javadoc) */
	interface ValueProvider<T> {
		T getValue();
	}

}
