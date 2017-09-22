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

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.net.InetSocketAddress;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.client.PoolManager;
import org.apache.geode.cache.query.QueryService;
import org.apache.geode.distributed.DistributedSystem;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.config.annotation.PoolConfigurer;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.util.DistributedSystemUtils;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} to construct, configure and initialize a {@link Pool}.
 *
 * If a new {@link Pool} is created, its lifecycle is bound to that of this declaring {@link FactoryBean}
 * and indirectly, the Spring container.
 *
 * If a {@link Pool} having the configured {@link String name} already exists, then the existing {@link Pool}
 * will be returned as is without any modifications and its lifecycle will be unaffected by this {@link FactoryBean}.
 *
 * @author Costin Leau
 * @author John Blum
 * @see java.net.InetSocketAddress
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.client.PoolFactory
 * @see org.apache.geode.cache.client.PoolManager
 * @see org.apache.geode.distributed.DistributedSystem
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.data.gemfire.config.annotation.PoolConfigurer
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @see org.springframework.data.gemfire.support.ConnectionEndpointList
 */
@SuppressWarnings("unused")
public class PoolFactoryBean extends AbstractFactoryBeanSupport<Pool> implements DisposableBean, InitializingBean {

	protected static final int DEFAULT_LOCATOR_PORT = DistributedSystemUtils.DEFAULT_LOCATOR_PORT;
	protected static final int DEFAULT_SERVER_PORT = DistributedSystemUtils.DEFAULT_CACHE_SERVER_PORT;

	// indicates whether the Pool has been created internally (by this FactoryBean) or not
	volatile boolean springBasedPool = true;

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

	private ConnectionEndpointList locators = new ConnectionEndpointList();
	private ConnectionEndpointList servers = new ConnectionEndpointList();

	private List<PoolConfigurer> poolConfigurers = Collections.emptyList();

	private volatile Pool pool;

	private PoolConfigurer compositePoolConfigurer = (beanName, bean) ->
		nullSafeCollection(poolConfigurers).forEach(poolConfigurer ->  poolConfigurer.configure(beanName, bean));

	private PoolFactoryInitializer poolFactoryInitializer;

	private String name;
	private String serverGroup = PoolFactory.DEFAULT_SERVER_GROUP;

	/**
	 * Prepares the construction, configuration and initialization of a new {@link Pool}.
	 *
	 * @throws Exception if {@link Pool} initialization fails.
	 * @see org.apache.geode.cache.client.PoolManager
	 * @see org.apache.geode.cache.client.PoolFactory
	 * @see org.apache.geode.cache.client.Pool
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		init(Optional.ofNullable(PoolManager.find(validatePoolName())));
	}

	/* (non-Javadoc) */
	@SuppressWarnings("all")
	private void init(Optional<Pool> existingPool) {

		if (existingPool.isPresent()) {
			this.pool = existingPool.get();
			this.springBasedPool = false;

			logDebug(() -> String.format(
				"Pool with name [%s] already exists; Using existing Pool; Pool Configurers [%d] will not be applied",
				existingPool.get().getName(), this.poolConfigurers.size()));
		}
		else {
			this.springBasedPool = true;
			applyPoolConfigurers();

			logDebug("No Pool with name [%s] was found; Creating new Pool", getName());
		}
	}

	/* (non-Javadoc) */
	private void applyPoolConfigurers() {
		applyPoolConfigurers(getCompositePoolConfigurer());
	}

	/**
	 * Null-safe operation to apply the given array of {@link PoolConfigurer PoolConfigurers}
	 * to this {@link PoolFactoryBean}.
	 *
	 * @param poolConfigurers array of {@link PoolConfigurer PoolConfigurers} applied to this {@link PoolFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PoolConfigurer
	 * @see #applyPoolConfigurers(Iterable)
	 */
	protected void applyPoolConfigurers(PoolConfigurer... poolConfigurers) {
		applyPoolConfigurers(Arrays.asList(nullSafeArray(poolConfigurers, PoolConfigurer.class)));
	}

	/**
	 * Null-safe operation to apply the given {@link Iterable} of {@link PoolConfigurer PoolConfigurers}
	 * to this {@link PoolFactoryBean}.
	 *
	 * @param poolConfigurers {@link Iterable} of {@link PoolConfigurer PoolConfigurers}
	 * applied to this {@link PoolFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PoolConfigurer
	 */
	protected void applyPoolConfigurers(Iterable<PoolConfigurer> poolConfigurers) {
		stream(nullSafeIterable(poolConfigurers).spliterator(), false)
			.forEach(poolConfigurer -> poolConfigurer.configure(getName(), this));
	}

	/* (non-Javadoc) */
	private String validatePoolName() {

		if (!StringUtils.hasText(getName())) {
			setName(Optional.ofNullable(getBeanName()).filter(StringUtils::hasText)
				.orElseThrow(() -> newIllegalArgumentException("Pool name is required")));
		}

		return getName();
	}

	/**
	 * Releases all system resources and destroys the {@link Pool} when created by this {@link PoolFactoryBean}.
	 *
	 * @throws Exception if the {@link Pool} destruction caused an error.
	 * @see org.springframework.beans.factory.DisposableBean#destroy()
	 */
	@Override
	public void destroy() throws Exception {

		Optional.ofNullable(this.pool)
			.filter(pool -> this.springBasedPool)
			.filter(pool -> !pool.isDestroyed())
			.ifPresent(pool -> {
				pool.releaseThreadLocalConnection();
				pool.destroy(this.keepAlive);
				setPool(null);
				logDebug("Destroyed Pool [%s]", pool.getName());
			});
	}

	/**
	 * Returns a reference to the Composite {@link PoolConfigurer} used to apply additional configuration
	 * to this {@link PoolFactoryBean} on Spring container initialization.
	 *
	 * @return the Composite {@link PoolConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.PoolConfigurer
	 */
	protected PoolConfigurer getCompositePoolConfigurer() {
		return this.compositePoolConfigurer;
	}

	/**
	 * Returns an object reference to the {@link Pool} created by this {@link PoolFactoryBean}.
	 *
	 * @return an object reference to the {@link Pool} created by this {@link PoolFactoryBean}.
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 * @see org.apache.geode.cache.client.Pool
	 */
	@Override
	public Pool getObject() throws Exception {

		return Optional.ofNullable(this.pool).orElseGet(() -> {

			eagerlyInitializeClientCacheIfNotPresent();

			PoolFactory poolFactory = configure(initialize(createPoolFactory()));

			this.pool = create(poolFactory, getName());

			return this.pool;
		});
	}

	/**
	 * Determines whether the {@link DistributedSystem} exists yet or not.
	 *
	 * @return a boolean value indicating whether the single, {@link DistributedSystem} has already been created.
	 * @see org.springframework.data.gemfire.GemfireUtils#getDistributedSystem()
	 * @see org.springframework.data.gemfire.GemfireUtils#isConnected(DistributedSystem)
	 * @see org.apache.geode.distributed.DistributedSystem
	 */
	boolean isDistributedSystemPresent() {
		return GemfireUtils.isConnected(GemfireUtils.getDistributedSystem());
	}

	/**
	 * Attempts to eagerly initialize the {@link ClientCache} if not already present so that a single
	 * {@link DistributedSystem} will exist, which is required to create a {@link Pool} instance.
	 *
	 * @see org.springframework.beans.factory.BeanFactory#getBean(Class)
	 * @see org.apache.geode.cache.client.ClientCache
	 * @see org.apache.geode.distributed.DistributedSystem
	 * @see #isDistributedSystemPresent()
	 */
	private void eagerlyInitializeClientCacheIfNotPresent() {
		if (!isDistributedSystemPresent()) {
			getBeanFactory().getBean(ClientCache.class);
		}
	}

	/**
	 * Creates an instance of the {@link PoolFactory} interface to construct, configure and initialize a {@link Pool}.
	 *
	 * @return a {@link PoolFactory} implementation to create a {@link Pool}.
	 * @see org.apache.geode.cache.client.PoolManager#createFactory()
	 * @see org.apache.geode.cache.client.PoolFactory
	 */
	protected PoolFactory createPoolFactory() {
		return PoolManager.createFactory();
	}

	/**
	 * Configures the given {@link PoolFactory} from this {@link PoolFactoryBean}.
	 *
	 * @param poolFactory {@link PoolFactory} to configure.
	 * @return the given {@link PoolFactory}.
	 * @see org.apache.geode.cache.client.PoolFactory
	 */
	protected PoolFactory configure(PoolFactory poolFactory) {

		Optional.ofNullable(poolFactory).ifPresent(it -> {

			it.setFreeConnectionTimeout(this.freeConnectionTimeout);
			it.setIdleTimeout(this.idleTimeout);
			it.setLoadConditioningInterval(this.loadConditioningInterval);
			it.setMaxConnections(this.maxConnections);
			it.setMinConnections(this.minConnections);
			it.setMultiuserAuthentication(this.multiUserAuthentication);
			it.setPingInterval(this.pingInterval);
			it.setPRSingleHopEnabled(this.prSingleHopEnabled);
			it.setReadTimeout(this.readTimeout);
			it.setRetryAttempts(this.retryAttempts);
			it.setServerGroup(this.serverGroup);
			it.setSocketBufferSize(this.socketBufferSize);
			it.setStatisticInterval(this.statisticInterval);
			it.setSubscriptionAckInterval(this.subscriptionAckInterval);
			it.setSubscriptionEnabled(this.subscriptionEnabled);
			it.setSubscriptionMessageTrackingTimeout(this.subscriptionMessageTrackingTimeout);
			it.setSubscriptionRedundancy(this.subscriptionRedundancy);
			it.setThreadLocalConnections(this.threadLocalConnections);

			nullSafeCollection(this.locators).forEach(locator ->
				it.addLocator(locator.getHost(), locator.getPort()));

			nullSafeCollection(this.servers).forEach(server ->
				it.addServer(server.getHost(), server.getPort()));
		});

		return poolFactory;
	}

	/**
	 * Initializes the given {@link PoolFactory} with any configured {@link PoolFactoryInitializer}.
	 *
	 * @param poolFactory {@link PoolFactory} to initialize.
	 * @return the initialized {@link PoolFactory}.
	 * @see org.apache.geode.cache.client.PoolFactory
	 */
	protected PoolFactory initialize(PoolFactory poolFactory) {

		return Optional.ofNullable(this.poolFactoryInitializer)
			.map(initializer -> initializer.initialize(poolFactory))
			.orElse(poolFactory);
	}

	/**
	 * Creates a {@link Pool} with the given {@link String name} using the provided {@link PoolFactory}.
	 *
	 * @param poolFactory {@link PoolFactory} used to create the {@link Pool}.
	 * @param poolName {@link String name} of the new {@link Pool}.
	 * @return a new instance of {@link Pool} with the given {@link String name}.
	 * @see org.apache.geode.cache.client.PoolFactory#create(String)
	 * @see org.apache.geode.cache.client.Pool
	 */
	protected Pool create(PoolFactory poolFactory, String poolName) {
		return poolFactory.create(poolName);
	}

	/**
	 * Returns the {@link Class} type of the {@link Pool} produced by this {@link PoolFactoryBean}.
	 *
	 * @return the {@link Class} type of the {@link Pool} produced by this {@link PoolFactoryBean}.
	 * @see org.springframework.beans.factory.FactoryBean#getObjectType()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Class<?> getObjectType() {
		return Optional.ofNullable(this.pool).map(Pool::getClass).orElse((Class) Pool.class);
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
	public void setName(String name) {
		this.name = name;
	}

	/* (non-Javadoc) */
	protected String getName() {
		return this.name;
	}

	/* (non-Javadoc) */
	public void setPool(Pool pool) {
		this.pool = pool;
	}

	/* (non-Javadoc) */
	public Pool getPool() {

		return Optional.ofNullable(this.pool).orElseGet(() -> new PoolAdapter() {

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
			public List<InetSocketAddress> getOnlineLocators() {
				return Optional.ofNullable(PoolFactoryBean.this.pool).map(Pool::getOnlineLocators)
					.orElseThrow(() -> new IllegalStateException("The Pool has not been initialized"));
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
				return Optional.ofNullable(PoolFactoryBean.this.getName()).filter(StringUtils::hasText)
					.orElseGet(PoolFactoryBean.this::getBeanName);
			}

			@Override
			public int getPendingEventCount() {
				return Optional.ofNullable(PoolFactoryBean.this.pool).map(Pool::getPendingEventCount)
					.orElseThrow(() -> new IllegalStateException("The Pool has not been initialized"));
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
				return Optional.ofNullable(PoolFactoryBean.this.pool).map(Pool::getQueryService)
					.orElseThrow(() -> new IllegalStateException("The Pool has not been initialized"));
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
			public void destroy(boolean keepAlive) {
				try {
					PoolFactoryBean.this.destroy();
				}
				catch (Exception ignore) {
					Optional.ofNullable(PoolFactoryBean.this.pool).ifPresent(pool -> pool.destroy(keepAlive));
				}
			}

			@Override
			public void releaseThreadLocalConnection() {
				Pool pool = PoolFactoryBean.this.pool;

				if (pool != null) {
					pool.releaseThreadLocalConnection();
				}
				else {
					throw new IllegalStateException("The Pool has not been initialized");
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

	/**
	 * Null-safe operation to set an array of {@link PoolConfigurer PoolConfigurers} used to apply
	 * additional configuration to this {@link PoolFactoryBean} when using Annotation-based configuration.
	 *
	 * @param poolConfigurers array of {@link PoolConfigurer PoolConfigurers} used to apply
	 * additional configuration to this {@link PoolFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PoolConfigurer
	 * @see #setPoolConfigurers(List)
	 */
	public void setPoolConfigurers(PoolConfigurer... poolConfigurers) {
		setPoolConfigurers(Arrays.asList(nullSafeArray(poolConfigurers, PoolConfigurer.class)));
	}

	/**
	 * Null-safe operation to set an {@link Iterable} of {@link PoolConfigurer PoolConfigurers} used to apply
	 * additional configuration to this {@link PoolFactoryBean} when using Annotation-based configuration.
	 *
	 * @param poolConfigurers {@link Iterable} of {@link PoolConfigurer PoolConfigurers} used to apply
	 * additional configuration to this {@link PoolFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PoolConfigurer
	 */
	public void setPoolConfigurers(List<PoolConfigurer> poolConfigurers) {
		this.poolConfigurers = Optional.ofNullable(poolConfigurers).orElseGet(Collections::emptyList);
	}

	/**
	 * Sets the {@link PoolFactoryInitializer} to initialize the {@link PoolFactory} used by
	 * this {@link PoolFactoryBean} to create a {@link Pool}.
	 *
	 * @param poolFactoryInitializer {@link PoolFactoryInitializer} user provided callback interface invoked
	 * by this {@link PoolFactoryBean} to initialize the {@link PoolFactory} constructed to create the {@link Pool}.
	 * @see org.springframework.data.gemfire.client.PoolFactoryBean.PoolFactoryInitializer
	 */
	public void setPoolFactoryInitializer(PoolFactoryInitializer poolFactoryInitializer) {
		this.poolFactoryInitializer = poolFactoryInitializer;
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

	/*
	 * (non-Javadoc)
	 * internal framework use only
	 */
	public final void setLocatorsConfiguration(Object locatorsConfiguration) {
	}

	/*
	 * (non-Javadoc)
	 * internal framework use only
	 */
	public final void setServersConfiguration(Object serversConfiguration) {
	}

	/**
	 * Callback interface to initialize the {@link PoolFactory} used by this {@link PoolFactoryBean}
	 * to create a {@link Pool} by providing additional or alternative configuration for the factory.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory
	 */
	public interface PoolFactoryInitializer {

		/**
		 * Initializes the given {@link PoolFactory}.
		 *
		 * @param poolFactory {@link PoolFactory} to initialize.
		 * @return the given {@link PoolFactory}.
		 * @see org.apache.geode.cache.client.PoolFactory
		 */
		PoolFactory initialize(PoolFactory poolFactory);
	}
}
