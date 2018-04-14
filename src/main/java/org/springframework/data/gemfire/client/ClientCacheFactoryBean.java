/*
 * Copyright 2011-2018 the original author or authors.
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

import java.net.InetSocketAddress;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolManager;
import org.apache.geode.distributed.DistributedSystem;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ApplicationContextEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.client.support.DefaultableDelegatingPoolAdapter;
import org.springframework.data.gemfire.client.support.DelegatingPoolAdapter;
import org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.StringUtils;

/**
 * Spring {@link org.springframework.beans.factory.FactoryBean} used to create a Pivotal GemFire/Apache Geode
 * {@link ClientCache}.
 *
 * @author Costin Leau
 * @author Lyndon Adams
 * @author John Blum
 * @see java.net.InetSocketAddress
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientCacheFactory
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.client.PoolManager
 * @see org.apache.geode.distributed.DistributedSystem
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.context.event.ContextRefreshedEvent
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @see org.springframework.data.gemfire.support.ConnectionEndpointList
 */
@SuppressWarnings("unused")
public class ClientCacheFactoryBean extends CacheFactoryBean implements ApplicationListener<ContextRefreshedEvent> {

	private Boolean keepAlive = false;
	private Boolean multiUserAuthentication;
	private Boolean prSingleHopEnabled;
	private Boolean readyForEvents;
	private Boolean subscriptionEnabled;
	private Boolean threadLocalConnections;

	private ConnectionEndpointList locators = new ConnectionEndpointList();
	private ConnectionEndpointList servers = new ConnectionEndpointList();

	private Integer durableClientTimeout;
	private Integer freeConnectionTimeout;
	private Integer loadConditioningInterval;
	private Integer maxConnections;
	private Integer minConnections;
	private Integer readTimeout;
	private Integer retryAttempts;
	private Integer socketBufferSize;
	private Integer socketConnectTimeout;
	private Integer statisticsInterval;
	private Integer subscriptionAckInterval;
	private Integer subscriptionMessageTrackingTimeout;
	private Integer subscriptionRedundancy;

	private List<ClientCacheConfigurer> clientCacheConfigurers = Collections.emptyList();

	private Long idleTimeout;
	private Long pingInterval;

	private Pool pool;

	private String durableClientId;
	private String poolName;
	private String serverGroup;

	private final ClientCacheConfigurer compositeClientCacheConfigurer = (beanName, bean) ->
		nullSafeCollection(clientCacheConfigurers).forEach(clientCacheConfigurer ->
			clientCacheConfigurer.configure(beanName, bean));

	/**
	 * Applies the composite {@link ClientCacheConfigurer ClientCacheConfigurers}
	 * to this {@link ClientCacheFactoryBean}.
	 *
	 * @see #getCompositeClientCacheConfigurer()
	 * @see #applyClientCacheConfigurers(ClientCacheConfigurer...)
	 */
	@Override
	protected void applyCacheConfigurers() {
		applyClientCacheConfigurers(getCompositeClientCacheConfigurer());
	}

	/**
	 * Null-safe operation to apply the given array of {@link ClientCacheConfigurer ClientCacheConfigurers}
	 * to this {@link ClientCacheFactoryBean}.
	 *
	 * @param clientCacheConfigurers array of {@link ClientCacheConfigurer ClientCacheConfigurers} applied to
	 * this {@link ClientCacheFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
	 * @see #applyClientCacheConfigurers(Iterable)
	 */
	protected void applyClientCacheConfigurers(ClientCacheConfigurer... clientCacheConfigurers) {
		applyClientCacheConfigurers(Arrays.asList(nullSafeArray(clientCacheConfigurers, ClientCacheConfigurer.class)));
	}

	/**
	 * Null-safe operation to apply the given {@link Iterable} of {@link ClientCacheConfigurer ClientCacheConfigurers}
	 * to this {@link ClientCacheFactoryBean}.
	 *
	 * @param clientCacheConfigurers {@link Iterable} of {@link ClientCacheConfigurer ClientCacheConfigurers}
	 * applied to this {@link ClientCacheFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
	 * @see java.lang.Iterable
	 */
	protected void applyClientCacheConfigurers(Iterable<ClientCacheConfigurer> clientCacheConfigurers) {
		stream(nullSafeIterable(clientCacheConfigurers).spliterator(), false)
			.forEach(clientCacheConfigurer -> clientCacheConfigurer.configure(getBeanName(), this));
	}

	/**
	 * Fetches an existing {@link ClientCache} instance from the {@link ClientCacheFactory}.
	 *
	 * @param <T> parameterized {@link Class} type extension of {@link GemFireCache}.
	 * @return an existing {@link ClientCache} instance if available.
	 * @throws org.apache.geode.cache.CacheClosedException if an existing {@link ClientCache} instance does not exist.
	 * @see org.apache.geode.cache.client.ClientCacheFactory#getAnyInstance()
	 * @see org.apache.geode.cache.GemFireCache
	 * @see #getCache()
	 */
	@Override
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T fetchCache() {
		return (T) Optional.ofNullable(getCache()).orElseGet(ClientCacheFactory::getAnyInstance);
	}

	/**
	 * Resolves the Pivotal GemFire/Apache Geode {@link Properties} used to configure the {@link ClientCache}.
	 *
	 * @return the resolved Pivotal GemFire/Apache Geode {@link Properties} used to configure the {@link ClientCache}.
	 * @see org.apache.geode.distributed.DistributedSystem#getProperties()
	 * @see #getDistributedSystem()
	 */
	@Override
	protected Properties resolveProperties() {

		Properties gemfireProperties = super.resolveProperties();

		DistributedSystem distributedSystem = getDistributedSystem();

		if (GemfireUtils.isConnected(distributedSystem)) {
			Properties distributedSystemProperties = (Properties) distributedSystem.getProperties().clone();
			distributedSystemProperties.putAll(gemfireProperties);
			gemfireProperties = distributedSystemProperties;
		}

		GemfireUtils.configureDurableClient(gemfireProperties, getDurableClientId(), getDurableClientTimeout());

		return gemfireProperties;
	}

	/**
	 * Returns the {@link DistributedSystem} formed from cache initialization.
	 *
	 * @param <T> {@link Class} type of the {@link DistributedSystem}.
	 * @return an instance of the {@link DistributedSystem}.
	 * @see org.apache.geode.distributed.DistributedSystem
	 */
	<T extends DistributedSystem> T getDistributedSystem() {
		return GemfireUtils.getDistributedSystem();
	}

	/**
	 * Constructs a new instance of {@link ClientCacheFactory} initialized with the given Pivotal GemFire/Apache Geode
	 * {@link Properties} used to construct, configure and initialize an instance of a {@link ClientCache}.
	 *
	 * @param gemfireProperties {@link Properties} used by the {@link ClientCacheFactory}
	 * to configure the {@link ClientCache}.
	 * @return a new instance of {@link ClientCacheFactory} initialized with
	 * the given Pivotal GemFire/Apache Geode {@link Properties}.
	 * @see org.apache.geode.cache.client.ClientCacheFactory
	 * @see java.util.Properties
	 */
	@Override
	protected Object createFactory(Properties gemfireProperties) {
		return new ClientCacheFactory(gemfireProperties);
	}

	/**
	 * Configures the {@link ClientCacheFactory} used to create the {@link ClientCache}.
	 *
	 * Sets PDX options specified by the user.
	 *
	 * Sets Pool options specified by the user.
	 *
	 * @param factory {@link ClientCacheFactory} used to create the {@link ClientCache}.
	 * @return the configured {@link ClientCacheFactory}.
	 * @see #configurePdx(ClientCacheFactory)
	 */
	@Override
	protected Object configureFactory(Object factory) {
		return configurePool(configurePdx((ClientCacheFactory) factory));
	}

	/**
	 * Configure PDX for the {@link ClientCacheFactory}.
	 *
	 * @param clientCacheFactory {@link ClientCacheFactory} used to configure PDX.
	 * @return the given {@link ClientCacheFactory}
	 * @see org.apache.geode.cache.client.ClientCacheFactory
	 */
	ClientCacheFactory configurePdx(ClientCacheFactory clientCacheFactory) {

		Optional.ofNullable(getPdxSerializer()).ifPresent(clientCacheFactory::setPdxSerializer);

		Optional.ofNullable(getPdxDiskStoreName()).filter(StringUtils::hasText)
			.ifPresent(clientCacheFactory::setPdxDiskStore);

		Optional.ofNullable(getPdxIgnoreUnreadFields()).ifPresent(clientCacheFactory::setPdxIgnoreUnreadFields);

		Optional.ofNullable(getPdxPersistent()).ifPresent(clientCacheFactory::setPdxPersistent);

		Optional.ofNullable(getPdxReadSerialized()).ifPresent(clientCacheFactory::setPdxReadSerialized);

		return clientCacheFactory;
	}

	/**
	 * Configure the {@literal DEFAULT} {@link Pool} configuration settings with the {@link ClientCacheFactory}
	 * using a given {@link Pool} instance or a named {@link Pool}.
	 *
	 * @param clientCacheFactory {@link ClientCacheFactory} use to configure the {@literal DEFAULT} {@link Pool}.
	 * @see org.apache.geode.cache.client.ClientCacheFactory
	 * @see org.apache.geode.cache.client.Pool
	 */
	ClientCacheFactory configurePool(ClientCacheFactory clientCacheFactory) {

		DefaultableDelegatingPoolAdapter pool =
			DefaultableDelegatingPoolAdapter.from(DelegatingPoolAdapter.from(resolvePool())).preferDefault();

		clientCacheFactory.setPoolFreeConnectionTimeout(pool.getFreeConnectionTimeout(getFreeConnectionTimeout()));
		clientCacheFactory.setPoolIdleTimeout(pool.getIdleTimeout(getIdleTimeout()));
		clientCacheFactory.setPoolLoadConditioningInterval(pool.getLoadConditioningInterval(getLoadConditioningInterval()));
		clientCacheFactory.setPoolMaxConnections(pool.getMaxConnections(getMaxConnections()));
		clientCacheFactory.setPoolMinConnections(pool.getMinConnections(getMinConnections()));
		clientCacheFactory.setPoolMultiuserAuthentication(pool.getMultiuserAuthentication(getMultiUserAuthentication()));
		clientCacheFactory.setPoolPingInterval(pool.getPingInterval(getPingInterval()));
		clientCacheFactory.setPoolPRSingleHopEnabled(pool.getPRSingleHopEnabled(getPrSingleHopEnabled()));
		clientCacheFactory.setPoolReadTimeout(pool.getReadTimeout(getReadTimeout()));
		clientCacheFactory.setPoolRetryAttempts(pool.getRetryAttempts(getRetryAttempts()));
		clientCacheFactory.setPoolServerGroup(pool.getServerGroup(getServerGroup()));
		clientCacheFactory.setPoolSocketBufferSize(pool.getSocketBufferSize(getSocketBufferSize()));
		clientCacheFactory.setPoolSocketConnectTimeout(pool.getSocketConnectTimeout(getSocketConnectTimeout()));
		clientCacheFactory.setPoolStatisticInterval(pool.getStatisticInterval(getStatisticsInterval()));
		clientCacheFactory.setPoolSubscriptionAckInterval(pool.getSubscriptionAckInterval(getSubscriptionAckInterval()));
		clientCacheFactory.setPoolSubscriptionEnabled(pool.getSubscriptionEnabled(getSubscriptionEnabled()));
		clientCacheFactory.setPoolSubscriptionMessageTrackingTimeout(pool.getSubscriptionMessageTrackingTimeout(getSubscriptionMessageTrackingTimeout()));
		clientCacheFactory.setPoolSubscriptionRedundancy(pool.getSubscriptionRedundancy(getSubscriptionRedundancy()));
		clientCacheFactory.setPoolThreadLocalConnections(pool.getThreadLocalConnections(getThreadLocalConnections()));

		AtomicBoolean noServers = new AtomicBoolean(getServers().isEmpty());

		boolean noLocators = getLocators().isEmpty();
		boolean hasLocators = !noLocators;
		boolean hasServers = !noServers.get();

		if (hasServers || noLocators) {

			Iterable<InetSocketAddress> servers = pool.getServers(getServers().toInetSocketAddresses());

			stream(servers.spliterator(), false).forEach(server -> {
				clientCacheFactory.addPoolServer(server.getHostName(), server.getPort());
				noServers.set(false);
			});
		}

		if (hasLocators || noServers.get()) {

			Iterable<InetSocketAddress> locators = pool.getLocators(getLocators().toInetSocketAddresses());

			stream(locators.spliterator(), false).forEach(locator ->
				clientCacheFactory.addPoolLocator(locator.getHostName(), locator.getPort()));
		}

		return clientCacheFactory;
	}

	/**
	 * Resolves the {@link Pool} used to configure the {@link ClientCache}, {@literal DEFAULT} {@link Pool}.
	 *
	 * @return the resolved {@link Pool} used to configure the {@link ClientCache}, {@literal DEFAULT} {@link Pool}.
	 * @see org.apache.geode.cache.client.PoolManager#find(String)
	 * @see org.apache.geode.cache.client.Pool
	 * @see #getPoolName()
	 * @see #getPool()
	 * @see #findPool(String)
	 * @see #isPoolNameResolvable(String)
	 */
	Pool resolvePool() {

		Pool pool = getPool();

		if (pool == null) {

			String poolName = resolvePoolName();

			pool = findPool(poolName);

			if (pool == null && isPoolNameResolvable(poolName)) {

				String dereferencedPoolName = SpringUtils.dereferenceBean(poolName);

				PoolFactoryBean poolFactoryBean =
					getBeanFactory().getBean(dereferencedPoolName, PoolFactoryBean.class);

				return poolFactoryBean.getPool();
			}
		}

		return pool;
	}

	String resolvePoolName() {

		return Optional.ofNullable(getPoolName())
			.filter(StringUtils::hasText)
			.orElse(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);
	}

	Pool findPool(String name) {
		return PoolManager.find(name);
	}

	private boolean isPoolNameResolvable(String poolName) {

		return Optional.ofNullable(poolName)
			.filter(getBeanFactory()::containsBean)
			.isPresent();
	}

	/**
	 * Creates a new {@link ClientCache} instance using the provided factory.
	 *
	 * @param <T> parameterized {@link Class} type extension of {@link GemFireCache}.
	 * @param factory instance of {@link ClientCacheFactory}.
	 * @return a new instance of {@link ClientCache} created by the provided factory.
	 * @see org.apache.geode.cache.client.ClientCacheFactory#create()
	 * @see org.apache.geode.cache.GemFireCache
	 */
	@Override
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T createCache(Object factory) {
		return (T) ((ClientCacheFactory) factory).create();
	}

	/**
	 * Inform the Pivotal GemFire/Apache Geode cluster that this cache client is ready to receive events
	 * iff the client is non-durable.
	 *
	 * @param event {@link ApplicationContextEvent} fired when the {@link ApplicationContext} is refreshed.
	 * @see org.apache.geode.cache.client.ClientCache#readyForEvents()
	 * @see #isReadyForEvents()
	 * @see #fetchCache()
	 */
	@Override
	public void onApplicationEvent(ContextRefreshedEvent event) {

		if (isReadyForEvents()) {
			try {
				this.<ClientCache>fetchCache().readyForEvents();
			}
			catch (IllegalStateException | CacheClosedException ignore) {
				// Thrown when clientCache.readyForEvents() is called on a non-durable client
			}
		}
	}

	/**
	 * Null-safe internal method used to close the {@link ClientCache} and preserve durability.
	 *
	 * @param cache {@link GemFireCache} to close.
	 * @see org.apache.geode.cache.client.ClientCache#close(boolean)
	 * @see #isKeepAlive()
	 */
	@Override
	protected void close(GemFireCache cache) {
		((ClientCache) cache).close(isKeepAlive());
	}

	/**
	 * Returns the {@link Class} type of the {@link GemFireCache} produced by this {@link ClientCacheFactoryBean}.
	 *
	 * @return the {@link Class} type of the {@link GemFireCache} produced by this {@link ClientCacheFactoryBean}.
	 * @see org.springframework.beans.factory.FactoryBean#getObjectType()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Class<? extends GemFireCache> getObjectType() {
		return Optional.ofNullable(getCache()).map(Object::getClass).orElse((Class) ClientCache.class);
	}

	public void addLocators(ConnectionEndpoint... locators) {
		this.locators.add(locators);
	}

	public void addLocators(Iterable<ConnectionEndpoint> locators) {
		this.locators.add(locators);
	}

	public void addServers(ConnectionEndpoint... servers) {
		this.servers.add(servers);
	}

	public void addServers(Iterable<ConnectionEndpoint> servers) {
		this.servers.add(servers);
	}

	/**
	 * Null-safe operation to set an array of {@link ClientCacheConfigurer ClientCacheConfigurers} used to apply
	 * additional configuration to this {@link ClientCacheFactoryBean} when using Annotation-based configuration.
	 *
	 * @param clientCacheConfigurers array of {@link ClientCacheConfigurer ClientCacheConfigurers} used to apply
	 * additional configuration to this {@link ClientCacheFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
	 * @see #setClientCacheConfigurers(List)
	 */
	public void setClientCacheConfigurers(ClientCacheConfigurer... clientCacheConfigurers) {
		setClientCacheConfigurers(Arrays.asList(nullSafeArray(clientCacheConfigurers, ClientCacheConfigurer.class)));
	}

	/**
	 * Null-safe operation to set an {@link Iterable} of {@link ClientCacheConfigurer ClientCacheConfigurers} to apply
	 * additional configuration to this {@link ClientCacheFactoryBean} when using Annotation-based configuration.
	 *
	 * @param peerCacheConfigurers {@link Iterable} of {@link ClientCacheConfigurer ClientCacheConfigurers} used to apply
	 * additional configuration to this {@link ClientCacheFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
	 */
	public void setClientCacheConfigurers(List<ClientCacheConfigurer> peerCacheConfigurers) {
		this.clientCacheConfigurers = Optional.ofNullable(peerCacheConfigurers).orElseGet(Collections::emptyList);
	}

	/**
	 * Returns a reference to the Composite {@link ClientCacheConfigurer} used to apply additional configuration
	 * to this {@link ClientCacheFactoryBean} on Spring container initialization.
	 *
	 * @return the Composite {@link ClientCacheConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
	 */
	public ClientCacheConfigurer getCompositeClientCacheConfigurer() {
		return this.compositeClientCacheConfigurer;
	}

	/**
	 * Set the GemFire System property 'durable-client-id' to indicate to the server that this client is durable.
	 *
	 * @param durableClientId a String value indicating the durable client id.
	 */
	public void setDurableClientId(String durableClientId) {
		this.durableClientId = durableClientId;
	}

	/**
	 * Gets the value of the GemFire System property 'durable-client-id' indicating to the server whether
	 * this client is durable.
	 *
	 * @return a String value indicating the durable client id.
	 */
	public String getDurableClientId() {
		return this.durableClientId;
	}

	/**
	 * Set the GemFire System property 'durable-client-timeout' indicating to the server how long to track events
	 * for the durable client when disconnected.
	 *
	 * @param durableClientTimeout an Integer value indicating the timeout in seconds for the server to keep
	 * the durable client's queue around.
	 */
	public void setDurableClientTimeout(Integer durableClientTimeout) {
		this.durableClientTimeout = durableClientTimeout;
	}

	/**
	 * Get the value of the GemFire System property 'durable-client-timeout' indicating to the server how long
	 * to track events for the durable client when disconnected.
	 *
	 * @return an Integer value indicating the timeout in seconds for the server to keep
	 * the durable client's queue around.
	 */
	public Integer getDurableClientTimeout() {
		return this.durableClientTimeout;
	}

	@Override
	public final void setEnableAutoReconnect(Boolean enableAutoReconnect) {
		throw new UnsupportedOperationException("Auto-reconnect does not apply to clients");
	}

	@Override
	public final Boolean getEnableAutoReconnect() {
		return Boolean.FALSE;
	}

	public void setFreeConnectionTimeout(Integer freeConnectionTimeout) {
		this.freeConnectionTimeout = freeConnectionTimeout;
	}

	public Integer getFreeConnectionTimeout() {
		return this.freeConnectionTimeout;
	}

	public void setIdleTimeout(Long idleTimeout) {
		this.idleTimeout = idleTimeout;
	}

	public Long getIdleTimeout() {
		return this.idleTimeout;
	}

	/**
	 * Sets whether the server(s) should keep the durable client's queue alive for the duration of the timeout
	 * when the client voluntarily disconnects.
	 *
	 * @param keepAlive a boolean value indicating to the server to keep the durable client's queues alive.
	 */
	public void setKeepAlive(Boolean keepAlive) {
		this.keepAlive = keepAlive;
	}

	/**
	 * Gets the user specified value for whether the server(s) should keep the durable client's queue alive
	 * for the duration of the timeout when the client voluntarily disconnects.
	 *
	 * @return a boolean value indicating whether the server should keep the durable client's queues alive.
	 */
	public Boolean getKeepAlive() {
		return this.keepAlive;
	}

	/**
	 * Determines whether the server(s) should keep the durable client's queue alive for the duration of the timeout
	 * when the client voluntarily disconnects.
	 *
	 * @return a boolean value indicating whether the server should keep the durable client's queues alive.
	 */
	public boolean isKeepAlive() {
		return Boolean.TRUE.equals(getKeepAlive());
	}

	public void setLoadConditioningInterval(Integer loadConditioningInterval) {
		this.loadConditioningInterval = loadConditioningInterval;
	}

	public Integer getLoadConditioningInterval() {
		return this.loadConditioningInterval;
	}

	public void setLocators(ConnectionEndpoint[] locators) {
		setLocators(ConnectionEndpointList.from(locators));
	}

	public void setLocators(Iterable<ConnectionEndpoint> locators) {
		getLocators().clear();
		addLocators(locators);
	}

	protected ConnectionEndpointList getLocators() {
		return this.locators;
	}

	public void setMaxConnections(Integer maxConnections) {
		this.maxConnections = maxConnections;
	}

	public Integer getMaxConnections() {
		return this.maxConnections;
	}

	public void setMinConnections(Integer minConnections) {
		this.minConnections = minConnections;
	}

	public Integer getMinConnections() {
		return this.minConnections;
	}

	public void setMultiUserAuthentication(Boolean multiUserAuthentication) {
		this.multiUserAuthentication = multiUserAuthentication;
	}

	public Boolean getMultiUserAuthentication() {
		return this.multiUserAuthentication;
	}

	/**
	 * Sets the {@link Pool} used by this cache client to obtain connections to the GemFire cluster.
	 *
	 * @param pool the GemFire {@link Pool} used by this {@link ClientCache} to obtain connections
	 * to the GemFire cluster.
	 * @throws IllegalArgumentException if the {@link Pool} is null.
	 */
	public void setPool(Pool pool) {
		this.pool = pool;
	}

	/**
	 * Gets the {@link Pool} used by this cache client to obtain connections to the GemFire cluster.
	 *
	 * @return the GemFire {@link Pool} used by this {@link ClientCache} to obtain connections
	 * to the GemFire cluster.
	 */
	public Pool getPool() {
		return this.pool;
	}

	/**
	 * Sets the name of the {@link Pool} used by this cache client to obtain connections to the GemFire cluster.
	 *
	 * @param poolName set the name of the GemFire {@link Pool} used by this GemFire {@link ClientCache}.
	 * @throws IllegalArgumentException if the {@link Pool} name is unspecified.
	 */
	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	/**
	 * Gets the name of the GemFire {@link Pool} used by this GemFire cache client.
	 *
	 * @return the name of the GemFire {@link Pool} used by this GemFire cache client.
	 */
	public String getPoolName() {
		return this.poolName;
	}

	public void setPingInterval(Long pingInterval) {
		this.pingInterval = pingInterval;
	}

	public Long getPingInterval() {
		return this.pingInterval;
	}

	public void setPrSingleHopEnabled(Boolean prSingleHopEnabled) {
		this.prSingleHopEnabled = prSingleHopEnabled;
	}

	public Boolean getPrSingleHopEnabled() {
		return this.prSingleHopEnabled;
	}

	public void setReadTimeout(Integer readTimeout) {
		this.readTimeout = readTimeout;
	}

	public Integer getReadTimeout() {
		return this.readTimeout;
	}

	/**
	 * Sets the readyForEvents property to indicate whether the cache client should notify the server
	 * that it is ready to receive updates.
	 *
	 * @param readyForEvents sets a boolean flag to notify the server that this durable client
	 * is ready to receive updates.
	 * @see #getReadyForEvents()
	 */
	public void setReadyForEvents(Boolean readyForEvents){
		this.readyForEvents = readyForEvents;
	}

	/**
	 * Gets the user-specified value for the readyForEvents property.
	 *
	 * @return a boolean value indicating the state of the 'readyForEvents' property.
	 */
	public Boolean getReadyForEvents(){
		return this.readyForEvents;
	}

	/**
	 * Determines whether this GemFire cache client is ready for events.  If 'readyForEvents' was explicitly set,
	 * then it takes precedence over all other considerations (e.g. durability).
	 *
	 * @return a boolean value indicating whether this GemFire cache client is ready for events.
	 * @see org.springframework.data.gemfire.GemfireUtils#isDurable(ClientCache)
	 * @see #getReadyForEvents()
	 */
	public boolean isReadyForEvents() {

		Boolean readyForEvents = getReadyForEvents();

		if (readyForEvents != null) {
			return Boolean.TRUE.equals(readyForEvents);
		}
		else {
			try {
				return GemfireUtils.isDurable(fetchCache());
			}
			catch (Throwable ignore) {
				return false;
			}
		}
	}

	public void setRetryAttempts(Integer retryAttempts) {
		this.retryAttempts = retryAttempts;
	}

	public Integer getRetryAttempts() {
		return this.retryAttempts;
	}

	public void setServerGroup(String serverGroup) {
		this.serverGroup = serverGroup;
	}

	public String getServerGroup() {
		return this.serverGroup;
	}

	public void setServers(ConnectionEndpoint[] servers) {
		setServers(ConnectionEndpointList.from(servers));
	}

	public void setServers(Iterable<ConnectionEndpoint> servers) {
		getServers().clear();
		addServers(servers);
	}

	protected ConnectionEndpointList getServers() {
		return this.servers;
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public Integer getSocketBufferSize() {
		return this.socketBufferSize;
	}

	public void setSocketConnectTimeout(Integer socketConnectTimeout) {
		this.socketConnectTimeout = socketConnectTimeout;
	}

	public Integer getSocketConnectTimeout() {
		return this.socketConnectTimeout;
	}

	public void setStatisticsInterval(Integer statisticsInterval) {
		this.statisticsInterval = statisticsInterval;
	}

	public Integer getStatisticsInterval() {
		return this.statisticsInterval;
	}

	public void setSubscriptionAckInterval(Integer subscriptionAckInterval) {
		this.subscriptionAckInterval = subscriptionAckInterval;
	}

	public Integer getSubscriptionAckInterval() {
		return this.subscriptionAckInterval;
	}

	public void setSubscriptionEnabled(Boolean subscriptionEnabled) {
		this.subscriptionEnabled = subscriptionEnabled;
	}

	public Boolean getSubscriptionEnabled() {
		return this.subscriptionEnabled;
	}

	public void setSubscriptionMessageTrackingTimeout(Integer subscriptionMessageTrackingTimeout) {
		this.subscriptionMessageTrackingTimeout = subscriptionMessageTrackingTimeout;
	}

	public Integer getSubscriptionMessageTrackingTimeout() {
		return this.subscriptionMessageTrackingTimeout;
	}

	public void setSubscriptionRedundancy(Integer subscriptionRedundancy) {
		this.subscriptionRedundancy = subscriptionRedundancy;
	}

	public Integer getSubscriptionRedundancy() {
		return this.subscriptionRedundancy;
	}

	public void setThreadLocalConnections(Boolean threadLocalConnections) {
		this.threadLocalConnections = threadLocalConnections;
	}

	public Boolean getThreadLocalConnections() {
		return this.threadLocalConnections;
	}

	@Override
	public final void setUseClusterConfiguration(Boolean useClusterConfiguration) {
		throw new UnsupportedOperationException("Cluster-based Configuration is not applicable for clients");
	}

	@Override
	public final Boolean getUseClusterConfiguration() {
		return Boolean.FALSE;
	}
}
