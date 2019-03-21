/*
 * Copyright 2011-2019 the original author or authors.
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
 */
package org.springframework.data.gemfire.server;

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.InterestRegistrationListener;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.apache.geode.cache.server.ServerLoadProbe;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.SmartLifecycle;
import org.springframework.data.gemfire.config.annotation.CacheServerConfigurer;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} used to construct, configure and initialize a {@link CacheServer}.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.context.SmartLifecycle
 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfigurer
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 */
@SuppressWarnings("unused")
public class CacheServerFactoryBean extends AbstractFactoryBeanSupport<CacheServer>
		implements DisposableBean, InitializingBean, SmartLifecycle {

	private boolean autoStartup = true;
	private boolean notifyBySubscription = CacheServer.DEFAULT_NOTIFY_BY_SUBSCRIPTION;
	private boolean tcpNoDelay = CacheServer.DEFAULT_TCP_NO_DELAY;

	private int maxConnections = CacheServer.DEFAULT_MAX_CONNECTIONS;
	private int maxMessageCount = CacheServer.DEFAULT_MAXIMUM_MESSAGE_COUNT;
	private int maxThreads = CacheServer.DEFAULT_MAX_THREADS;
	private int maxTimeBetweenPings = CacheServer.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS;
	private int messageTimeToLive = CacheServer.DEFAULT_MESSAGE_TIME_TO_LIVE;
	private int port = CacheServer.DEFAULT_PORT;
	private int socketBufferSize = CacheServer.DEFAULT_SOCKET_BUFFER_SIZE;
	private int subscriptionCapacity = ClientSubscriptionConfig.DEFAULT_CAPACITY;

	private long loadPollInterval = CacheServer.DEFAULT_LOAD_POLL_INTERVAL;

	private Cache cache;

	private CacheServer cacheServer;

	private List<CacheServerConfigurer> cacheServerConfigurers = Collections.emptyList();

	private CacheServerConfigurer compositeCacheServerConfigurer = (beanName, bean) ->
		nullSafeCollection(cacheServerConfigurers).forEach(cacheServerConfigurer ->
			cacheServerConfigurer.configure(beanName, bean));

	private ServerLoadProbe serverLoadProbe = CacheServer.DEFAULT_LOAD_PROBE;

	private Set<InterestRegistrationListener> listeners = Collections.emptySet();

	private String bindAddress = CacheServer.DEFAULT_BIND_ADDRESS;
	private String hostNameForClients = CacheServer.DEFAULT_HOSTNAME_FOR_CLIENTS;
	private String subscriptionDiskStore;

	private String[] serverGroups = {};

	private SubscriptionEvictionPolicy subscriptionEvictionPolicy = SubscriptionEvictionPolicy.DEFAULT;

	/**
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("deprecation")
	public void afterPropertiesSet() throws IOException {

		applyCacheServerConfigurers();

		Cache cache = resolveCache();

		this.cacheServer = postProcess(configure(addCacheServer(cache)));
	}

	/* (non-Javadoc) */
	private void applyCacheServerConfigurers() {
		applyCacheServerConfigurers(getCompositeCacheServerConfigurer());
	}

	/**
	 * Null-safe operation to apply the given array of {@link CacheServerConfigurer CacheServerConfigurers}
	 * to this {@link CacheServerFactoryBean}.
	 *
	 * @param cacheServerConfigurers array of {@link CacheServerConfigurer CacheServerConfigurers} applied to
	 * this {@link CacheServerFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfigurer
	 * @see #applyCacheServerConfigurers(Iterable)
	 */
	protected void applyCacheServerConfigurers(CacheServerConfigurer... cacheServerConfigurers) {
		applyCacheServerConfigurers(Arrays.asList(nullSafeArray(cacheServerConfigurers, CacheServerConfigurer.class)));
	}

	/**
	 * Null-safe operation to apply the given {@link Iterable} of {@link CacheServerConfigurer CacheServerConfigurers}
	 * to this {@link CacheServerFactoryBean}.
	 *
	 * @param cacheServerConfigurers {@link Iterable} of {@link CacheServerConfigurer CacheServerConfigurers} applied to
	 * this {@link CacheServerFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfigurer
	 */
	protected void applyCacheServerConfigurers(Iterable<CacheServerConfigurer> cacheServerConfigurers) {
		stream(nullSafeIterable(cacheServerConfigurers).spliterator(), false)
			.forEach(cacheServerConfigurer -> cacheServerConfigurer.configure(getBeanName(), this));
	}

	/* (non-Javadoc) */
	private Cache resolveCache() {
		return Optional.ofNullable(this.cache)
			.orElseThrow(() -> newIllegalArgumentException("Cache is required"));
	}

	/**
	 * Adds a {@link CacheServer} to the given {@link Cache} for server {@link ClientCache cache clients}.
	 *
	 * @param cache {@link Cache} used to add a {@link CacheServer}.
	 * @return the newly added {@link CacheServer}.
	 * @see org.apache.geode.cache.Cache#addCacheServer()
	 * @see org.apache.geode.cache.server.CacheServer
	 */
	protected CacheServer addCacheServer(Cache cache) {
		return cache.addCacheServer();
	}

	/**
	 * Configures the provided {@link CacheServer} with any custom, application-specific configuration.
	 *
	 * @param cacheServer {@link CacheServer} to configure.
	 * @return the given {@link CacheServer}.
	 * @see org.apache.geode.cache.server.CacheServer
	 */
	protected CacheServer configure(CacheServer cacheServer) {

		cacheServer.setBindAddress(this.bindAddress);
		cacheServer.setGroups(this.serverGroups);
		cacheServer.setHostnameForClients(this.hostNameForClients);
		cacheServer.setLoadPollInterval(this.loadPollInterval);
		cacheServer.setLoadProbe(this.serverLoadProbe);
		cacheServer.setMaxConnections(this.maxConnections);
		cacheServer.setMaximumMessageCount(this.maxMessageCount);
		cacheServer.setMaximumTimeBetweenPings(this.maxTimeBetweenPings);
		cacheServer.setMaxThreads(this.maxThreads);
		cacheServer.setMessageTimeToLive(this.messageTimeToLive);
		cacheServer.setNotifyBySubscription(this.notifyBySubscription);
		cacheServer.setPort(this.port);
		cacheServer.setSocketBufferSize(this.socketBufferSize);
		cacheServer.setTcpNoDelay(this.tcpNoDelay);

		nullSafeCollection(this.listeners).forEach(cacheServer::registerInterestRegistrationListener);

		ClientSubscriptionConfig config = cacheServer.getClientSubscriptionConfig();

		config.setCapacity(this.subscriptionCapacity);
		getSubscriptionEvictionPolicy().setEvictionPolicy(config);

		Optional.ofNullable(this.subscriptionDiskStore).filter(StringUtils::hasText)
			.ifPresent(config::setDiskStoreName);

		return cacheServer;
	}

	/**
	 * Post-process the {@link CacheServer} with any necessary follow-up actions.
	 *
	 * @param cacheServer {@link CacheServer} to process.
	 * @return the given {@link CacheServer}.
	 * @see org.apache.geode.cache.server.CacheServer
	 */
	protected CacheServer postProcess(CacheServer cacheServer) {
		return cacheServer;
	}

	/**
	 * Returns a reference to the Composite {@link CacheServerConfigurer} used to apply additional configuration
	 * to this {@link CacheServerFactoryBean} on Spring container initialization.
	 *
	 * @return the Composite {@link CacheServerConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfigurer
	 */
	protected CacheServerConfigurer getCompositeCacheServerConfigurer() {
		return this.compositeCacheServerConfigurer;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public CacheServer getObject() {
		return this.cacheServer;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Class<?> getObjectType() {
		return Optional.ofNullable(this.cacheServer).map(CacheServer::getClass).orElse((Class) CacheServer.class);
	}

	/* (non-Javadoc) */
	public boolean isRunning() {
		return Optional.ofNullable(this.cacheServer).map(CacheServer::isRunning).orElse(false);
	}

	/* (non-Javadoc) */
	public boolean isAutoStartup() {
		return this.autoStartup;
	}

	/**
	 * Start at the latest possible moment.
	 */
	public int getPhase() {
		return Integer.MAX_VALUE;
	}

	/* (non-Javadoc) */
	public void destroy() {
		stop();
		this.cacheServer = null;
	}

	/* (non-Javadoc) */
	@Override
	public void start() {
		try {
			cacheServer.start();
		}
		catch (IOException e) {
			throw new BeanInitializationException("Cannot start cache server", e);
		}
	}

	/* (non-Javadoc) */
	@Override
	public void stop(Runnable callback) {
		stop();
		callback.run();
	}

	/* (non-Javadoc) */
	public void stop() {
		Optional.ofNullable(this.cacheServer).ifPresent(CacheServer::stop);
	}

	/* (non-Javadoc) */
	public void setAutoStartup(boolean autoStartup) {
		this.autoStartup = autoStartup;
	}

	/* (non-Javadoc) */
	public void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

	/* (non-Javadoc) */
	public void setCache(Cache cache) {
		this.cache = cache;
	}

	/*
	 * (non-Javadoc)
	 * For testing purposes only!
	 */
	void setCacheServer(CacheServer cacheServer) {
		this.cacheServer = cacheServer;
	}

	/**
	 * Null-safe operation to set an array of {@link CacheServerConfigurer CacheServerConfigurers} used to apply
	 * additional configuration to this {@link CacheServerFactoryBean} when using Annotation-based configuration.
	 *
	 * @param cacheServerConfigurers array of {@link CacheServerConfigurer CacheServerConfigurers} used to apply
	 * additional configuration to this {@link CacheServerFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
	 * @see #setCacheServerConfigurers(List)
	 */
	public void setCacheServerConfigurers(CacheServerConfigurer... cacheServerConfigurers) {
		setCacheServerConfigurers(Arrays.asList(nullSafeArray(cacheServerConfigurers, CacheServerConfigurer.class)));
	}

	/**
	 * Null-safe operation to set an {@link Iterable} of {@link CacheServerConfigurer CacheServerConfigurers}
	 * used to apply additional configuration to this {@link CacheServerFactoryBean} when using
	 * Annotation-based configuration.
	 *
	 * @param cacheServerConfigurers {@literal Iterable} of {@link CacheServerConfigurer CacheServerConfigurers}
	 * used to apply additional configuration to this {@link CacheServerFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
	 */
	public void setCacheServerConfigurers(List<CacheServerConfigurer> cacheServerConfigurers) {
		this.cacheServerConfigurers = Optional.ofNullable(cacheServerConfigurers).orElseGet(Collections::emptyList);
	}

	/* (non-Javadoc) */
	public void setHostNameForClients(String hostNameForClients) {
		this.hostNameForClients = hostNameForClients;
	}

	/* (non-Javadoc) */
	public void setListeners(Set<InterestRegistrationListener> listeners) {
		this.listeners = listeners;
	}

	/* (non-Javadoc) */
	public void setLoadPollInterval(long loadPollInterval) {
		this.loadPollInterval = loadPollInterval;
	}

	/* (non-Javadoc) */
	public void setMaxConnections(int maxConnections) {
		this.maxConnections = maxConnections;
	}

	/* (non-Javadoc) */
	public void setMaxMessageCount(int maxMessageCount) {
		this.maxMessageCount = maxMessageCount;
	}

	/* (non-Javadoc) */
	public void setMaxThreads(int maxThreads) {
		this.maxThreads = maxThreads;
	}

	/* (non-Javadoc) */
	public void setMaxTimeBetweenPings(int maxTimeBetweenPings) {
		this.maxTimeBetweenPings = maxTimeBetweenPings;
	}

	/* (non-Javadoc) */
	public void setMessageTimeToLive(int messageTimeToLive) {
		this.messageTimeToLive = messageTimeToLive;
	}

	/* (non-Javadoc) */
	public void setNotifyBySubscription(boolean notifyBySubscription) {
		this.notifyBySubscription = notifyBySubscription;
	}

	/* (non-Javadoc) */
	public void setPort(int port) {
		this.port = port;
	}

	/* (non-Javadoc) */
	public void setServerGroups(String[] serverGroups) {
		this.serverGroups = serverGroups;
	}

	/* (non-Javadoc) */
	public void setServerLoadProbe(ServerLoadProbe serverLoadProbe) {
		this.serverLoadProbe = serverLoadProbe;
	}

	/* (non-Javadoc) */
	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	/* (non-Javadoc) */
	public void setSubscriptionCapacity(int subscriptionCapacity) {
		this.subscriptionCapacity = subscriptionCapacity;
	}

	/* (non-Javadoc) */
	public void setSubscriptionDiskStore(String diskStoreName) {
		this.subscriptionDiskStore = diskStoreName;
	}

	/* (non-Javadoc) */
	SubscriptionEvictionPolicy getSubscriptionEvictionPolicy() {
		return Optional.ofNullable(this.subscriptionEvictionPolicy).orElse(SubscriptionEvictionPolicy.DEFAULT);
	}

	/* (non-Javadoc) */
	public void setSubscriptionEvictionPolicy(SubscriptionEvictionPolicy evictionPolicy) {
		this.subscriptionEvictionPolicy = evictionPolicy;
	}

	/* (non-Javadoc) */
	public void setTcpNoDelay(boolean tcpNoDelay) {
		this.tcpNoDelay = tcpNoDelay;
	}
}
