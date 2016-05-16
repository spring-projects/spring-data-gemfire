/*
 * Copyright 2011-2013 the original author or authors.
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
package org.springframework.data.gemfire.server;

import java.io.IOException;
import java.util.Collections;
import java.util.Set;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.InterestRegistrationListener;
import com.gemstone.gemfire.cache.server.CacheServer;
import com.gemstone.gemfire.cache.server.ClientSubscriptionConfig;
import com.gemstone.gemfire.cache.server.ServerLoadProbe;

import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.SmartLifecycle;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * FactoryBean for easy creation and configuration of GemFire {@link CacheServer} instances.
 *
 * @author Costin Leau
 * @author John Blum
 */
@SuppressWarnings("unused")
public class CacheServerFactoryBean implements FactoryBean<CacheServer>, InitializingBean, DisposableBean,
		SmartLifecycle {

	private boolean autoStartup = true;
	private boolean notifyBySubscription = CacheServer.DEFAULT_NOTIFY_BY_SUBSCRIPTION;

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

	private ServerLoadProbe serverLoadProbe = CacheServer.DEFAULT_LOAD_PROBE;

	private Set<InterestRegistrationListener> listeners = Collections.emptySet();

	private String bindAddress = CacheServer.DEFAULT_BIND_ADDRESS;
	private String hostNameForClients = CacheServer.DEFAULT_HOSTNAME_FOR_CLIENTS;
	private String subscriptionDiskStore;

	private String[] serverGroups = {};

	private SubscriptionEvictionPolicy subscriptionEvictionPolicy = SubscriptionEvictionPolicy.DEFAULT;

	public CacheServer getObject() {
		return cacheServer;
	}

	public Class<?> getObjectType() {
		return (this.cacheServer != null ? cacheServer.getClass() : CacheServer.class);
	}

	public boolean isSingleton() {
		return true;
	}

	@SuppressWarnings("deprecation")
	public void afterPropertiesSet() throws IOException {
		Assert.notNull(cache, "A GemFire Cache is required.");

		cacheServer = cache.addCacheServer();
		cacheServer.setBindAddress(bindAddress);
		cacheServer.setGroups(serverGroups);
		cacheServer.setHostnameForClients(hostNameForClients);
		cacheServer.setLoadPollInterval(loadPollInterval);
		cacheServer.setLoadProbe(serverLoadProbe);
		cacheServer.setMaxConnections(maxConnections);
		cacheServer.setMaximumMessageCount(maxMessageCount);
		cacheServer.setMaximumTimeBetweenPings(maxTimeBetweenPings);
		cacheServer.setMaxThreads(maxThreads);
		cacheServer.setMessageTimeToLive(messageTimeToLive);
		cacheServer.setNotifyBySubscription(notifyBySubscription);
		cacheServer.setPort(port);
		cacheServer.setSocketBufferSize(socketBufferSize);

		for (InterestRegistrationListener listener : listeners) {
			cacheServer.registerInterestRegistrationListener(listener);
		}

		ClientSubscriptionConfig config = cacheServer.getClientSubscriptionConfig();

		config.setCapacity(subscriptionCapacity);
		getSubscriptionEvictionPolicy().setEvictionPolicy(config);

		if (StringUtils.hasText(subscriptionDiskStore)) {
			config.setDiskStoreName(subscriptionDiskStore);
		}
	}

	public boolean isAutoStartup() {
		return autoStartup;
	}

	public boolean isRunning() {
		return (cacheServer != null && cacheServer.isRunning());
	}

	public int getPhase() {
		// start at the latest possible moment
		return Integer.MAX_VALUE;
	}

	public void destroy() {
		stop();
		cacheServer = null;
	}

	public void start() {
		try {
			cacheServer.start();
		}
		catch (IOException e) {
			throw new BeanInitializationException("Cannot start cache server", e);
		}
	}

	public void stop(final Runnable callback) {
		stop();
		callback.run();
	}

	public void stop() {
		if (cacheServer != null) {
			cacheServer.stop();
		}
	}

	public void setAutoStartup(boolean autoStartup) {
		this.autoStartup = autoStartup;
	}

	public void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

	public void setCache(final Cache cache) {
		this.cache = cache;
	}

	/*
	 * (non-Javadoc)
	 * For testing purposes only!
	 */
	void setCacheServer(final CacheServer cacheServer) {
		this.cacheServer = cacheServer;
	}

	public void setHostNameForClients(String hostNameForClients) {
		this.hostNameForClients = hostNameForClients;
	}

	public void setListeners(Set<InterestRegistrationListener> listeners) {
		this.listeners = listeners;
	}

	public void setLoadPollInterval(long loadPollInterval) {
		this.loadPollInterval = loadPollInterval;
	}

	public void setMaxConnections(int maxConnections) {
		this.maxConnections = maxConnections;
	}

	public void setMaxMessageCount(int maxMessageCount) {
		this.maxMessageCount = maxMessageCount;
	}

	public void setMaxThreads(int maxThreads) {
		this.maxThreads = maxThreads;
	}

	public void setMaxTimeBetweenPings(int maxTimeBetweenPings) {
		this.maxTimeBetweenPings = maxTimeBetweenPings;
	}

	public void setMessageTimeToLive(int messageTimeToLive) {
		this.messageTimeToLive = messageTimeToLive;
	}

	public void setNotifyBySubscription(boolean notifyBySubscription) {
		this.notifyBySubscription = notifyBySubscription;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public void setServerGroups(String[] serverGroups) {
		this.serverGroups = serverGroups;
	}

	public void setServerLoadProbe(ServerLoadProbe serverLoadProbe) {
		this.serverLoadProbe = serverLoadProbe;
	}

	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public void setSubscriptionCapacity(int subscriptionCapacity) {
		this.subscriptionCapacity = subscriptionCapacity;
	}

	public void setSubscriptionDiskStore(String diskStoreName) {
		this.subscriptionDiskStore = diskStoreName;
	}

	SubscriptionEvictionPolicy getSubscriptionEvictionPolicy() {
		return (subscriptionEvictionPolicy != null ? subscriptionEvictionPolicy : SubscriptionEvictionPolicy.DEFAULT);
	}

	public void setSubscriptionEvictionPolicy(SubscriptionEvictionPolicy evictionPolicy) {
		this.subscriptionEvictionPolicy = evictionPolicy;
	}

}
