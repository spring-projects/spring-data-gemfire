/*
 * Copyright 2011 the original author or authors.
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

import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.io.Resource;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.InterestRegistrationListener;
import com.gemstone.gemfire.cache.server.CacheServer;
import com.gemstone.gemfire.cache.server.ClientSubscriptionConfig;
import com.gemstone.gemfire.cache.server.ServerLoadProbe;

/**
 * FactoryBean for easy creation and configuration of GemFire {@link CacheServer} instances.
 * 
 * @author Costin Leau
 */
public class CacheServerFactoryBean implements FactoryBean<CacheServer>, InitializingBean, DisposableBean,
		SmartLifecycle {

	private boolean autoStartup = true;

	private int port = CacheServer.DEFAULT_PORT;
	private int maxConnections = CacheServer.DEFAULT_MAX_CONNECTIONS;
	private int maxThreads = CacheServer.DEFAULT_MAX_THREADS;
	private boolean notifyBySubscription = CacheServer.DEFAULT_NOTIFY_BY_SUBSCRIPTION;
	private int socketBufferSize = CacheServer.DEFAULT_SOCKET_BUFFER_SIZE;
	private int maxTimeBetweenPings = CacheServer.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS;
	private int maxMessageCount = CacheServer.DEFAULT_MAXIMUM_MESSAGE_COUNT;
	private int messageTimeToLive = CacheServer.DEFAULT_MESSAGE_TIME_TO_LIVE;
	private String[] serverGroups = CacheServer.DEFAULT_GROUPS;
	private ServerLoadProbe serverLoadProbe = CacheServer.DEFAULT_LOAD_PROBE;
	private long loadPollInterval = CacheServer.DEFAULT_LOAD_POLL_INTERVAL;
	private String bindAddress = CacheServer.DEFAULT_BIND_ADDRESS;
	private String hostNameForClients = CacheServer.DEFAULT_HOSTNAME_FOR_CLIENTS;
	private Set<InterestRegistrationListener> listeners = Collections.emptySet();

	private SubscriptionEvictionPolicy evictionPolicy = SubscriptionEvictionPolicy.valueOf(ClientSubscriptionConfig.DEFAULT_EVICTION_POLICY.toUpperCase());
	private int subscriptionCapacity = ClientSubscriptionConfig.DEFAULT_CAPACITY;
	private Resource subscriptionDiskStore;

	private Cache cache;
	private CacheServer cacheServer;


	public CacheServer getObject() {
		return cacheServer;
	}

	public Class<?> getObjectType() {
		return ((this.cacheServer != null) ? cacheServer.getClass() : CacheServer.class);
	}

	public boolean isSingleton() {
		return true;
	}

	public void afterPropertiesSet() throws IOException {
		Assert.notNull(cache, "cache is required");

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

		// client config
		ClientSubscriptionConfig config = cacheServer.getClientSubscriptionConfig();
		config.setCapacity(subscriptionCapacity);
		config.setEvictionPolicy(evictionPolicy.name().toLowerCase());
		if (subscriptionDiskStore != null)
			config.setDiskStoreName(subscriptionDiskStore.getFile().getCanonicalPath());
	}

	public void destroy() {
		stop();
		cacheServer = null;
	}

	public boolean isAutoStartup() {
		return autoStartup;
	}

	public void stop(Runnable callback) {
		stop();
		callback.run();
	}

	public int getPhase() {
		// start the latest
		return Integer.MAX_VALUE;
	}

	public boolean isRunning() {
		return (cacheServer != null ? cacheServer.isRunning() : false);
	}

	public void start() {
		try {
			cacheServer.start();
		} catch (IOException ex) {
			throw new BeanInitializationException("Cannot start cache server", ex);
		}
	}

	public void stop() {
		if (cacheServer != null) {
			cacheServer.stop();
		}
	}

	public void setAutoStartup(boolean autoStartup) {
		this.autoStartup = autoStartup;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public void setMaxConnections(int maxConnections) {
		this.maxConnections = maxConnections;
	}

	public void setMaxThreads(int maxThreads) {
		this.maxThreads = maxThreads;
	}

	public void setNotifyBySubscription(boolean notifyBySubscription) {
		this.notifyBySubscription = notifyBySubscription;
	}

	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public void setMaxTimeBetweenPings(int maxTimeBetweenPings) {
		this.maxTimeBetweenPings = maxTimeBetweenPings;
	}

	public void setMaxMessageCount(int maxMessageCount) {
		this.maxMessageCount = maxMessageCount;
	}

	public void setMessageTimeToLive(int messageTimeToLive) {
		this.messageTimeToLive = messageTimeToLive;
	}

	public void setServerGroups(String[] serverGroups) {
		this.serverGroups = serverGroups;
	}

	public void setServerLoadProbe(ServerLoadProbe serverLoadProbe) {
		this.serverLoadProbe = serverLoadProbe;
	}

	public void setLoadPollInterval(long loadPollInterval) {
		this.loadPollInterval = loadPollInterval;
	}

	public void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

	public void setHostNameForClients(String hostNameForClients) {
		this.hostNameForClients = hostNameForClients;
	}

	public void setListeners(Set<InterestRegistrationListener> listeners) {
		this.listeners = listeners;
	}

	public void setCache(Cache cache) {
		this.cache = cache;
	}

	/**
	 * @param evictionPolicy the evictionPolicy to set
	 */
	public void setSubscriptionEvictionPolicy(SubscriptionEvictionPolicy evictionPolicy) {
		this.evictionPolicy = evictionPolicy;
	}

	/**
	 * @param subscriptionCapacity the subscriptionCapacity to set
	 */
	public void setSubscriptionCapacity(int subscriptionCapacity) {
		this.subscriptionCapacity = subscriptionCapacity;
	}

	public void setSubscriptionDiskStore(Resource resource) {
		this.subscriptionDiskStore = resource;
	}
}