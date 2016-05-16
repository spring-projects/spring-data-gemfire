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

package org.springframework.data.gemfire.config.annotation;

import java.util.Map;
import java.util.Set;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.InterestRegistrationListener;
import com.gemstone.gemfire.cache.server.CacheServer;
import com.gemstone.gemfire.cache.server.ClientSubscriptionConfig;
import com.gemstone.gemfire.cache.server.ServerLoadProbe;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.server.CacheServerFactoryBean;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;

/**
 * Spring {@link Configuration} class used to configure, construct and initialize and GemFire {@link CacheServer}
 * instance in a Spring application context.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfiguration
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.server.CacheServer
 * @since 1.9.0
 */
@Configuration
@SuppressWarnings("unused")
public class CacheServerConfiguration extends PeerCacheConfiguration {

	protected static final boolean DEFAULT_AUTO_STARTUP = true;

	protected static final String DEFAULT_NAME = "SpringBasedCacheServerApplication";

	private boolean autoStartup = DEFAULT_AUTO_STARTUP;

	private Integer maxConnections;
	private Integer maxMessageCount;
	private Integer maxThreads;
	private Integer maxTimeBetweenPings;
	private Integer messageTimeToLive;
	private Integer port;
	private Integer socketBufferSize;
	private Integer subscriptionCapacity;

	private Long loadPollInterval;

	private ServerLoadProbe serverLoadProbe;

	private Set<InterestRegistrationListener> interestRegistrationListeners;

	private String bindAddress;
	private String hostnameForClients;
	private String subscriptionDiskStoreName;

	private SubscriptionEvictionPolicy subscriptionEvictionPolicy;

	@Bean
	public CacheServerFactoryBean gemfireCacheServer(Cache gemfireCache) {
		CacheServerFactoryBean gemfireCacheServer = new CacheServerFactoryBean();

		gemfireCacheServer.setCache(gemfireCache);
		gemfireCacheServer.setAutoStartup(autoStartup());
		gemfireCacheServer.setBindAddress(bindAddress());
		gemfireCacheServer.setHostNameForClients(hostnameForClients());
		gemfireCacheServer.setListeners(interestRegistrationListeners());
		gemfireCacheServer.setLoadPollInterval(loadPollInterval());
		gemfireCacheServer.setMaxConnections(maxConnections());
		gemfireCacheServer.setMaxMessageCount(maxMessageCount());
		gemfireCacheServer.setMaxThreads(maxThreads());
		gemfireCacheServer.setMaxTimeBetweenPings(maxTimeBetweenPings());
		gemfireCacheServer.setMessageTimeToLive(messageTimeToLive());
		gemfireCacheServer.setPort(port());
		gemfireCacheServer.setServerLoadProbe(serverLoadProbe());
		gemfireCacheServer.setSocketBufferSize(socketBufferSize());
		gemfireCacheServer.setSubscriptionCapacity(subscriptionCapacity());
		gemfireCacheServer.setSubscriptionDiskStore(subscriptionDiskStoreName());
		gemfireCacheServer.setSubscriptionEvictionPolicy(subscriptionEvictionPolicy());

		return gemfireCacheServer;
	}

	/**
	 * Configures GemFire {@link CacheServer} specific settings.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing cache server meta-data used to configure
	 * the GemFire {@link CacheServer}.
	 * @see org.springframework.core.type.AnnotationMetadata
	 */
	@Override
	protected void configureOther(AnnotationMetadata importMetadata) {
		super.configureCache(importMetadata);

		if (isCacheServerApplication(importMetadata)) {
			Map<String, Object> cacheServerApplicationMetadata =
				importMetadata.getAnnotationAttributes(getAnnotationTypeName());

			setAutoStartup(Boolean.TRUE.equals(cacheServerApplicationMetadata.get("autoStartup")));
			setBindAddress((String) cacheServerApplicationMetadata.get("bindAddress"));
			setHostnameForClients((String) cacheServerApplicationMetadata.get("hostnameForClients"));
			setLoadPollInterval((Long) cacheServerApplicationMetadata.get("loadPollInterval"));
			setMaxConnections((Integer) cacheServerApplicationMetadata.get("maxConnections"));
			setMaxMessageCount((Integer) cacheServerApplicationMetadata.get("maxMessageCount"));
			setMaxThreads((Integer) cacheServerApplicationMetadata.get("maxThreads"));
			setMaxTimeBetweenPings((Integer) cacheServerApplicationMetadata.get("maxTimeBetweenPings"));
			setMessageTimeToLive((Integer) cacheServerApplicationMetadata.get("messageTimeToLive"));
			setPort((Integer) cacheServerApplicationMetadata.get("port"));
			setSocketBufferSize((Integer) cacheServerApplicationMetadata.get("socketBufferSize"));
			setSubscriptionCapacity((Integer) cacheServerApplicationMetadata.get("subscriptionCapacity"));
			setSubscriptionDiskStoreName((String) cacheServerApplicationMetadata.get("subscriptionDiskStoreName"));
			setSubscriptionEvictionPolicy((SubscriptionEvictionPolicy) cacheServerApplicationMetadata.get(
				"subscriptionEvictionPolicy"));
		}
	}

	@Override
	protected Class getAnnotationType() {
		return CacheServerApplication.class;
	}

	/* (non-Javadoc) */
	void setAutoStartup(boolean autoStartup) {
		this.autoStartup = autoStartup;
	}

	protected boolean autoStartup() {
		return this.autoStartup;
	}

	/* (non-Javadoc) */
	void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

	protected String bindAddress() {
		return nullSafeValue(this.bindAddress, CacheServer.DEFAULT_BIND_ADDRESS);
	}

	/* (non-Javadoc) */
	void setHostnameForClients(String hostnameForClients) {
		this.hostnameForClients = hostnameForClients;
	}

	protected String hostnameForClients() {
		return nullSafeValue(this.hostnameForClients, CacheServer.DEFAULT_HOSTNAME_FOR_CLIENTS);
	}

	/* (non-Javadoc) */
	void setInterestRegistrationListeners(Set<InterestRegistrationListener> interestRegistrationListeners) {
		this.interestRegistrationListeners = interestRegistrationListeners;
	}

	protected Set<InterestRegistrationListener> interestRegistrationListeners() {
		return nullSafeSet(this.interestRegistrationListeners);
	}

	/* (non-Javadoc) */
	void setLoadPollInterval(Long loadPollInterval) {
		this.loadPollInterval = loadPollInterval;
	}

	protected Long loadPollInterval() {
		return nullSafeValue(this.loadPollInterval, CacheServer.DEFAULT_LOAD_POLL_INTERVAL);
	}

	/* (non-Javadoc) */
	void setMaxConnections(Integer maxConnections) {
		this.maxConnections = maxConnections;
	}

	protected Integer maxConnections() {
		return nullSafeValue(this.maxConnections, CacheServer.DEFAULT_MAX_CONNECTIONS);
	}

	/* (non-Javadoc) */
	void setMaxMessageCount(Integer maxMessageCount) {
		this.maxMessageCount = maxMessageCount;
	}

	protected Integer maxMessageCount() {
		return nullSafeValue(this.maxMessageCount, CacheServer.DEFAULT_MAXIMUM_MESSAGE_COUNT);
	}

	/* (non-Javadoc) */
	void setMaxThreads(Integer maxThreads) {
		this.maxThreads = maxThreads;
	}

	protected Integer maxThreads() {
		return nullSafeValue(this.maxThreads, CacheServer.DEFAULT_MAX_THREADS);
	}

	/* (non-Javadoc) */
	void setMaxTimeBetweenPings(Integer maxTimeBetweenPings) {
		this.maxTimeBetweenPings = maxTimeBetweenPings;
	}

	protected Integer maxTimeBetweenPings() {
		return nullSafeValue(this.maxTimeBetweenPings, CacheServer.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS);
	}

	/* (non-Javadoc) */
	void setMessageTimeToLive(Integer messageTimeToLive) {
		this.messageTimeToLive = messageTimeToLive;
	}

	protected Integer messageTimeToLive() {
		return nullSafeValue(this.messageTimeToLive, CacheServer.DEFAULT_MESSAGE_TIME_TO_LIVE);
	}

	/* (non-Javadoc) */
	void setPort(Integer port) {
		this.port = port;
	}

	protected Integer port() {
		return nullSafeValue(this.port, CacheServer.DEFAULT_PORT);
	}

	/* (non-Javadoc) */
	void setServerLoadProbe(ServerLoadProbe serverLoadProbe) {
		this.serverLoadProbe = serverLoadProbe;
	}

	protected ServerLoadProbe serverLoadProbe() {
		return nullSafeValue(this.serverLoadProbe, CacheServer.DEFAULT_LOAD_PROBE);
	}

	/* (non-Javadoc) */
	void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	protected Integer socketBufferSize() {
		return nullSafeValue(this.socketBufferSize, CacheServer.DEFAULT_SOCKET_BUFFER_SIZE);
	}

	/* (non-Javadoc) */
	void setSubscriptionCapacity(Integer subscriptionCapacity) {
		this.subscriptionCapacity = subscriptionCapacity;
	}

	protected Integer subscriptionCapacity() {
		return nullSafeValue(this.subscriptionCapacity, ClientSubscriptionConfig.DEFAULT_CAPACITY);
	}

	/* (non-Javadoc) */
	void setSubscriptionDiskStoreName(String subscriptionDiskStoreName) {
		this.subscriptionDiskStoreName = subscriptionDiskStoreName;
	}

	protected String subscriptionDiskStoreName() {
		return this.subscriptionDiskStoreName;
	}

	/* (non-Javadoc) */
	void setSubscriptionEvictionPolicy(SubscriptionEvictionPolicy subscriptionEvictionPolicy) {
		this.subscriptionEvictionPolicy = subscriptionEvictionPolicy;
	}

	protected SubscriptionEvictionPolicy subscriptionEvictionPolicy() {
		return nullSafeValue(this.subscriptionEvictionPolicy, SubscriptionEvictionPolicy.DEFAULT);
	}

	@Override
	public String toString() {
		return DEFAULT_NAME;
	}
}
