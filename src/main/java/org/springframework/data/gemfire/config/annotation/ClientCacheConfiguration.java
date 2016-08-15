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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.support.ConnectionEndpoint;

/**
 * Spring {@link Configuration} class used to configure, construct and initialize
 * a GemFire {@link com.gemstone.gemfire.cache.client.ClientCache} instance in a Spring application context.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @since 1.0.0
 */
@Configuration
@SuppressWarnings("unused")
public class ClientCacheConfiguration extends AbstractCacheConfiguration {

	protected static final boolean DEFAULT_READY_FOR_EVENTS = false;

	protected static final String DEFAULT_NAME = "SpringBasedCacheClientApplication";

	private boolean readyForEvents = DEFAULT_READY_FOR_EVENTS;

	private Boolean keepAlive;
	private Boolean multiUserAuthentication;
	private Boolean prSingleHopEnabled;
	private Boolean subscriptionEnabled;
	private Boolean threadLocalConnections;

	private Integer durableClientTimeout;
	private Integer freeConnectionTimeout;
	private Integer loadConditioningInterval;
	private Integer maxConnections;
	private Integer minConnections;
	private Integer readTimeout;
	private Integer retryAttempts;
	private Integer socketBufferSize;
	private Integer statisticsInterval;
	private Integer subscriptionAckInterval;
	private Integer subscriptionMessageTrackingTimeout;
	private Integer subscriptionRedundancy;

	private Iterable<ConnectionEndpoint> locators;
	private Iterable<ConnectionEndpoint> servers;

	private Long idleTimeout;
	private Long pingInterval;

	private String durableClientId;
	private String serverGroup;

	@Bean
	public ClientCacheFactoryBean gemfireCache() {
		ClientCacheFactoryBean gemfireCache = constructCacheFactoryBean();

		gemfireCache.setDurableClientId(durableClientId());
		gemfireCache.setDurableClientTimeout(durableClientTimeout());
		gemfireCache.setFreeConnectionTimeout(freeConnectionTimeout());
		gemfireCache.setIdleTimeout(idleTimeout());
		gemfireCache.setKeepAlive(keepAlive());
		gemfireCache.setLocators(poolLocators());
		gemfireCache.setLoadConditioningInterval(loadConditioningInterval());
		gemfireCache.setMaxConnections(maxConnections());
		gemfireCache.setMinConnections(minConnections());
		gemfireCache.setMultiUserAuthentication(multiUserAuthentication());
		gemfireCache.setPingInterval(pingInterval());
		gemfireCache.setPrSingleHopEnabled(prSingleHopEnabled());
		gemfireCache.setReadTimeout(readTimeout());
		gemfireCache.setReadyForEvents(readyForEvents());
		gemfireCache.setRetryAttempts(retryAttempts());
		gemfireCache.setServerGroup(serverGroup());
		gemfireCache.setServers(poolServers());
		gemfireCache.setSocketBufferSize(socketBufferSize());
		gemfireCache.setStatisticsInterval(statisticsInterval());
		gemfireCache.setSubscriptionAckInterval(subscriptionAckInterval());
		gemfireCache.setSubscriptionEnabled(subscriptionEnabled());
		gemfireCache.setSubscriptionMessageTrackingTimeout(subscriptionMessageTrackingTimeout());
		gemfireCache.setSubscriptionRedundancy(subscriptionRedundancy());
		gemfireCache.setThreadLocalConnections(threadLocalConnections());

		return gemfireCache;
	}

	@Override
	@SuppressWarnings("unchecked")
	protected <T extends CacheFactoryBean> T newCacheFactoryBean() {
		return (T) new ClientCacheFactoryBean();
	}

	/**
	 * Configures GemFire {@link com.gemstone.gemfire.cache.client.ClientCache} specific settings.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing client cache meta-data used to configure
	 * the GemFire {@link com.gemstone.gemfire.cache.client.ClientCache}.
	 * @see org.springframework.core.type.AnnotationMetadata
	 */
	@Override
	protected void configureCache(AnnotationMetadata importMetadata) {
		super.configureCache(importMetadata);

		if (isClientCacheApplication(importMetadata)) {
			Map<String, Object> clientCacheApplicationAttributes =
				importMetadata.getAnnotationAttributes(getAnnotationTypeName());

			setDurableClientId((String) clientCacheApplicationAttributes.get("durableClientId"));
			setDurableClientTimeout((Integer) clientCacheApplicationAttributes.get("durableClientTimeout"));
			setFreeConnectionTimeout((Integer) clientCacheApplicationAttributes.get("freeConnectionTimeout"));
			setIdleTimeout((Long) clientCacheApplicationAttributes.get("idleTimeout"));
			setKeepAlive(Boolean.TRUE.equals(clientCacheApplicationAttributes.get("keepAlive")));
			setLoadConditioningInterval((Integer) clientCacheApplicationAttributes.get("loadConditionInterval"));
			setMaxConnections((Integer) clientCacheApplicationAttributes.get("maxConnections"));
			setMinConnections((Integer) clientCacheApplicationAttributes.get("minConnections"));
			setMultiUserAuthentication(Boolean.TRUE.equals(clientCacheApplicationAttributes.get("multiUserAuthentication")));
			setPingInterval((Long) clientCacheApplicationAttributes.get("pingInterval"));
			setPrSingleHopEnabled(Boolean.TRUE.equals(clientCacheApplicationAttributes.get("prSingleHopEnabled")));
			setReadTimeout((Integer) clientCacheApplicationAttributes.get("readTimeout"));
			setReadyForEvents(Boolean.TRUE.equals(clientCacheApplicationAttributes.get("readyForEvents")));
			setRetryAttempts((Integer) clientCacheApplicationAttributes.get("retryAttempts"));
			setServerGroup((String) clientCacheApplicationAttributes.get("serverGroup"));
			setSocketBufferSize((Integer) clientCacheApplicationAttributes.get("socketBufferSize"));
			setStatisticsInterval((Integer) clientCacheApplicationAttributes.get("statisticInterval"));
			setSubscriptionAckInterval((Integer) clientCacheApplicationAttributes.get("subscriptionAckInterval"));
			setSubscriptionEnabled(Boolean.TRUE.equals(clientCacheApplicationAttributes.get("subscriptionEnabled")));
			setSubscriptionMessageTrackingTimeout((Integer) clientCacheApplicationAttributes.get("subscriptionMessageTrackingTimeout"));
			setSubscriptionRedundancy((Integer) clientCacheApplicationAttributes.get("subscriptionRedundancy"));
			setThreadLocalConnections(Boolean.TRUE.equals(clientCacheApplicationAttributes.get("threadLocalConnections")));

			configureLocatorOrServer(clientCacheApplicationAttributes);
		}
	}

	/* (non-Javadoc) */
	void configureLocatorOrServer(Map<String, Object> clientCacheApplicationAttributes) {
		AnnotationAttributes[] locators =
			(AnnotationAttributes[]) clientCacheApplicationAttributes.get("locators");

		List<ConnectionEndpoint> poolLocators = new ArrayList<ConnectionEndpoint>(locators.length);

		for (AnnotationAttributes locator : locators) {
			poolLocators.add(new ConnectionEndpoint((String) locator.get("host"), (Integer) locator.get("port")));
		}

		setPoolLocators(poolLocators);

		AnnotationAttributes[] servers =
			(AnnotationAttributes[]) clientCacheApplicationAttributes.get("servers");

		List<ConnectionEndpoint> poolServers = new ArrayList<ConnectionEndpoint>(servers.length);

		for (AnnotationAttributes server : servers) {
			poolServers.add(new ConnectionEndpoint((String) server.get("host"), (Integer) server.get("port")));
		}

		setPoolServers(poolServers);
	}

	@Override
	protected Class getAnnotationType() {
		return ClientCacheApplication.class;
	}

	/* (non-Javadoc) */
	void setDurableClientId(String durableClientId) {
		this.durableClientId = durableClientId;
	}

	protected String durableClientId() {
		return this.durableClientId;
	}

	/* (non-Javadoc) */
	void setDurableClientTimeout(Integer durableClientTimeout) {
		this.durableClientTimeout = durableClientTimeout;
	}

	protected Integer durableClientTimeout() {
		return this.durableClientTimeout;
	}

	/* (non-Javadoc) */
	void setFreeConnectionTimeout(Integer freeConnectionTimeout) {
		this.freeConnectionTimeout = freeConnectionTimeout;
	}

	protected Integer freeConnectionTimeout() {
		return this.freeConnectionTimeout;
	}

	/* (non-Javadoc) */
	void setIdleTimeout(Long idleTimeout) {
		this.idleTimeout = idleTimeout;
	}

	protected Long idleTimeout() {
		return this.idleTimeout;
	}

	/* (non-Javadoc) */
	void setKeepAlive(Boolean keepAlive) {
		this.keepAlive = keepAlive;
	}

	protected Boolean keepAlive() {
		return this.keepAlive;
	}

	/* (non-Javadoc) */
	void setLoadConditioningInterval(Integer loadConditioningInterval) {
		this.loadConditioningInterval = loadConditioningInterval;
	}

	protected Integer loadConditioningInterval() {
		return this.loadConditioningInterval;
	}

	/* (non-Javadoc) */
	void setMaxConnections(Integer maxConnections) {
		this.maxConnections = maxConnections;
	}

	protected Integer maxConnections() {
		return this.maxConnections;
	}

	/* (non-Javadoc) */
	void setMinConnections(Integer minConnections) {
		this.minConnections = minConnections;
	}

	protected Integer minConnections() {
		return this.minConnections;
	}

	/* (non-Javadoc) */
	void setMultiUserAuthentication(Boolean multiUserAuthentication) {
		this.multiUserAuthentication = multiUserAuthentication;
	}

	protected Boolean multiUserAuthentication() {
		return this.multiUserAuthentication;
	}

	/* (non-Javadoc) */
	void setPingInterval(Long pingInterval) {
		this.pingInterval = pingInterval;
	}

	protected Long pingInterval() {
		return this.pingInterval;
	}

	/* (non-Javadoc) */
	void setPoolLocators(Iterable<ConnectionEndpoint> locators) {
		this.locators = locators;
	}

	protected Iterable<ConnectionEndpoint> poolLocators() {
		return this.locators;
	}

	/* (non-Javadoc) */
	void setPoolServers(Iterable<ConnectionEndpoint> servers) {
		this.servers = servers;
	}

	protected Iterable<ConnectionEndpoint> poolServers() {
		return this.servers;
	}

	/* (non-Javadoc) */
	void setPrSingleHopEnabled(Boolean prSingleHopEnabled) {
		this.prSingleHopEnabled = prSingleHopEnabled;
	}

	protected Boolean prSingleHopEnabled() {
		return this.prSingleHopEnabled;
	}

	/* (non-Javadoc) */
	void setReadTimeout(Integer readTimeout) {
		this.readTimeout = readTimeout;
	}

	protected Integer readTimeout() {
		return this.readTimeout;
	}

	/* (non-Javadoc) */
	void setReadyForEvents(boolean readyForEvents) {
		this.readyForEvents = readyForEvents;
	}

	protected boolean readyForEvents() {
		return this.readyForEvents;
	}

	/* (non-Javadoc) */
	void setRetryAttempts(Integer retryAttempts) {
		this.retryAttempts = retryAttempts;
	}

	protected Integer retryAttempts() {
		return this.retryAttempts;
	}

	/* (non-Javadoc) */
	void setServerGroup(String serverGroup) {
		this.serverGroup = serverGroup;
	}

	protected String serverGroup() {
		return this.serverGroup;
	}

	/* (non-Javadoc) */
	void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	protected Integer socketBufferSize() {
		return this.socketBufferSize;
	}

	/* (non-Javadoc) */
	void setStatisticsInterval(Integer statisticsInterval) {
		this.statisticsInterval = statisticsInterval;
	}

	protected Integer statisticsInterval() {
		return this.statisticsInterval;
	}

	/* (non-Javadoc) */
	void setSubscriptionAckInterval(Integer subscriptionAckInterval) {
		this.subscriptionAckInterval = subscriptionAckInterval;
	}

	protected Integer subscriptionAckInterval() {
		return this.subscriptionAckInterval;
	}

	/* (non-Javadoc) */
	void setSubscriptionEnabled(Boolean subscriptionEnabled) {
		this.subscriptionEnabled = subscriptionEnabled;
	}

	protected Boolean subscriptionEnabled() {
		return this.subscriptionEnabled;
	}

	/* (non-Javadoc) */
	void setSubscriptionMessageTrackingTimeout(Integer subscriptionMessageTrackingTimeout) {
		this.subscriptionMessageTrackingTimeout = subscriptionMessageTrackingTimeout;
	}

	protected Integer subscriptionMessageTrackingTimeout() {
		return this.subscriptionMessageTrackingTimeout;
	}

	/* (non-Javadoc) */
	void setSubscriptionRedundancy(Integer subscriptionRedundancy) {
		this.subscriptionRedundancy = subscriptionRedundancy;
	}

	protected Integer subscriptionRedundancy() {
		return this.subscriptionRedundancy;
	}

	/* (non-Javadoc) */
	void setThreadLocalConnections(Boolean threadLocalConnections) {
		this.threadLocalConnections = threadLocalConnections;
	}

	protected Boolean threadLocalConnections() {
		return this.threadLocalConnections;
	}

	@Override
	public String toString() {
		return DEFAULT_NAME;
	}
}
