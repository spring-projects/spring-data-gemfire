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

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeMap;

import java.lang.annotation.Annotation;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.server.CacheServer;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.config.support.ClientRegionPoolBeanFactoryPostProcessor;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.util.StringUtils;

/**
 * Spring {@link Configuration} class used to construct, configure and initialize
 * a {@link org.apache.geode.cache.client.ClientCache} instance in a Spring application context.
 *
 * @author John Blum
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
 * @see org.springframework.data.gemfire.config.support.ClientRegionPoolBeanFactoryPostProcessor
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @see org.springframework.data.gemfire.support.ConnectionEndpointList
 * @since 1.0.0
 */
@Configuration
@SuppressWarnings("unused")
public class ClientCacheConfiguration extends AbstractCacheConfiguration {

	private static final AtomicBoolean CLIENT_REGION_POOL_BEAN_FACTORY_POST_PROCESSOR_REGISTERED =
		new AtomicBoolean(false);

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

	@Autowired(required = false)
	private List<ClientCacheConfigurer> clientCacheConfigurers = Collections.emptyList();

	private Long idleTimeout;
	private Long pingInterval;

	private String durableClientId;
	private String serverGroup;

	/**
	 * Bean declaration for a single, peer {@link ClientCache} instance.
	 *
	 * @return a new instance of a peer {@link ClientCache}.
	 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
	 * @see org.apache.geode.cache.client.ClientCache
	 * @see org.apache.geode.cache.GemFireCache
	 * @see #constructCacheFactoryBean()
	 */
	@Bean
	public ClientCacheFactoryBean gemfireCache() {

		ClientCacheFactoryBean gemfireCache = constructCacheFactoryBean();

		gemfireCache.setClientCacheConfigurers(resolveClientCacheConfigurers());
		gemfireCache.setDurableClientId(getDurableClientId());
		gemfireCache.setDurableClientTimeout(getDurableClientTimeout());
		gemfireCache.setFreeConnectionTimeout(getFreeConnectionTimeout());
		gemfireCache.setIdleTimeout(getIdleTimeout());
		gemfireCache.setKeepAlive(getKeepAlive());
		gemfireCache.setLocators(getPoolLocators());
		gemfireCache.setLoadConditioningInterval(getLoadConditioningInterval());
		gemfireCache.setMaxConnections(getMaxConnections());
		gemfireCache.setMinConnections(getMinConnections());
		gemfireCache.setMultiUserAuthentication(getMultiUserAuthentication());
		gemfireCache.setPingInterval(getPingInterval());
		gemfireCache.setPrSingleHopEnabled(getPrSingleHopEnabled());
		gemfireCache.setReadTimeout(getReadTimeout());
		gemfireCache.setReadyForEvents(getReadyForEvents());
		gemfireCache.setRetryAttempts(getRetryAttempts());
		gemfireCache.setServerGroup(getServerGroup());
		gemfireCache.setServers(getPoolServers());
		gemfireCache.setSocketBufferSize(getSocketBufferSize());
		gemfireCache.setStatisticsInterval(getStatisticsInterval());
		gemfireCache.setSubscriptionAckInterval(getSubscriptionAckInterval());
		gemfireCache.setSubscriptionEnabled(getSubscriptionEnabled());
		gemfireCache.setSubscriptionMessageTrackingTimeout(getSubscriptionMessageTrackingTimeout());
		gemfireCache.setSubscriptionRedundancy(getSubscriptionRedundancy());
		gemfireCache.setThreadLocalConnections(getThreadLocalConnections());

		return gemfireCache;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("all")
	private List<ClientCacheConfigurer> resolveClientCacheConfigurers() {

		return Optional.ofNullable(this.clientCacheConfigurers)
			.filter(clientCacheConfigurers -> !clientCacheConfigurers.isEmpty())
			.orElseGet(() ->

				Optional.of(this.getBeanFactory())
					.filter(beanFactory -> beanFactory instanceof ListableBeanFactory)
					.map(beanFactory -> {

						Map<String, ClientCacheConfigurer> beansOfType = ((ListableBeanFactory) beanFactory)
							.getBeansOfType(ClientCacheConfigurer.class, true, true);

						return nullSafeMap(beansOfType).values().stream().collect(Collectors.toList());

					})
					.orElseGet(Collections::emptyList)
			);
	}

	/**
	 * Constructs a new instance of {@link ClientCacheFactoryBean} used to create a peer {@link ClientCache}.
	 *
	 * @param <T> {@link Class} sub-type of {@link CacheFactoryBean}.
	 * @return a new instance of {@link ClientCacheFactoryBean}.
	 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
	 */
	@Override
	@SuppressWarnings("unchecked")
	protected <T extends CacheFactoryBean> T newCacheFactoryBean() {
		return (T) new ClientCacheFactoryBean();
	}

	/**
	 * Configures Spring container infrastructure components and beans used by Spring Data GemFire
	 * to enable Pivotal GemFire or Apache Geode to function properly inside a Spring context.
	 *
	 * This overridden method configures and registers additional Spring components and bean applicable to
	 * {@link ClientCache ClientCaches}.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing annotation meta-data
	 * for the Spring Data GemFire cache application class.
	 * @see org.springframework.core.type.AnnotationMetadata
	 */
	@Override
	protected void configureInfrastructure(AnnotationMetadata importMetadata) {

		super.configureInfrastructure(importMetadata);

		registerClientRegionPoolBeanFactoryPostProcessor(importMetadata);
	}

	/* (non-Javadoc) */
	private void registerClientRegionPoolBeanFactoryPostProcessor(AnnotationMetadata importMetadata) {

		if (CLIENT_REGION_POOL_BEAN_FACTORY_POST_PROCESSOR_REGISTERED.compareAndSet(false, true)) {
			register(BeanDefinitionBuilder.rootBeanDefinition(ClientRegionPoolBeanFactoryPostProcessor.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE).getBeanDefinition());
		}
	}

	/**
	 * Configures {@link ClientCache} specific settings.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing client cache meta-data used to
	 * configure the {@link ClientCache}.
	 * @see org.springframework.core.type.AnnotationMetadata
	 * @see #configureLocatorsAndServers(Map)
	 */
	@Override
	protected void configureCache(AnnotationMetadata importMetadata) {

		super.configureCache(importMetadata);

		if (isClientCacheApplication(importMetadata)) {

			AnnotationAttributes clientCacheApplicationAttributes = getAnnotationAttributes(importMetadata);

			setDurableClientId(resolveProperty(cacheClientProperty("durable-client-id"),
				(String) clientCacheApplicationAttributes.get("durableClientId")));

			setDurableClientTimeout(resolveProperty(cacheClientProperty("durable-client-timeout"),
				(Integer) clientCacheApplicationAttributes.get("durableClientTimeout")));

			setFreeConnectionTimeout(
				resolveProperty(namedPoolProperty("default", "free-connection-timeout"),
				resolveProperty(poolProperty("free-connection-timeout"),
				(Integer) clientCacheApplicationAttributes.get("freeConnectionTimeout"))));

			setIdleTimeout(
				resolveProperty(namedPoolProperty("default", "idle-timeout"),
				resolveProperty(poolProperty("idle-timeout"),
				(Long) clientCacheApplicationAttributes.get("idleTimeout"))));

			setKeepAlive(resolveProperty(cacheClientProperty("keep-alive"),
				Boolean.TRUE.equals(clientCacheApplicationAttributes.get("keepAlive"))));

			setLoadConditioningInterval(
				resolveProperty(namedPoolProperty("default","load-conditioning-interval"),
				resolveProperty(poolProperty("load-conditioning-interval"),
				(Integer) clientCacheApplicationAttributes.get("loadConditioningInterval"))));

			setMaxConnections(
				resolveProperty(namedPoolProperty("default", "max-connections"),
				resolveProperty(poolProperty("max-connections"),
				(Integer) clientCacheApplicationAttributes.get("maxConnections"))));

			setMinConnections(
				resolveProperty(namedPoolProperty("default", "min-connections"),
				resolveProperty(poolProperty("min-connections"),
				(Integer) clientCacheApplicationAttributes.get("minConnections"))));

			setMultiUserAuthentication(
				resolveProperty(namedPoolProperty("default", "multi-user-authentication"),
				resolveProperty(poolProperty("multi-user-authentication"),
				Boolean.TRUE.equals(clientCacheApplicationAttributes.get("multiUserAuthentication")))));

			setPingInterval(
				resolveProperty(namedPoolProperty("default", "ping-interval"),
				resolveProperty(poolProperty("ping-interval"),
				(Long) clientCacheApplicationAttributes.get("pingInterval"))));

			setPrSingleHopEnabled(
				resolveProperty(namedPoolProperty("default", "pr-single-hop-enabled"),
				resolveProperty(poolProperty("pr-single-hop-enabled"),
				Boolean.TRUE.equals(clientCacheApplicationAttributes.get("prSingleHopEnabled")))));

			setReadTimeout(
				resolveProperty(namedPoolProperty("default", "read-timeout"),
				resolveProperty(poolProperty("read-timeout"),
				(Integer) clientCacheApplicationAttributes.get("readTimeout"))));

			setReadyForEvents(
				resolveProperty(namedPoolProperty("default", "ready-for-events"),
				resolveProperty(poolProperty("ready-for-events"),
				Boolean.TRUE.equals(clientCacheApplicationAttributes.get("readyForEvents")))));

			setRetryAttempts(
				resolveProperty(namedPoolProperty("default", "retry-attempts"),
				resolveProperty(poolProperty("retry-attempts"),
				(Integer) clientCacheApplicationAttributes.get("retryAttempts"))));

			setServerGroup(
				resolveProperty(namedPoolProperty("default", "server-group"),
				resolveProperty(poolProperty("server-group"),
				(String) clientCacheApplicationAttributes.get("serverGroup"))));

			setSocketBufferSize(
				resolveProperty(namedPoolProperty("default", "socket-buffer-size"),
				resolveProperty(poolProperty("socket-buffer-size"),
				(Integer) clientCacheApplicationAttributes.get("socketBufferSize"))));

			setStatisticsInterval(
				resolveProperty(namedPoolProperty("default", "statistic-interval"),
				resolveProperty(poolProperty("statistic-interval"),
				(Integer) clientCacheApplicationAttributes.get("statisticInterval"))));

			setSubscriptionAckInterval(
				resolveProperty(namedPoolProperty("default", "subscription-ack-interval"),
				resolveProperty(poolProperty("subscription-ack-interval"),
				(Integer) clientCacheApplicationAttributes.get("subscriptionAckInterval"))));

			setSubscriptionEnabled(
				resolveProperty(namedPoolProperty("default", "subscription-enabled"),
				resolveProperty(poolProperty("subscription-enabled"),
				Boolean.TRUE.equals(clientCacheApplicationAttributes.get("subscriptionEnabled")))));

			setSubscriptionMessageTrackingTimeout(
				resolveProperty(namedPoolProperty("default", "subscription-message-tracking-timeout"),
				resolveProperty(poolProperty("subscription-message-tracking-timeout"),
				(Integer) clientCacheApplicationAttributes.get("subscriptionMessageTrackingTimeout"))));

			setSubscriptionRedundancy(
				resolveProperty(namedPoolProperty("default", "subscription-redundancy"),
				resolveProperty(poolProperty("subscription-redundancy"),
				(Integer) clientCacheApplicationAttributes.get("subscriptionRedundancy"))));

			setThreadLocalConnections(
				resolveProperty(namedPoolProperty("default", "thread-local-connections"),
				resolveProperty(poolProperty("thread-local-connections"),
				Boolean.TRUE.equals(clientCacheApplicationAttributes.get("threadLocalConnections")))));

			configureLocatorsAndServers(clientCacheApplicationAttributes);
		}
	}

	/**
	 * Uses the list of Pivotal GemFire/Apache Geode Locator and Server connection endpoint definitions and meta-data
	 * to configure the client {@link Pool} used to communicate with the servers in the cluster.
	 *
	 * @param clientCacheApplicationAttributes {@link ClientCacheApplication} annotation containing {@link Pool}
	 * Locator/Server connection endpoint meta-data.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
	 * @see java.util.Map
	 */
	private void configureLocatorsAndServers(Map<String, Object> clientCacheApplicationAttributes) {

		ConnectionEndpointList poolLocators;

		String locatorsFromProperty = resolveProperty(namedPoolProperty("default", "locators"),
			resolveProperty(poolProperty("locators"), (String) null));

		if (StringUtils.hasText(locatorsFromProperty)) {

			String[] locatorHostsPorts = locatorsFromProperty.split(",");

			poolLocators = ConnectionEndpointList.parse(GemfireUtils.DEFAULT_LOCATOR_PORT, locatorHostsPorts);
		}
		else {

			poolLocators = new ConnectionEndpointList();

			AnnotationAttributes[] locators = (AnnotationAttributes[]) clientCacheApplicationAttributes.get("locators");

			for (AnnotationAttributes locator : locators) {
				poolLocators.add(newConnectionEndpoint((String) locator.get("host"), (Integer) locator.get("port")));
			}
		}

		setPoolLocators(poolLocators);

		ConnectionEndpointList poolServers;

		String serversFromProperty = resolveProperty(namedPoolProperty("default", "servers"),
			resolveProperty(poolProperty("servers"), (String) null));

		if (StringUtils.hasText(serversFromProperty)) {
			String[] serverHostsPorts = serversFromProperty.split(",");
			poolServers = ConnectionEndpointList.parse(CacheServer.DEFAULT_PORT, serverHostsPorts);
		}
		else {
			poolServers = new ConnectionEndpointList();

			AnnotationAttributes[] servers = (AnnotationAttributes[]) clientCacheApplicationAttributes.get("servers");

			for (AnnotationAttributes server : servers) {
				poolServers.add(newConnectionEndpoint((String) server.get("host"), (Integer) server.get("port")));
			}
		}

		setPoolServers(poolServers);
	}

	protected ConnectionEndpoint newConnectionEndpoint(String host, Integer port) {
		return new ConnectionEndpoint(host, port);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return ClientCacheApplication.class;
	}

	void setDurableClientId(String durableClientId) {
		this.durableClientId = durableClientId;
	}

	protected String getDurableClientId() {
		return this.durableClientId;
	}

	void setDurableClientTimeout(Integer durableClientTimeout) {
		this.durableClientTimeout = durableClientTimeout;
	}

	protected Integer getDurableClientTimeout() {
		return this.durableClientTimeout;
	}

	void setFreeConnectionTimeout(Integer freeConnectionTimeout) {
		this.freeConnectionTimeout = freeConnectionTimeout;
	}

	protected Integer getFreeConnectionTimeout() {
		return this.freeConnectionTimeout;
	}

	void setIdleTimeout(Long idleTimeout) {
		this.idleTimeout = idleTimeout;
	}

	protected Long getIdleTimeout() {
		return this.idleTimeout;
	}

	void setKeepAlive(Boolean keepAlive) {
		this.keepAlive = keepAlive;
	}

	protected Boolean getKeepAlive() {
		return this.keepAlive;
	}

	void setLoadConditioningInterval(Integer loadConditioningInterval) {
		this.loadConditioningInterval = loadConditioningInterval;
	}

	protected Integer getLoadConditioningInterval() {
		return this.loadConditioningInterval;
	}

	void setMaxConnections(Integer maxConnections) {
		this.maxConnections = maxConnections;
	}

	protected Integer getMaxConnections() {
		return this.maxConnections;
	}

	void setMinConnections(Integer minConnections) {
		this.minConnections = minConnections;
	}

	protected Integer getMinConnections() {
		return this.minConnections;
	}

	void setMultiUserAuthentication(Boolean multiUserAuthentication) {
		this.multiUserAuthentication = multiUserAuthentication;
	}

	protected Boolean getMultiUserAuthentication() {
		return this.multiUserAuthentication;
	}

	void setPingInterval(Long pingInterval) {
		this.pingInterval = pingInterval;
	}

	protected Long getPingInterval() {
		return this.pingInterval;
	}

	void setPoolLocators(Iterable<ConnectionEndpoint> locators) {
		this.locators = locators;
	}

	protected Iterable<ConnectionEndpoint> getPoolLocators() {
		return this.locators;
	}

	void setPoolServers(Iterable<ConnectionEndpoint> servers) {
		this.servers = servers;
	}

	protected Iterable<ConnectionEndpoint> getPoolServers() {
		return this.servers;
	}

	void setPrSingleHopEnabled(Boolean prSingleHopEnabled) {
		this.prSingleHopEnabled = prSingleHopEnabled;
	}

	protected Boolean getPrSingleHopEnabled() {
		return this.prSingleHopEnabled;
	}

	void setReadTimeout(Integer readTimeout) {
		this.readTimeout = readTimeout;
	}

	protected Integer getReadTimeout() {
		return this.readTimeout;
	}

	void setReadyForEvents(boolean readyForEvents) {
		this.readyForEvents = readyForEvents;
	}

	protected boolean getReadyForEvents() {
		return this.readyForEvents;
	}

	void setRetryAttempts(Integer retryAttempts) {
		this.retryAttempts = retryAttempts;
	}

	protected Integer getRetryAttempts() {
		return this.retryAttempts;
	}

	void setServerGroup(String serverGroup) {
		this.serverGroup = serverGroup;
	}

	protected String getServerGroup() {
		return this.serverGroup;
	}

	void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	protected Integer getSocketBufferSize() {
		return this.socketBufferSize;
	}

	void setStatisticsInterval(Integer statisticsInterval) {
		this.statisticsInterval = statisticsInterval;
	}

	protected Integer getStatisticsInterval() {
		return this.statisticsInterval;
	}

	void setSubscriptionAckInterval(Integer subscriptionAckInterval) {
		this.subscriptionAckInterval = subscriptionAckInterval;
	}

	protected Integer getSubscriptionAckInterval() {
		return this.subscriptionAckInterval;
	}

	void setSubscriptionEnabled(Boolean subscriptionEnabled) {
		this.subscriptionEnabled = subscriptionEnabled;
	}

	protected Boolean getSubscriptionEnabled() {
		return this.subscriptionEnabled;
	}

	void setSubscriptionMessageTrackingTimeout(Integer subscriptionMessageTrackingTimeout) {
		this.subscriptionMessageTrackingTimeout = subscriptionMessageTrackingTimeout;
	}

	protected Integer getSubscriptionMessageTrackingTimeout() {
		return this.subscriptionMessageTrackingTimeout;
	}

	void setSubscriptionRedundancy(Integer subscriptionRedundancy) {
		this.subscriptionRedundancy = subscriptionRedundancy;
	}

	protected Integer getSubscriptionRedundancy() {
		return this.subscriptionRedundancy;
	}

	void setThreadLocalConnections(Boolean threadLocalConnections) {
		this.threadLocalConnections = threadLocalConnections;
	}

	protected Boolean getThreadLocalConnections() {
		return this.threadLocalConnections;
	}

	/**
	 * Returns a {@link String} containing the name of the Spring-configured Apache Geode
	 * {@link ClientCache} application.
	 *
	 * @return a {@link String} containing the name of the Spring-configured Apache Geode
	 * {@link ClientCache} application.
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return DEFAULT_NAME;
	}
}
