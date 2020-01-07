/*
 * Copyright 2012-2020 the original author or authors.
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
package org.springframework.data.gemfire.config.annotation;

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.lang.annotation.Annotation;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.util.StringUtils;

/**
 * The {@link AddPoolConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that registers
 * a {@link PoolFactoryBean} definition for the {@link org.apache.geode.cache.client.Pool}
 * configuration meta-data defined in {@link EnablePool} annotations.
 *
 * @author John Blum
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.AddPoolsConfiguration
 * @see org.springframework.data.gemfire.config.annotation.PoolConfigurer
 * @see org.springframework.data.gemfire.config.annotation.EnablePools
 * @see org.springframework.data.gemfire.config.annotation.EnablePool
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @since 1.9.0
 */
public class AddPoolConfiguration extends AbstractAnnotationConfigSupport
		implements ImportBeanDefinitionRegistrar {

	@Autowired(required = false)
	private List<PoolConfigurer> poolConfigurers = Collections.emptyList();

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnablePool.class;
	}

	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

		if (isAnnotationPresent(importingClassMetadata)) {

			AnnotationAttributes enablePoolAttributes = getAnnotationAttributes(importingClassMetadata);

			registerPoolFactoryBeanDefinition(enablePoolAttributes, registry);
		}
	}

	/**
	 * Registers a {@link PoolFactoryBean} definition in the Spring application context configured with
	 * the {@link EnablePool} annotation meta-data.
	 *
	 * @param enablePoolAttributes {@link EnablePool} annotation attributes.
	 * @param registry Spring {@link BeanDefinitionRegistry} used to register the {@link PoolFactoryBean} definition.
	 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
	 * @see org.springframework.data.gemfire.client.PoolFactoryBean
	 * @see org.springframework.data.gemfire.config.annotation.EnablePool
	 * @see java.util.Map
	 */
	protected void registerPoolFactoryBeanDefinition(AnnotationAttributes enablePoolAttributes,
			BeanDefinitionRegistry registry) {

		String poolName = getAndValidatePoolName(enablePoolAttributes);

		BeanDefinitionBuilder poolFactoryBean = BeanDefinitionBuilder.genericBeanDefinition(PoolFactoryBean.class);

		poolFactoryBean.addPropertyValue("freeConnectionTimeout",
			resolveProperty(namedPoolProperty(poolName, "free-connection-timeout"),
				resolveProperty(poolProperty("free-connection-timeout"),
					enablePoolAttributes.<Integer>getNumber("freeConnectionTimeout"))));

		poolFactoryBean.addPropertyValue("idleTimeout",
			resolveProperty(namedPoolProperty(poolName, "idle-timeout"),
				resolveProperty(poolProperty("idle-timeout"),
					enablePoolAttributes.<Long>getNumber("idleTimeout"))));

		poolFactoryBean.addPropertyValue("loadConditioningInterval",
			resolveProperty(namedPoolProperty(poolName, "load-conditioning-interval"),
				resolveProperty(poolProperty("load-conditioning-interval"),
					enablePoolAttributes.<Integer>getNumber("loadConditioningInterval"))));

		poolFactoryBean.addPropertyValue("maxConnections",
			resolveProperty(namedPoolProperty(poolName, "max-connections"),
				resolveProperty(poolProperty("max-connections"),
					enablePoolAttributes.<Integer>getNumber("maxConnections"))));

		poolFactoryBean.addPropertyValue("minConnections",
			resolveProperty(namedPoolProperty(poolName, "min-connections"),
				resolveProperty(poolProperty("min-connections"),
					enablePoolAttributes.<Integer>getNumber("minConnections"))));

		poolFactoryBean.addPropertyValue("multiUserAuthentication",
			resolveProperty(namedPoolProperty(poolName, "multi-user-authentication"),
				resolveProperty(poolProperty("multi-user-authentication"),
					enablePoolAttributes.getBoolean("multiUserAuthentication"))));

		poolFactoryBean.addPropertyValue("pingInterval",
			resolveProperty(namedPoolProperty(poolName, "ping-interval"),
				resolveProperty(poolProperty("ping-interval"),
					enablePoolAttributes.<Long>getNumber("pingInterval"))));

		poolFactoryBean.addPropertyValue("poolConfigurers", resolvePoolConfigurers());

		poolFactoryBean.addPropertyValue("prSingleHopEnabled",
			resolveProperty(namedPoolProperty(poolName, "pr-single-hop-enabled"),
				resolveProperty(poolProperty("pr-single-hop-enabled"),
					enablePoolAttributes.getBoolean("prSingleHopEnabled"))));

		poolFactoryBean.addPropertyValue("readTimeout",
			resolveProperty(namedPoolProperty(poolName, "read-timeout"),
				resolveProperty(poolProperty("read-timeout"),
					enablePoolAttributes.<Integer>getNumber("readTimeout"))));

		poolFactoryBean.addPropertyValue("retryAttempts",
			resolveProperty(namedPoolProperty(poolName, "retry-attempts"),
				resolveProperty(poolProperty("retry-attempts"),
					enablePoolAttributes.<Integer>getNumber("retryAttempts"))));

		poolFactoryBean.addPropertyValue("serverGroup",
			resolveProperty(namedPoolProperty(poolName, "server-group"),
				resolveProperty(poolProperty("server-group"),
					enablePoolAttributes.getString("serverGroup"))));

		poolFactoryBean.addPropertyValue("socketBufferSize",
			resolveProperty(namedPoolProperty(poolName, "socket-buffer-size"),
				resolveProperty(poolProperty("socket-buffer-size"),
					enablePoolAttributes.<Integer>getNumber("socketBufferSize"))));

		poolFactoryBean.addPropertyValue("statisticInterval",
			resolveProperty(namedPoolProperty(poolName, "statistic-interval"),
				resolveProperty(poolProperty("statistic-interval"),
					enablePoolAttributes.<Integer>getNumber("statisticInterval"))));

		poolFactoryBean.addPropertyValue("subscriptionAckInterval",
			resolveProperty(namedPoolProperty(poolName, "subscription-ack-interval"),
				resolveProperty(poolProperty("subscription-ack-interval"),
					enablePoolAttributes.<Integer>getNumber("subscriptionAckInterval"))));

		poolFactoryBean.addPropertyValue("subscriptionEnabled",
			resolveProperty(namedPoolProperty(poolName, "subscription-enabled"),
				resolveProperty(poolProperty("subscription-enabled"),
					enablePoolAttributes.getBoolean("subscriptionEnabled"))));

		poolFactoryBean.addPropertyValue("subscriptionMessageTrackingTimeout",
			resolveProperty(namedPoolProperty(poolName, "subscription-message-tracking-timeout"),
				resolveProperty(poolProperty("subscription-message-tracking-timeout"),
					enablePoolAttributes.<Integer>getNumber("subscriptionMessageTrackingTimeout"))));

		poolFactoryBean.addPropertyValue("subscriptionRedundancy",
			resolveProperty(namedPoolProperty(poolName, "subscription-redundancy"),
				resolveProperty(poolProperty("subscription-redundancy"),
					enablePoolAttributes.<Integer>getNumber("subscriptionRedundancy"))));

		poolFactoryBean.addPropertyValue("threadLocalConnections",
			resolveProperty(namedPoolProperty(poolName, "thread-local-connections"),
				resolveProperty(poolProperty("thread-local-connections"),
					enablePoolAttributes.getBoolean("threadLocalConnections"))));

		configurePoolConnections(poolName, enablePoolAttributes, poolFactoryBean);

		registry.registerBeanDefinition(poolName, poolFactoryBean.getBeanDefinition());
	}

	private List<PoolConfigurer> resolvePoolConfigurers() {

		return Optional.ofNullable(this.poolConfigurers)
			.filter(poolConfigurers -> !poolConfigurers.isEmpty())
			.orElseGet(() -> Collections.singletonList(LazyResolvingComposablePoolConfigurer.create(getBeanFactory())));
	}

	protected String getAndValidatePoolName(Map<String, Object> enablePoolAttributes) {

		return Optional.ofNullable((String) enablePoolAttributes.get("name"))
			.filter(StringUtils::hasText)
			.orElseThrow(() -> newIllegalArgumentException("Pool name is required"));
	}

	/**
	 * Uses the list of Pivotal GemFire Locator and Server connection endpoint definitions and meta-data to configure
	 * the Pivotal GemFire client {@link org.apache.geode.cache.client.Pool} used to communicate with the servers
	 * in the Pivotal GemFire cluster.
	 *
	 * @param enablePoolAttributes {@link EnablePool} annotation containing
	 * {@link org.apache.geode.cache.client.Pool} Locator/Server connection endpoint meta-data.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
	 * @see java.util.Map
	 */
	protected BeanDefinitionBuilder configurePoolConnections(String poolName, AnnotationAttributes enablePoolAttributes,
			BeanDefinitionBuilder poolFactoryBean) {

		configurePoolLocators(poolName, enablePoolAttributes, poolFactoryBean);
		configurePoolServers(poolName, enablePoolAttributes, poolFactoryBean);

		return poolFactoryBean;
	}

	protected BeanDefinitionBuilder configurePoolLocators(String poolName, AnnotationAttributes enablePoolAttributes,
			BeanDefinitionBuilder poolFactoryBean) {

		String locatorsFromProperty = resolveProperty(namedPoolProperty(poolName, "locators"),
			resolveProperty(poolProperty("locators"), (String) null));

		ConnectionEndpointList locators = Optional.ofNullable(locatorsFromProperty)
			.filter(StringUtils::hasText)
			.map(it -> ConnectionEndpointList.parse(GemfireUtils.DEFAULT_LOCATOR_PORT, it.split(",")))
			.orElseGet(() -> parseConnectionEndpoints(enablePoolAttributes,"locators",
				"locatorsString", GemfireUtils.DEFAULT_LOCATOR_PORT));

		poolFactoryBean.addPropertyValue("locators", locators);

		return poolFactoryBean;
	}

	protected BeanDefinitionBuilder configurePoolServers(String poolName, AnnotationAttributes enablePoolAttributes,
			BeanDefinitionBuilder poolFactoryBean) {

		String serversFromProperty = resolveProperty(namedPoolProperty(poolName, "servers"),
			resolveProperty("servers", (String) null));

		ConnectionEndpointList servers = Optional.ofNullable(serversFromProperty)
			.filter(StringUtils::hasText)
			.map(it -> ConnectionEndpointList.parse(GemfireUtils.DEFAULT_CACHE_SERVER_PORT, it.split(",")))
			.orElseGet(() -> parseConnectionEndpoints(enablePoolAttributes, "servers",
				"serversString", GemfireUtils.DEFAULT_CACHE_SERVER_PORT));

		poolFactoryBean.addPropertyValue("servers", servers);

		return poolFactoryBean;
	}

	protected ConnectionEndpointList parseConnectionEndpoints(AnnotationAttributes enablePoolAttributes,
			String arrayAttributeName, String stringAttributeName, int defaultPort) {

		AnnotationAttributes[] connectionEndpointsMetaData =
			enablePoolAttributes.getAnnotationArray(arrayAttributeName);

		ConnectionEndpointList connectionEndpoints = new ConnectionEndpointList();

		stream(nullSafeArray(connectionEndpointsMetaData, AnnotationAttributes.class))
			.forEach(annotationAttributes ->
				connectionEndpoints.add(newConnectionEndpoint((String) annotationAttributes.get("host"),
					(Integer) annotationAttributes.get("port"))));

		Optional.ofNullable(enablePoolAttributes.getString(stringAttributeName))
			.filter(StringUtils::hasText)
			.ifPresent(hostsPorts ->
				connectionEndpoints.add(ConnectionEndpointList.parse(defaultPort, hostsPorts.split(","))));

		return connectionEndpoints;
	}

	protected ConnectionEndpoint newConnectionEndpoint(String host, Integer port) {
		return new ConnectionEndpoint(host, port);
	}
}
