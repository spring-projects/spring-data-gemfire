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

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeMap;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.ListableBeanFactory;
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

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

		if (importingClassMetadata.hasAnnotation(EnablePool.class.getName())) {

			Map<String, Object> enablePoolAttributes =
				importingClassMetadata.getAnnotationAttributes(EnablePool.class.getName());

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
	protected void registerPoolFactoryBeanDefinition(Map<String, Object> enablePoolAttributes,
			BeanDefinitionRegistry registry) {

		String poolName = getAndValidatePoolName(enablePoolAttributes);

		BeanDefinitionBuilder poolFactoryBean = BeanDefinitionBuilder.genericBeanDefinition(PoolFactoryBean.class);

		poolFactoryBean.addPropertyValue("freeConnectionTimeout",
			resolveProperty(namedPoolProperty(poolName, "free-connection-timeout"),
				resolveProperty(poolProperty("free-connection-timeout"),
					(Integer) enablePoolAttributes.get("freeConnectionTimeout"))));

		poolFactoryBean.addPropertyValue("idleTimeout",
			resolveProperty(namedPoolProperty(poolName, "idle-timeout"),
				resolveProperty(poolProperty("idle-timeout"),
					(Long) enablePoolAttributes.get("idleTimeout"))));

		poolFactoryBean.addPropertyValue("loadConditioningInterval",
			resolveProperty(namedPoolProperty(poolName, "load-conditioning-interval"),
				resolveProperty(poolProperty("load-conditioning-interval"),
					(Integer) enablePoolAttributes.get("loadConditioningInterval"))));

		poolFactoryBean.addPropertyValue("maxConnections",
			resolveProperty(namedPoolProperty(poolName, "max-connections"),
				resolveProperty(poolProperty("max-connections"),
					(Integer) enablePoolAttributes.get("maxConnections"))));

		poolFactoryBean.addPropertyValue("minConnections",
			resolveProperty(namedPoolProperty(poolName, "min-connections"),
				resolveProperty(poolProperty("min-connections"),
					(Integer) enablePoolAttributes.get("minConnections"))));

		poolFactoryBean.addPropertyValue("multiUserAuthentication",
			resolveProperty(namedPoolProperty(poolName, "multi-user-authentication"),
				resolveProperty(poolProperty("multi-user-authentication"),
					(Boolean) enablePoolAttributes.get("multiUserAuthentication"))));

		poolFactoryBean.addPropertyValue("pingInterval",
			resolveProperty(namedPoolProperty(poolName, "ping-interval"),
				resolveProperty(poolProperty("ping-interval"),
					(Long) enablePoolAttributes.get("pingInterval"))));

		poolFactoryBean.addPropertyValue("poolConfigurers", resolvePoolConfigurers());

		poolFactoryBean.addPropertyValue("prSingleHopEnabled",
			resolveProperty(namedPoolProperty(poolName, "pr-single-hop-enabled"),
				resolveProperty(poolProperty("pr-single-hop-enabled"),
					(Boolean) enablePoolAttributes.get("prSingleHopEnabled"))));

		poolFactoryBean.addPropertyValue("readTimeout",
			resolveProperty(namedPoolProperty(poolName, "read-timeout"),
				resolveProperty(poolProperty("read-timeout"),
					(Integer) enablePoolAttributes.get("readTimeout"))));

		poolFactoryBean.addPropertyValue("retryAttempts",
			resolveProperty(namedPoolProperty(poolName, "retry-attempts"),
				resolveProperty(poolProperty("retry-attempts"),
					(Integer) enablePoolAttributes.get("retryAttempts"))));

		poolFactoryBean.addPropertyValue("serverGroup",
			resolveProperty(namedPoolProperty(poolName, "server-group"),
				resolveProperty(poolProperty("server-group"),
					(String) enablePoolAttributes.get("serverGroup"))));

		poolFactoryBean.addPropertyValue("socketBufferSize",
			resolveProperty(namedPoolProperty(poolName, "socket-buffer-size"),
				resolveProperty(poolProperty("socket-buffer-size"),
					(Integer) enablePoolAttributes.get("socketBufferSize"))));

		poolFactoryBean.addPropertyValue("statisticInterval",
			resolveProperty(namedPoolProperty(poolName, "statistic-interval"),
				resolveProperty(poolProperty("statistic-interval"),
					(Integer) enablePoolAttributes.get("statisticInterval"))));

		poolFactoryBean.addPropertyValue("subscriptionAckInterval",
			resolveProperty(namedPoolProperty(poolName, "subscription-ack-interval"),
				resolveProperty(poolProperty("subscription-ack-interval"),
					(Integer) enablePoolAttributes.get("subscriptionAckInterval"))));

		poolFactoryBean.addPropertyValue("subscriptionEnabled",
			resolveProperty(namedPoolProperty(poolName, "subscription-enabled"),
				resolveProperty(poolProperty("subscription-enabled"),
					(Boolean) enablePoolAttributes.get("subscriptionEnabled"))));

		poolFactoryBean.addPropertyValue("subscriptionMessageTrackingTimeout",
			resolveProperty(namedPoolProperty(poolName, "subscription-message-tracking-timeout"),
				resolveProperty(poolProperty("subscription-message-tracking-timeout"),
					(Integer) enablePoolAttributes.get("subscriptionMessageTrackingTimeout"))));

		poolFactoryBean.addPropertyValue("subscriptionRedundancy",
			resolveProperty(namedPoolProperty(poolName, "subscription-redundancy"),
				resolveProperty(poolProperty("subscription-redundancy"),
					(Integer) enablePoolAttributes.get("subscriptionRedundancy"))));

		poolFactoryBean.addPropertyValue("threadLocalConnections",
			resolveProperty(namedPoolProperty(poolName, "thread-local-connections"),
				resolveProperty(poolProperty("thread-local-connections"),
					(Boolean) enablePoolAttributes.get("threadLocalConnections"))));

		configurePoolConnections(poolName, enablePoolAttributes, poolFactoryBean);

		registry.registerBeanDefinition(poolName, poolFactoryBean.getBeanDefinition());
	}

	/* (non-Javadoc) */
	private List<PoolConfigurer> resolvePoolConfigurers() {

		return Optional.ofNullable(this.poolConfigurers)
			.filter(poolConfigurers -> !poolConfigurers.isEmpty())
			.orElseGet(() ->
				Optional.of(this.getBeanFactory())
					.filter(beanFactory -> beanFactory instanceof ListableBeanFactory)
					.map(beanFactory -> {
						Map<String, PoolConfigurer> beansOfType = ((ListableBeanFactory) beanFactory)
							.getBeansOfType(PoolConfigurer.class, true, true);

						return nullSafeMap(beansOfType).values().stream().collect(Collectors.toList());
					})
					.orElseGet(Collections::emptyList)
			);
	}

	/* (non-Javadoc) */
	protected String getAndValidatePoolName(Map<String, Object> enablePoolAttributes) {

		return Optional.ofNullable((String) enablePoolAttributes.get("name"))
			.filter(StringUtils::hasText)
			.orElseThrow(() -> newIllegalArgumentException("Pool name is required"));
	}

	/**
	 * Uses the list of GemFire Locator and Server connection endpoint definitions and meta-data to configure
	 * the GemFire client {@link org.apache.geode.cache.client.Pool} used to communicate with the servers
	 * in the GemFire cluster.
	 *
	 * @param enablePoolAttributes {@link EnablePool} annotation containing
	 * {@link org.apache.geode.cache.client.Pool} Locator/Server connection endpoint meta-data.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
	 * @see java.util.Map
	 */
	protected BeanDefinitionBuilder configurePoolConnections(String poolName, Map<String, Object> enablePoolAttributes,
			BeanDefinitionBuilder poolFactoryBean) {

		configurePoolLocators(poolName, enablePoolAttributes, poolFactoryBean);
		configurePoolServers(poolName, enablePoolAttributes, poolFactoryBean);

		return poolFactoryBean;
	}

	/* (non-Javadoc) */
	protected BeanDefinitionBuilder configurePoolLocators(String poolName, Map<String, Object> enablePoolAttributes,
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

	/* (non-Javadoc) */
	protected BeanDefinitionBuilder configurePoolServers(String poolName, Map<String, Object> enablePoolAttributes,
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

	/* (non-Javadoc) */
	protected ConnectionEndpointList parseConnectionEndpoints(Map<String, Object> enablePoolAttributes,
			String arrayAttributeName, String stringAttributeName, int defaultPort) {

		AnnotationAttributes[] connectionEndpointsMetaData =
			(AnnotationAttributes[]) enablePoolAttributes.get(arrayAttributeName);

		ConnectionEndpointList connectionEndpoints = new ConnectionEndpointList();

		stream(nullSafeArray(connectionEndpointsMetaData, AnnotationAttributes.class))
			.forEach(annotationAttributes ->
				connectionEndpoints.add(newConnectionEndpoint((String) annotationAttributes.get("host"),
					(Integer) annotationAttributes.get("port"))));

		Optional.ofNullable((String) enablePoolAttributes.get(stringAttributeName))
			.filter(StringUtils::hasText)
			.ifPresent(hostsPorts ->
				connectionEndpoints.add(ConnectionEndpointList.parse(defaultPort, hostsPorts.split(","))));

		return connectionEndpoints;
	}

	/* (non-Javadoc) */
	protected ConnectionEndpoint newConnectionEndpoint(String host, Integer port) {
		return new ConnectionEndpoint(host, port);
	}

	/* (non-Javadoc) */
	@Override
	protected Class getAnnotationType() {
		return EnablePool.class;
	}
}
