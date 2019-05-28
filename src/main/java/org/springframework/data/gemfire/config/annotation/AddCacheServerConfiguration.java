/*
 * Copyright 2012-2019 the original author or authors.
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

import java.lang.annotation.Annotation;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.server.CacheServerFactoryBean;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;
import org.springframework.util.StringUtils;

/**
 * The {@link AddCacheServerConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that registers
 * a {@link CacheServerFactoryBean} definition for the {@link org.apache.geode.cache.server.CacheServer}
 * configuration meta-data defined in {@link EnableCacheServer} annotation.
 *
 * @author John Blum
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.annotation.AddCacheServersConfiguration
 * @see org.springframework.data.gemfire.config.annotation.CacheServerApplication
 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfiguration
 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfigurer
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServers
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServer
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @see org.springframework.data.gemfire.server.CacheServerFactoryBean
 * @since 1.9.0
 */
public class AddCacheServerConfiguration extends AbstractAnnotationConfigSupport
		implements ImportBeanDefinitionRegistrar {

	@Autowired(required = false)
	private List<CacheServerConfigurer> cacheServerConfigurers = Collections.emptyList();

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableCacheServer.class;
	}

	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

		if (importingClassMetadata.hasAnnotation(EnableCacheServer.class.getName())) {

			Map<String, Object> enableCacheServerAttributes =
				importingClassMetadata.getAnnotationAttributes(EnableCacheServer.class.getName());

			registerCacheServerFactoryBeanDefinition(enableCacheServerAttributes, registry);
		}
	}

	/**
	 * Registers a {@link CacheServerFactoryBean} bean definition for the given {@link EnableCacheServer} annotation
	 * configuration meta-data.
	 *
	 * @param enableCacheServerAttributes attributes for the {@link EnableCacheServer} annotation.
	 * @param registry {@link BeanDefinitionRegistry} used to register the {@link CacheServerFactoryBean}
	 * bean definition.
	 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
	 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
	 * @see org.springframework.data.gemfire.server.CacheServerFactoryBean
	 */
	protected void registerCacheServerFactoryBeanDefinition(Map<String, Object> enableCacheServerAttributes,
			BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(CacheServerFactoryBean.class);

		String beanName = registerCacheServerFactoryBeanDefinition(builder.getBeanDefinition(),
			(String) enableCacheServerAttributes.get("name"), registry);

		builder.addPropertyReference("cache", GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);
		builder.addPropertyValue("cacheServerConfigurers", resolveCacheServerConfigurers());

		builder.addPropertyValue("autoStartup",
			resolveProperty(namedCacheServerProperty(beanName, "auto-startup"),
				resolveProperty(cacheServerProperty("auto-startup"),
					(Boolean) enableCacheServerAttributes.get("autoStartup"))));

		builder.addPropertyValue("bindAddress",
			resolveProperty(namedCacheServerProperty(beanName, "bind-address"),
				resolveProperty(cacheServerProperty("bind-address"),
					(String) enableCacheServerAttributes.get("bindAddress"))));

		builder.addPropertyValue("hostNameForClients",
			resolveProperty(namedCacheServerProperty(beanName, "hostname-for-clients"),
				resolveProperty(cacheServerProperty("hostname-for-clients"),
					(String) enableCacheServerAttributes.get("hostnameForClients"))));

		builder.addPropertyValue("loadPollInterval",
			resolveProperty(namedCacheServerProperty(beanName, "load-poll-interval"),
				resolveProperty(cacheServerProperty("load-poll-interval"),
					(Long) enableCacheServerAttributes.get("loadPollInterval"))));

		builder.addPropertyValue("maxConnections",
			resolveProperty(namedCacheServerProperty(beanName, "max-connections"),
				resolveProperty(cacheServerProperty("max-connections"),
					(Integer) enableCacheServerAttributes.get("maxConnections"))));

		builder.addPropertyValue("maxMessageCount",
			resolveProperty(namedCacheServerProperty(beanName, "max-message-count"),
				resolveProperty(cacheServerProperty("max-message-count"),
					(Integer) enableCacheServerAttributes.get("maxMessageCount"))));

		builder.addPropertyValue("maxThreads",
			resolveProperty(namedCacheServerProperty(beanName, "max-threads"),
				resolveProperty(cacheServerProperty("max-threads"),
					(Integer) enableCacheServerAttributes.get("maxThreads"))));

		builder.addPropertyValue("maxTimeBetweenPings",
			resolveProperty(namedCacheServerProperty(beanName, "max-time-between-pings"),
				resolveProperty(cacheServerProperty("max-time-between-pings"),
					(Integer) enableCacheServerAttributes.get("maxTimeBetweenPings"))));

		builder.addPropertyValue("messageTimeToLive",
			resolveProperty(namedCacheServerProperty(beanName, "message-time-to-live"),
				resolveProperty(cacheServerProperty("message-time-to-live"),
					(Integer) enableCacheServerAttributes.get("messageTimeToLive"))));

		builder.addPropertyValue("port",
			resolveProperty(namedCacheServerProperty(beanName, "port"),
				resolveProperty(cacheServerProperty("port"),
					(Integer) enableCacheServerAttributes.get("port"))));

		builder.addPropertyValue("socketBufferSize",
			resolveProperty(namedCacheServerProperty(beanName, "socket-buffer-size"),
				resolveProperty(cacheServerProperty("socket-buffer-size"),
					(Integer) enableCacheServerAttributes.get("socketBufferSize"))));

		builder.addPropertyValue("subscriptionCapacity",
			resolveProperty(namedCacheServerProperty(beanName, "subscription-capacity"),
				resolveProperty(cacheServerProperty("subscription-capacity"),
					(Integer) enableCacheServerAttributes.get("subscriptionCapacity"))));

		builder.addPropertyValue("subscriptionDiskStore",
			resolveProperty(namedCacheServerProperty(beanName, "subscription-disk-store-name"),
				resolveProperty(cacheServerProperty("subscription-disk-store-name"),
					(String) enableCacheServerAttributes.get("subscriptionDiskStoreName"))));

		builder.addPropertyValue("subscriptionEvictionPolicy",
			resolveProperty(namedCacheServerProperty(beanName, "subscription-eviction-policy"),
				SubscriptionEvictionPolicy.class, resolveProperty(cacheServerProperty("subscription-eviction-policy"),
					SubscriptionEvictionPolicy.class, (SubscriptionEvictionPolicy) enableCacheServerAttributes.get("subscriptionEvictionPolicy"))));

		builder.addPropertyValue("tcpNoDelay",
			resolveProperty(namedCacheServerProperty(beanName, "tcp-no-delay"),
				resolveProperty(cacheServerProperty("tcp-no-delay"),
					(Boolean) enableCacheServerAttributes.get("tcpNoDelay"))));
	}

	private List<CacheServerConfigurer> resolveCacheServerConfigurers() {

		return Optional.ofNullable(this.cacheServerConfigurers)
			.filter(cacheServerConfigurers -> !cacheServerConfigurers.isEmpty())
			.orElseGet(() ->
				Collections.singletonList(LazyResolvingComposableCacheServerConfigurer.create(getBeanFactory())));

	}

	protected String registerCacheServerFactoryBeanDefinition(AbstractBeanDefinition beanDefinition, String beanName,
			BeanDefinitionRegistry registry) {

		if (StringUtils.hasText(beanName)) {
			BeanDefinitionReaderUtils.registerBeanDefinition(
				newBeanDefinitionHolder(beanDefinition, beanName), registry);

			return beanName;
		}
		else {
			return BeanDefinitionReaderUtils.registerWithGeneratedName(beanDefinition, registry);
		}
	}

	protected BeanDefinitionHolder newBeanDefinitionHolder(BeanDefinition beanDefinition, String beanName) {
		return new BeanDefinitionHolder(beanDefinition, beanName);
	}
}
