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

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.server.CacheServerFactoryBean;
import org.springframework.util.StringUtils;

/**
 * The {@link AddCacheServerConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that registers
 * a {@link CacheServerFactoryBean} definition for the {@link org.apache.geode.cache.server.CacheServer}
 * configuration meta-data defined in {@link EnableCacheServer} annotation.
 *
 * @author John Blum
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.config.BeanDefinitionHolder
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
 * @see org.springframework.data.gemfire.server.CacheServerFactoryBean
 * @since 1.9.0
 */
public class AddCacheServerConfiguration implements BeanFactoryAware, ImportBeanDefinitionRegistrar {

	private BeanFactory beanFactory;

	@Autowired(required = false)
	private List<CacheServerConfigurer> cacheServerConfigurers = Collections.emptyList();

	/**
	 * {@inheritDoc}
	 */
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

		builder.addPropertyReference("cache", GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);
		builder.addPropertyValue("cacheServerConfigurers", resolveCacheServerConfigurers());
		builder.addPropertyValue("autoStartup", enableCacheServerAttributes.get("autoStartup"));
		builder.addPropertyValue("bindAddress", enableCacheServerAttributes.get("bindAddress"));
		builder.addPropertyValue("hostNameForClients", enableCacheServerAttributes.get("hostnameForClients"));
		builder.addPropertyValue("loadPollInterval", enableCacheServerAttributes.get("loadPollInterval"));
		builder.addPropertyValue("maxConnections", enableCacheServerAttributes.get("maxConnections"));
		builder.addPropertyValue("maxMessageCount", enableCacheServerAttributes.get("maxMessageCount"));
		builder.addPropertyValue("maxThreads", enableCacheServerAttributes.get("maxThreads"));
		builder.addPropertyValue("maxTimeBetweenPings", enableCacheServerAttributes.get("maxTimeBetweenPings"));
		builder.addPropertyValue("messageTimeToLive", enableCacheServerAttributes.get("messageTimeToLive"));
		builder.addPropertyValue("port", enableCacheServerAttributes.get("port"));
		builder.addPropertyValue("socketBufferSize", enableCacheServerAttributes.get("socketBufferSize"));
		builder.addPropertyValue("subscriptionCapacity", enableCacheServerAttributes.get("subscriptionCapacity"));
		builder.addPropertyValue("subscriptionDiskStore", enableCacheServerAttributes.get("subscriptionDiskStoreName"));
		builder.addPropertyValue("subscriptionEvictionPolicy", enableCacheServerAttributes.get("subscriptionEvictionPolicy"));

		registerCacheServerFactoryBeanDefinition(builder.getBeanDefinition(),
			(String) enableCacheServerAttributes.get("name"), registry);
	}

	/* (non-Javadoc) */
	private List<CacheServerConfigurer> resolveCacheServerConfigurers() {

		return Optional.ofNullable(this.cacheServerConfigurers)
			.filter(cacheServerConfigurers -> !cacheServerConfigurers.isEmpty())
			.orElseGet(() ->
				Optional.of(this.beanFactory)
					.filter(beanFactory -> beanFactory instanceof ListableBeanFactory)
					.map(beanFactory -> {
						Map<String, CacheServerConfigurer> beansOfType = ((ListableBeanFactory) beanFactory)
							.getBeansOfType(CacheServerConfigurer.class, true, true);

						return nullSafeMap(beansOfType).values().stream().collect(Collectors.toList());
					})
					.orElseGet(Collections::emptyList)
			);

	}
	/* (non-Javadoc) */
	protected void registerCacheServerFactoryBeanDefinition(AbstractBeanDefinition beanDefinition, String beanName,
			BeanDefinitionRegistry registry) {

		if (StringUtils.hasText(beanName)) {
			BeanDefinitionReaderUtils.registerBeanDefinition(
				newBeanDefinitionHolder(beanDefinition, beanName), registry);
		}
		else {
			BeanDefinitionReaderUtils.registerWithGeneratedName(beanDefinition, registry);
		}
	}

	/* (non-Javadoc) */
	protected BeanDefinitionHolder newBeanDefinitionHolder(BeanDefinition beanDefinition, String beanName) {
		return new BeanDefinitionHolder(beanDefinition, beanName);
	}

	/* (non-Javadoc) */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}
}
