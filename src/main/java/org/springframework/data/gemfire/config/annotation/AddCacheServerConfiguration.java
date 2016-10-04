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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.server.CacheServerFactoryBean;

/**
 * The {@link AddCacheServerConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that registers
 * a {@link CacheServerFactoryBean} definition for the {@link com.gemstone.gemfire.cache.server.CacheServer}
 * configuration meta-data defined in {@link EnableCacheServer}.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServer
 * @since 1.9.0
 */
public class AddCacheServerConfiguration implements ImportBeanDefinitionRegistrar {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {
		if (importingClassMetadata.hasAnnotation(EnableCacheServer.class.getName())) {
			Map<String, Object> enableCacheServerAttributes = importingClassMetadata.getAnnotationAttributes(
				EnableCacheServer.class.getName());

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

		BeanDefinitionReaderUtils.registerWithGeneratedName(builder.getBeanDefinition(), registry);
	}
}
