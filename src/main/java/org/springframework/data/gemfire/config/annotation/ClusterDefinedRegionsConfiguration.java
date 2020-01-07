/*
 * Copyright 2018-2020 the original author or authors.
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
 */
package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Annotation;
import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionShortcut;

import org.apache.shiro.util.Assert;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.annotation.Order;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.util.CacheUtils;
import org.springframework.util.ObjectUtils;

/**
 * The {@link ClusterDefinedRegionsConfiguration} class configures client Proxy-based {@link Region Regions}
 * for all {@link Region Regions} defined in the cluster to which the cache client is connected.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.core.annotation.AnnotationAttributes
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @since 2.1.0
 */
@Configuration
public class ClusterDefinedRegionsConfiguration extends AbstractAnnotationConfigSupport implements ImportAware {

	protected static final ClientRegionShortcut DEFAULT_CLIENT_REGION_SHORTCUT = ClientRegionShortcut.PROXY;

	private ClientRegionShortcut clientRegionShortcut = DEFAULT_CLIENT_REGION_SHORTCUT;

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableClusterDefinedRegions.class;
	}

	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {

		AnnotationAttributes enableClusterDefinedRegionsAttributes = getAnnotationAttributes(importMetadata);

		setClientRegionShortcut(enableClusterDefinedRegionsAttributes.getEnum("clientRegionShortcut"));
	}

	protected void setClientRegionShortcut(ClientRegionShortcut clientRegionShortcut) {
		this.clientRegionShortcut = clientRegionShortcut;
	}

	protected Optional<ClientRegionShortcut> getClientRegionShortcut() {
		return Optional.ofNullable(this.clientRegionShortcut);
	}

	protected ClientRegionShortcut resolveClientRegionShortcut() {
		return getClientRegionShortcut().orElse(DEFAULT_CLIENT_REGION_SHORTCUT);
	}

	@Bean
	@Order(Ordered.HIGHEST_PRECEDENCE + 1000000)
	public GemfireDataSourcePostProcessor gemfireDataSourcePostProcessor() {
		return new GemfireDataSourcePostProcessor().using(getBeanFactory()).using(resolveClientRegionShortcut());
	}

	@Bean
	Object nullCacheDependentBean(GemFireCache cache) {

		Assert.isTrue(CacheUtils.isClient(cache), String.format("GemFireCache [%s] must be a %s",
			ObjectUtils.nullSafeClassName(cache), ClientCache.class.getName()));

		return null;
	}
}
