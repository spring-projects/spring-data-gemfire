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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionShortcut;

import org.junit.Test;

import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor;

/**
 * Unit tests for {@link EnableClusterDefinedRegions} and {@link ClusterDefinedRegionsConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor
 * @see org.springframework.data.gemfire.config.annotation.ClusterDefinedRegionsConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableClusterDefinedRegions
 * @since 2.1.0
 */
public class EnableClusterDefinedRegionsUnitTests {

	@Test
	public void configuresClientRegionShortcutUsingAnnotationMetadata() {

		Map<String, Object> enableClusterDefinedRegionsAttributes =
			Collections.singletonMap("clientRegionShortcut", ClientRegionShortcut.LOCAL);

		AnnotationMetadata mockAnnotationMetadata = mock(AnnotationMetadata.class);

		when(mockAnnotationMetadata.getAnnotationAttributes(EnableClusterDefinedRegions.class.getName()))
			.thenReturn(enableClusterDefinedRegionsAttributes);

		ClusterDefinedRegionsConfiguration configuration = new ClusterDefinedRegionsConfiguration();

		configuration.setImportMetadata(mockAnnotationMetadata);

		assertThat(configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.LOCAL);
	}

	@Test
	public void setGetAndResolveClientRegionShortcut() {

		ClusterDefinedRegionsConfiguration configuration = new ClusterDefinedRegionsConfiguration();

		assertThat(configuration.getClientRegionShortcut().orElse(null)).isEqualTo(ClientRegionShortcut.PROXY);
		assertThat(configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.PROXY);

		configuration.setClientRegionShortcut(ClientRegionShortcut.CACHING_PROXY);

		assertThat(configuration.getClientRegionShortcut().orElse(null)).isEqualTo(ClientRegionShortcut.CACHING_PROXY);
		assertThat(configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.CACHING_PROXY);

		configuration.setClientRegionShortcut(ClientRegionShortcut.LOCAL);

		assertThat(configuration.getClientRegionShortcut().orElse(null)).isEqualTo(ClientRegionShortcut.LOCAL);
		assertThat(configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.LOCAL);

		configuration.setClientRegionShortcut(null);

		assertThat(configuration.getClientRegionShortcut().orElse(null)).isNull();
		assertThat(configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.PROXY);
	}

	@Test
	public void configureGemfireDataSourcePostProcessor() {

		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class);

		ClusterDefinedRegionsConfiguration configuration = new ClusterDefinedRegionsConfiguration();

		configuration.setBeanFactory(mockBeanFactory);
		configuration.setClientRegionShortcut(ClientRegionShortcut.CACHING_PROXY);

		GemfireDataSourcePostProcessor postProcessor = configuration.gemfireDataSourcePostProcessor();

		assertThat(postProcessor.getBeanFactory().orElse(null)).isEqualTo(mockBeanFactory);
		assertThat(postProcessor.getClientRegionShortcut().orElse(null))
			.isEqualTo(ClientRegionShortcut.CACHING_PROXY);
	}

	@Test(expected = IllegalArgumentException.class)
	public void configuresSpringApplicationWithGenericCache() {

		GemFireCache mockCache = mock(GemFireCache.class);

		try {
			new ClusterDefinedRegionsConfiguration().nullCacheDependentBean(mockCache);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("GemFireCache [%s] must be a %s",
				mockCache.getClass().getName(), ClientCache.class.getName());

			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void configuresSpringApplicationWithNullCache() {

		try {
			new ClusterDefinedRegionsConfiguration().nullCacheDependentBean(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("GemFireCache [null] must be a %s", ClientCache.class.getName());
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void configuresSpringApplicationWithPeerCache() {

		Cache mockCache = mock(Cache.class);

		try {
			new ClusterDefinedRegionsConfiguration().nullCacheDependentBean(mockCache);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("GemFireCache [%s] must be a %s",
				mockCache.getClass().getName(), ClientCache.class.getName());

			assertThat(expected).hasNoCause();

			throw expected;
		}
	}
}
