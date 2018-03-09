/*
 * Copyright 2012-2018 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;

import javax.annotation.Resource;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link PeerCacheApplication} SDG annotation.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.apache.geode.cache.Cache
 * @since 1.9.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = PeerCacheApplicationIntegrationTests.PeerCacheApplicationConfiguration.class)
@SuppressWarnings("all")
public class PeerCacheApplicationIntegrationTests {

	@Resource(name = "Echo")
	private Region<String, String> echo;

	@Test
	public void echoPartitionRegionEchoesKeysAsValues() {
		assertThat(echo.get("Hello")).isEqualTo("Hello");
		assertThat(echo.get("Test")).isEqualTo("Test");
	}

	@PeerCacheApplication(name = "PeerCacheApplicationIntegrationTests", logLevel="warn")
	static class PeerCacheApplicationConfiguration {

		@Bean("Echo")
		PartitionedRegionFactoryBean<String, String> echoRegion(Cache gemfireCache) {

			PartitionedRegionFactoryBean<String, String> echoRegion =
				new PartitionedRegionFactoryBean<String, String>();

			echoRegion.setCache(gemfireCache);
			echoRegion.setCacheLoader(echoCacheLoader());
			echoRegion.setClose(false);
			echoRegion.setPersistent(false);

			return echoRegion;
		}

		CacheLoader<String, String> echoCacheLoader() {

			return new CacheLoader<String, String>() {

				@Override
				public String load(LoaderHelper<String, String> helper) throws CacheLoaderException {
					return helper.getKey();
				}

				@Override
				public void close() {
				}
			};
		}
	}
}
