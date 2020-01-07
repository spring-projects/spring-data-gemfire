/*
 * Copyright 2017-2020 the original author or authors.
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

package org.springframework.data.gemfire.client;

import static org.assertj.core.api.Assertions.assertThat;

import javax.annotation.Resource;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.config.annotation.ClientCacheApplication;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for client {@link Region Regions} created with SDG's {@link ClientRegionFactoryBean}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @since 2.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
public class ClientRegionIntegrationTests {

	@Resource(name = "Example")
	private Region<Object, Object> example;

	@Test
	public void clientRegionUsesDefaultPoolWhenUnspecified() {
		assertThat(this.example.getAttributes().getPoolName()).isNull();
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	static class ClientRegionConfiguration {

		@Bean("Example")
		public ClientRegionFactoryBean<Object, Object> exampleRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> exampleRegion = new ClientRegionFactoryBean<>();

			exampleRegion.setCache(gemfireCache);
			exampleRegion.setClose(false);
			exampleRegion.setShortcut(ClientRegionShortcut.LOCAL);

			return exampleRegion;
		}
	}
}
