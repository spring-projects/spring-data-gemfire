/*
 * Copyright 2018-2019 the original author or authors.
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

package org.springframework.data.gemfire.config.xml.support;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.function.Function;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.function.annotation.OnRegion;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests to test the configuration of the {@link ClientCache} {@literal DEFAULT} {@link Pool},
 * client {@link Region Region's} using a configured {@link Pool} and a {@link Function} referring to
 * a client {@link Region} requiring the configured {@link Pool}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.Pool
 * @see <a href=""></a>
 * @since 1.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class PoolAlreadyExistsIntegrationTests {

	@Autowired
	private ClientCache clientCache;

	private void assertRegion(Region<?, ?> region, String name) {

		assertThat(region).isNotNull();
		assertThat(region.getName()).isEqualTo(GemfireUtils.toRegionName(name));
		assertThat(region.getFullPath()).isEqualTo(GemfireUtils.toRegionPath(name));
		assertThat(region.getAttributes()).isNotNull();
		assertThat(region.getAttributes().getPoolName()).isEqualTo("TestPool");
	}

	@Test
	public void regionsConfiguredWithPool() {

		assertRegion(this.clientCache.getRegion("RegionOne"), "RegionOne");
		assertRegion(this.clientCache.getRegion("RegionTwo"), "RegionTwo");
	}

	@OnRegion(id = "TestFunction", region = "RegionTwo")
	interface TestFunctionExecution {

		void doSomething();

	}
}
