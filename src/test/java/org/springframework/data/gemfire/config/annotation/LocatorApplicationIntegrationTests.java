/*
 * Copyright 2018 the original author or authors.
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

import java.util.Properties;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.distributed.Locator;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests for {@link LocatorApplication} and {@link LocatorApplicationConfiguration} asserting that
 * an Apache Geode / Pivotal GemFire {@link Cache} application can connect to the {@link Locator} configured
 * and bootstrapped by {@link LocatorApplication}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.CacheFactory
 * @see org.apache.geode.distributed.DistributedSystem
 * @see org.apache.geode.distributed.Locator
 * @see org.springframework.data.gemfire.config.annotation.LocatorApplication
 * @see org.springframework.data.gemfire.config.annotation.LocatorApplicationConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.2.0
 */
@RunWith(SpringRunner.class)
@SuppressWarnings("unused")
public class LocatorApplicationIntegrationTests {

	private static final String GEMFIRE_LOG_LEVEL = "error";

	@Autowired
	private Locator locator;

	@Before
	public void setup() {
		assertThat(this.locator).isNotNull();
	}

	@Test
	public void gemfireCacheCanConnectToLocator() {

		DistributedSystem distributedSystem = this.locator.getDistributedSystem();

		assertThat(distributedSystem).isNotNull();

		Properties distributedSystemProperties = distributedSystem.getProperties();

		assertThat(distributedSystemProperties).isNotNull();

		Cache peerCache = null;

		try {
			peerCache = new CacheFactory()
				.set("name", LocatorApplicationIntegrationTests.class.getSimpleName())
				.set("bind-address", distributedSystemProperties.getProperty("bind-address"))
				.set("cache-xml-file", distributedSystemProperties.getProperty("cache-xml-file"))
				.set("jmx-manager", distributedSystemProperties.getProperty("jmx-manager"))
				.set("locators", distributedSystemProperties.getProperty("locators"))
				.set("log-file", distributedSystemProperties.getProperty("log-file"))
				.set("log-level", distributedSystemProperties.getProperty("log-level"))
				.create();

			assertThat(peerCache).isNotNull();
			assertThat(peerCache.getDistributedSystem()).isNotNull();
			assertThat(peerCache.getDistributedSystem().getProperties()).isNotNull();
			assertThat(peerCache.getDistributedSystem().getProperties().getProperty("locators"))
				.isEqualTo(distributedSystemProperties.getProperty("locators"));
		}
		finally {
			GemfireUtils.close(peerCache);
		}
	}

	@LocatorApplication(
		name = "LocatorApplicationIntegrationTests",
		bindAddress = "localhost",
		logLevel = GEMFIRE_LOG_LEVEL,
		port = 0
	)
	static class TestConfiguration { }

}
