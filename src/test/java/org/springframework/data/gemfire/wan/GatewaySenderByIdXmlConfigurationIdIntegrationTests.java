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
package org.springframework.data.gemfire.wan;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;

import javax.annotation.Resource;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.wan.GatewaySender;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.EnableLocator;
import org.springframework.data.gemfire.config.annotation.PeerCacheApplication;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests testing the configuration of {@link GatewaySender GatewaySenders} on a cache {@link Region}
 * by {@literal identifier} using the SDG XML Namespace.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.springframework.data.gemfire.config.annotation.EnableLocator
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
 * @see org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.2.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(locations = "GatewaySenderByIdXmlConfigurationIdIntegrationTests-context.xml")
@SuppressWarnings("unused")
public class GatewaySenderByIdXmlConfigurationIdIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static ProcessWrapper geodeServer;

	private static final String GEMFIRE_LOG_LEVEL = "error";

	@BeforeClass
	public static void startGeodeServer() throws IOException {

		int port = findAvailablePort();

		System.setProperty("spring.data.gemfire.locator.port", String.valueOf(port));

		geodeServer = run(GeodeServerConfiguration.class, "-Dspring.data.gemfire.locator.port=" + port);
		waitForServerToStart("localhost", port);
	}

	@AfterClass
	public static void stopGeodeServer() {

		stop(geodeServer);

		System.getProperties().stringPropertyNames().stream()
			.filter(propertyName -> propertyName.startsWith("spring.data.gemfire"))
			.forEach(System::clearProperty);
	}

	@Resource(name = "Example")
	private Region<?, ?> example;

	@Test
	public void regionGatewaySendersByIdConfiguredCorrectly() {

		assertThat(this.example).isNotNull();
		assertThat(this.example.getName()).isEqualTo("Example");

		RegionAttributes<?, ?> exampleAttributes = this.example.getAttributes();

		assertThat(exampleAttributes).isNotNull();
		assertThat(exampleAttributes.getDataPolicy()).isEqualTo(DataPolicy.REPLICATE);
		assertThat(exampleAttributes.getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySenderOne", "TestGatewaySenderTwo");
	}

	@EnableLocator
	@PeerCacheApplication(logLevel = GEMFIRE_LOG_LEVEL)
	static class GeodeServerConfiguration {

		public static void main(String[] args) {

			//System.err.printf("Locator Port [%s]%n", System.getProperty("spring.data.gemfire.locator.port"));

			runSpringApplication(GeodeServerConfiguration.class, args);
			block();
		}

		@Bean("TestGatewaySenderOne")
		public GatewaySenderFactoryBean gatewaySenderOne(Cache cache) {

			GatewaySenderFactoryBean gatewaySenderOne = new GatewaySenderFactoryBean(cache);

			gatewaySenderOne.setRemoteDistributedSystemId(1);
			gatewaySenderOne.setManualStart(true);

			return gatewaySenderOne;
		}

		@Bean("TestGatewaySenderTwo")
		public GatewaySenderFactoryBean gatewaySenderTwo(Cache cache) {

			GatewaySenderFactoryBean gatewaySenderTwo = new GatewaySenderFactoryBean(cache);

			gatewaySenderTwo.setRemoteDistributedSystemId(1);
			gatewaySenderTwo.setManualStart(true);

			return gatewaySenderTwo;
		}

		@Bean("Example")
		public ReplicatedRegionFactoryBean<Object, Object> exampleRegion(GemFireCache gemfireCache,
				@Qualifier("TestGatewaySenderOne") GatewaySender gatewaySenderOne,
				@Qualifier("TestGatewaySenderTwo") GatewaySender gatewaySenderTwo) {

			ReplicatedRegionFactoryBean<Object, Object> exampleRegion = new ReplicatedRegionFactoryBean<>();

			exampleRegion.setCache(gemfireCache);
			exampleRegion.setGatewaySenders(ArrayUtils.asArray(gatewaySenderOne, gatewaySenderTwo));

			return exampleRegion;
		}
	}
}
