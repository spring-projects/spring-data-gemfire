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
package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;
import java.util.Collections;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionShortcut;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.IndexFactoryBean;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests for the {@link EnableClusterConfiguration} annotation
 * and {@link ClusterConfigurationConfiguration} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
 * @see org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableClusterConfiguration
 * @see org.springframework.data.gemfire.process.ProcessWrapper
 * @see org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = EnableClusterConfigurationIntegrationTests.TestConfiguration.class)
@SuppressWarnings("unused")
public class EnableClusterConfigurationIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static final String GEMFIRE_LOG_LEVEL = "error";

	private static ProcessWrapper gemfireServer;

	@Autowired
	private ClientCache gemfireCache;

	private GemfireAdminOperations adminOperations;

	@BeforeClass
	public static void startGemFireServer() throws Exception {

		int availablePort = findAvailablePort();

		gemfireServer = run(ServerTestConfiguration.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort));

		waitForServerToStart("localhost", availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {

		stop(gemfireServer);
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
	}

	@Before
	public void setup() {

		this.adminOperations = new RestHttpGemfireAdminTemplate.Builder()
			.with(this.gemfireCache)
			.on("localhost")
			.listenOn(Integer.getInteger(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, 40404))
			.build();
	}

	@Test
	public void serverIndexesAreCorrect() {

		assertThat(this.adminOperations.getAvailableServerRegionIndexes())
			.containsAll(Arrays.asList("IndexOne", "IndexTwo"));
	}

	@Test
	public void serverRegionsAreCorrect() {

		assertThat(this.adminOperations.getAvailableServerRegions())
			.containsAll(Arrays.asList("RegionOne", "RegionTwo", "RegionThree", "RegionFour"));
	}

	@Configuration
	@EnableClusterConfiguration
	@Import(ClientTestConfiguration.class)
	static class TestConfiguration { }

	@ClientCacheApplication(logLevel = GEMFIRE_LOG_LEVEL, subscriptionEnabled = true)
	static class ClientTestConfiguration {

		@Bean
		ClientCacheConfigurer clientCachePoolPortConfigurer(
				@Value("${" + GEMFIRE_CACHE_SERVER_PORT_PROPERTY + ":40404}") int port) {

			return (bean, clientCacheFactoryBean) -> clientCacheFactoryBean
				.setServers(Collections.singletonList(new ConnectionEndpoint("localhost", port)));
		}

		@Bean("IndexOne")
		@DependsOn("RegionOne")
		IndexFactoryBean indexOne(GemFireCache gemfireCache) {

			IndexFactoryBean indexFactory = new IndexFactoryBean();

			indexFactory.setCache(gemfireCache);
			indexFactory.setExpression("id");
			indexFactory.setFrom("/RegionOne");
			indexFactory.setType(IndexType.KEY);

			return indexFactory;
		}

		@Bean("RegionOne")
		ClientRegionFactoryBean<Object, Object> regionOne(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> clientRegionFactory = new ClientRegionFactoryBean<>();

			clientRegionFactory.setCache(gemfireCache);
			clientRegionFactory.setClose(false);
			clientRegionFactory.setShortcut(ClientRegionShortcut.PROXY);

			return clientRegionFactory;
		}

		@Bean("RegionTwo")
		ClientRegionFactoryBean<Object, Object> regionTwo(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> clientRegionFactory = new ClientRegionFactoryBean<>();

			clientRegionFactory.setCache(gemfireCache);
			clientRegionFactory.setClose(false);
			clientRegionFactory.setShortcut(ClientRegionShortcut.PROXY);

			return clientRegionFactory;
		}

		@Bean("RegionThree")
		ClientRegionFactoryBean<Object, Object> regionThree(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> clientRegionFactory = new ClientRegionFactoryBean<>();

			clientRegionFactory.setCache(gemfireCache);
			clientRegionFactory.setClose(false);
			clientRegionFactory.setShortcut(ClientRegionShortcut.CACHING_PROXY);

			return clientRegionFactory;
		}
	}

	@CacheServerApplication(name = "EnableClusterConfigurationIntegrationTests", logLevel = GEMFIRE_LOG_LEVEL)
	static class ServerTestConfiguration {

		public static void main(String[] args) {

			AnnotationConfigApplicationContext applicationContext =
				new AnnotationConfigApplicationContext(ServerTestConfiguration.class);

			applicationContext.registerShutdownHook();
		}

		@Bean
		CacheServerConfigurer cacheServerPortConfigurer(
				@Value("${" + GEMFIRE_CACHE_SERVER_PORT_PROPERTY + ":40404}") int port) {

			return (bean, cacheServerFactoryBean) -> cacheServerFactoryBean.setPort(port);
		}

		@Bean("IndexTwo")
		@DependsOn("RegionTwo")
		IndexFactoryBean indexTwo(GemFireCache gemfireCache) {

			IndexFactoryBean indexFactory = new IndexFactoryBean();

			indexFactory.setCache(gemfireCache);
			indexFactory.setExpression("name");
			indexFactory.setFrom("/RegionTwo");
			indexFactory.setType(IndexType.HASH);

			return indexFactory;
		}

		@Bean("RegionTwo")
		ReplicatedRegionFactoryBean<Object, Object> regionTwo(GemFireCache gemfireCache) {

			ReplicatedRegionFactoryBean<Object, Object> regionFactoryBean = new ReplicatedRegionFactoryBean<>();

			regionFactoryBean.setCache(gemfireCache);
			regionFactoryBean.setClose(false);
			regionFactoryBean.setPersistent(false);

			return regionFactoryBean;
		}

		@Bean("RegionFour")
		PartitionedRegionFactoryBean<Object, Object> regionFour(GemFireCache gemfireCache) {

			PartitionedRegionFactoryBean<Object, Object> regionFactoryBean = new PartitionedRegionFactoryBean<>();

			regionFactoryBean.setCache(gemfireCache);
			regionFactoryBean.setClose(false);
			regionFactoryBean.setPersistent(false);

			return regionFactoryBean;
		}
	}
}
