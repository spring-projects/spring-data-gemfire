/*
 * Copyright 2016-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.gemfire.process.ProcessExecutor;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.server.CacheServerFactoryBean;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integrations tests for {@link GemfireTemplate} testing proper function and behavior of executing (OQL) queries
 * from a cache client application using the {@link GemfireTemplate} to a cluster of Pivotal GemFire'servers that have
 * been grouped according to business function and data access, primarily to distribute the load.
 *
 * Each Pivotal GemFire{@link Pool} is configured to target a specific server group.  Each group of servers in the cluster
 * defines specific {@link Region Regions} to manage data independently and separately from other data that might
 * garner high frequency access.
 *
 * SDG's {@link GemfireTemplate} should intelligently employ the right
 * {@link org.apache.geode.cache.query.QueryService} configured with the {@link Region Region's} {@link Pool}
 * meta-data when executing the query in order to ensure the right servers containing the {@link Region Region's}
 * with the data of interest are targeted.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.GemfireTemplate
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see <a href="https://jira.spring.io/browse/SGF-555">Repository queries on client Regions associated with a Pool configured with a specified server group can lead to a RegionNotFoundException.</a>
 * @since 1.9.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes =
	GemfireTemplateQueriesOnGroupedPooledClientCacheRegionsIntegrationTests.GemFireClientCacheConfiguration.class)
@SuppressWarnings("unused")
public class GemfireTemplateQueriesOnGroupedPooledClientCacheRegionsIntegrationTests
		extends ClientServerIntegrationTestsSupport{

	private static ProcessWrapper serverOne;
	private static ProcessWrapper serverTwo;

	@Autowired
	@Qualifier("catsTemplate")
	private GemfireTemplate catsTemplate;

	@Autowired
	@Qualifier("dogsTemplate")
	private GemfireTemplate dogsTemplate;

	@BeforeClass
	public static void setupGemFireCluster() throws Exception {
		serverOne = ProcessExecutor.launch(createDirectory("serverOne"), GemFireCacheServerOneConfiguration.class);
		waitForServerToStart("localhost", 41414);
		serverTwo = ProcessExecutor.launch(createDirectory("serverTwo"), GemFireCacheServerTwoConfiguration.class);
		waitForServerToStart("localhost", 42424);
	}

	@AfterClass
	public static void shutdownGemFireCluster() {
		serverOne.stop();
		serverTwo.stop();
	}

	@Test
	public void findsAllCats() {
		List<String> catNames = catsTemplate.<String>find("SELECT c.name FROM /Cats c").asList();

		assertThat(catNames).isNotNull();
		assertThat(catNames.size()).isEqualTo(5);
		assertThat(catNames).containsAll(Arrays.asList("Grey", "Patchit", "Tyger", "Molly", "Sammy"));
	}

	@Test
	public void findsAllDogs() {
		List<String> dogNames = dogsTemplate.<String>find("SELECT d.name FROM /Dogs d").asList();

		assertThat(dogNames).isNotNull();
		assertThat(dogNames.size()).isEqualTo(2);
		assertThat(dogNames).containsAll(Arrays.asList("Spuds", "Maha"));
	}

	@Data
	@Region("Cats")
	@RequiredArgsConstructor(staticName = "newCat")
	static class Cat {
		@Id @NonNull private String name;
	}

	@Data
	@Region("Dogs")
	@RequiredArgsConstructor(staticName = "newDog")
	static class Dog {
		@Id @NonNull private String name;
	}

	@Configuration
	static class GemFireClientCacheConfiguration {

		Properties gemfireProperties() {

			return PropertiesBuilder.create()
				.setProperty("name", applicationName())
				.setProperty("log-level", logLevel())
				.build();
		}

		String applicationName() {
			return GemFireClientCacheConfiguration.class.getName();
		}

		String logLevel() {
			return System.getProperty("spring.data.gemfire.log.level", GEMFIRE_LOG_LEVEL);
		}

		@Bean
		ClientCacheFactoryBean gemfireCache() {

			ClientCacheFactoryBean gemfireCache = new ClientCacheFactoryBean();

			gemfireCache.setClose(true);
			gemfireCache.setPoolName("ServerOnePool");
			gemfireCache.setProperties(gemfireProperties());

			return gemfireCache;
		}

		@Bean(name = "ServerOnePool")
		PoolFactoryBean serverOnePool() {

			PoolFactoryBean serverOnePool = new PoolFactoryBean();

			serverOnePool.setMaxConnections(2);
			serverOnePool.setPingInterval(TimeUnit.SECONDS.toMillis(5));
			serverOnePool.setReadTimeout(Long.valueOf(TimeUnit.SECONDS.toMillis(20)).intValue());
			serverOnePool.setRetryAttempts(1);
			serverOnePool.setServerGroup("serverOne");
			serverOnePool.setLocators(ConnectionEndpointList.from(newConnectionEndpoint("localhost", 11235)));

			return serverOnePool;
		}

		@Bean(name = "ServerTwoPool")
		PoolFactoryBean serverTwoPool() {

			PoolFactoryBean serverOnePool = new PoolFactoryBean();

			serverOnePool.setMaxConnections(2);
			serverOnePool.setPingInterval(TimeUnit.SECONDS.toMillis(5));
			serverOnePool.setReadTimeout(Long.valueOf(TimeUnit.SECONDS.toMillis(20)).intValue());
			serverOnePool.setRetryAttempts(1);
			serverOnePool.setServerGroup("serverTwo");
			serverOnePool.setLocators(ConnectionEndpointList.from(newConnectionEndpoint("localhost", 11235)));

			return serverOnePool;
		}

		@Bean(name = "Cats")
		ClientRegionFactoryBean<String, Cat> catsRegion(GemFireCache gemfireCache,
				@Qualifier("ServerOnePool") Pool serverOnePool) {

			ClientRegionFactoryBean<String, Cat> catsRegion = new ClientRegionFactoryBean<String, Cat>();

			catsRegion.setCache(gemfireCache);
			catsRegion.setClose(false);
			catsRegion.setPoolName(serverOnePool.getName());
			catsRegion.setShortcut(ClientRegionShortcut.PROXY);

			return catsRegion;
		}

		@Bean(name = "Dogs")
		ClientRegionFactoryBean<String, Cat> dogsRegion(GemFireCache gemfireCache,
				@Qualifier("ServerTwoPool") Pool serverTwoPool) {

			ClientRegionFactoryBean<String, Cat> dogsRegion = new ClientRegionFactoryBean<String, Cat>();

			dogsRegion.setCache(gemfireCache);
			dogsRegion.setClose(false);
			dogsRegion.setPoolName(serverTwoPool.getName());
			dogsRegion.setShortcut(ClientRegionShortcut.PROXY);

			return dogsRegion;
		}

		@Bean
		@DependsOn("Cats")
		GemfireTemplate catsTemplate(GemFireCache gemfireCache) {
			return new GemfireTemplate(gemfireCache.getRegion("Cats"));
		}

		@Bean
		@DependsOn("Dogs")
		GemfireTemplate dogsTemplate(GemFireCache gemfireCache) {
			return new GemfireTemplate(gemfireCache.getRegion("Dogs"));
		}

		ConnectionEndpoint newConnectionEndpoint(String host, int port) {
			return new ConnectionEndpoint(host, port);
		}
	}

	static abstract class AbstractGemFireCacheServerConfiguration {

		Properties gemfireProperties() {

			return PropertiesBuilder.create()
				.setProperty("name", applicationName())
				.setProperty("log-level", logLevel())
				.setProperty("locators", "localhost[11235]")
				.setProperty("enable-cluster-configuration", "false")
				.setProperty("groups", groups())
				.setProperty("start-locator", startLocator())
				.setProperty("use-cluster-configuration", "false")
				.build();
		}

		String applicationName() {
			return getClass().getName();
		}

		abstract String groups();

		String logLevel() {
			return System.getProperty("spring.data.gemfire.log.level", GEMFIRE_LOG_LEVEL);
		}

		String startLocator() {
			return "";
		}

		@Bean
		CacheFactoryBean gemfireCache() {

			CacheFactoryBean gemfireCache = new CacheFactoryBean();

			gemfireCache.setClose(true);
			gemfireCache.setProperties(gemfireProperties());

			return gemfireCache;
		}

		@Bean
		CacheServerFactoryBean gemfireCacheServer(GemFireCache gemfireCache) {

			CacheServerFactoryBean gemfireCacheServer = new CacheServerFactoryBean();

			gemfireCacheServer.setAutoStartup(true);
			gemfireCacheServer.setCache((Cache) gemfireCache);
			gemfireCacheServer.setMaxTimeBetweenPings(Long.valueOf(TimeUnit.SECONDS.toMillis(60)).intValue());
			gemfireCacheServer.setPort(cacheServerPort());

			return gemfireCacheServer;
		}

		abstract int cacheServerPort();

	}

	@Configuration
	@SuppressWarnings("all")
	static class GemFireCacheServerOneConfiguration extends AbstractGemFireCacheServerConfiguration {

		@Resource(name = "Cats")
		private org.apache.geode.cache.Region<String, Cat> cats;

		Cat save(Cat cat) {
			cats.put(cat.getName(), cat);
			return cat;
		}

		@PostConstruct
		public void postConstruct() {
			save(Cat.newCat("Grey"));
			save(Cat.newCat("Patchit"));
			save(Cat.newCat("Tyger"));
			save(Cat.newCat("Molly"));
			save(Cat.newCat("Sammy"));
		}

		@Override
		int cacheServerPort() {
			return 41414;
		}

		@Override
		String groups() {
			return "serverOne";
		}

		@Override
		String startLocator() {
			return "localhost[11235]";
		}

		@Bean(name = "Cats")
		LocalRegionFactoryBean catsRegion(GemFireCache gemfireCache) {

			LocalRegionFactoryBean catsRegion = new LocalRegionFactoryBean();

			catsRegion.setCache(gemfireCache);
			catsRegion.setClose(false);
			catsRegion.setPersistent(false);

			return catsRegion;
		}

		public static void main(String[] args) {
			new AnnotationConfigApplicationContext(GemFireCacheServerOneConfiguration.class)
				.registerShutdownHook();
		}
	}

	@Configuration
	@SuppressWarnings("all")
	static class GemFireCacheServerTwoConfiguration extends AbstractGemFireCacheServerConfiguration {

		@Resource(name = "Dogs")
		private org.apache.geode.cache.Region<String, Dog> dogs;

		Dog save(Dog dog) {
			dogs.put(dog.getName(), dog);
			return dog;
		}

		@PostConstruct
		public void postConstruct() {
			save(Dog.newDog("Spuds"));
			save(Dog.newDog("Maha"));
		}

		@Override
		int cacheServerPort() {
			return 42424;
		}

		@Override
		String groups() {
			return "serverTwo";
		}

		@Bean(name = "Dogs")
		LocalRegionFactoryBean<String, Dog> dogsRegion(GemFireCache gemfireCache) {

			LocalRegionFactoryBean<String, Dog> dogsRegion = new LocalRegionFactoryBean<String, Dog>();

			dogsRegion.setCache(gemfireCache);
			dogsRegion.setClose(false);
			dogsRegion.setPersistent(false);

			return dogsRegion;
		}

		public static void main(String[] args) {
			new AnnotationConfigApplicationContext(GemFireCacheServerTwoConfiguration.class)
				.registerShutdownHook();
		}
	}
}
