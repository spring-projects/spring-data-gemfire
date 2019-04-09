/*
 * Copyright 2017-2019 the original author or authors.
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
import static org.springframework.data.gemfire.util.ArrayUtils.asArray;

import java.io.Serializable;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.Resource;

import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.EntryEvent;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.Operation;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.query.CqEvent;
import org.apache.geode.cache.util.CacheListenerAdapter;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.data.gemfire.listener.annotation.ContinuousQuery;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Integration tests for {@link EnableContinuousQueries}, {@link ContinuousQueryConfiguration}, {@link ContinuousQuery}
 * and {@link ContinuousQueryListenerContainer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.query.CqEvent
 * @see org.springframework.data.gemfire.config.annotation.ContinuousQueryConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableContinuousQueries
 * @see org.springframework.data.gemfire.listener.annotation.ContinuousQuery
 * @see org.springframework.data.gemfire.process.ProcessWrapper
 * @see org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = EnableContinuousQueriesConfigurationIntegrationTests.TestConfiguration.class)
@SuppressWarnings("unused")
public class EnableContinuousQueriesConfigurationIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static AtomicInteger boilingTemperatureReadingsCounter = new AtomicInteger(0);
	private static AtomicInteger freezingTemperatureReadingsCounter = new AtomicInteger(0);
	private static AtomicInteger totalTemperatureReadingsCounter = new AtomicInteger(0);

	private static ProcessWrapper gemfireServer;

	@BeforeClass
	public static void startGemFireServer() throws Exception {

		int availablePort = findAvailablePort();

		gemfireServer = run(GemFireServerConfiguration.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort));

		waitForServerToStart("localhost", availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}

	@Resource(name = "TemperatureReadings")
	private Region<Long, TemperatureReading> temperatureReadings;

	@Before
	public void setup() {

		TemperatureReading temperatureReading = this.temperatureReadings.get(1L);

		assertThat(temperatureReading).isNotNull();
		assertThat(temperatureReading.getTemperature()).isEqualTo(99);

		assertThat(this.temperatureReadings.sizeOnServer()).isEqualTo(10);

		//waitOn(() -> totalTemperatureReadingsCounter.get() >= 5, 100L);

		//assertThat(totalTemperatureReadingsCounter.get()).isNotZero();
	}

	@Test
	public void boilingTemperatureReadingsEqualsThree() {
		assertThat(boilingTemperatureReadingsCounter.get()).isEqualTo(3);
	}

	@Test
	public void freezingTemperatureReadingsEqualsTwo() {
		assertThat(freezingTemperatureReadingsCounter.get()).isEqualTo(2);
	}

	@Configuration
	@EnableContinuousQueries
	@Import(GemFireClientConfiguration.class)
	static class TestConfiguration {

		@ContinuousQuery(name = "BoilingTemperatures",
			query = "SELECT * FROM /TemperatureReadings r WHERE r.temperature >= 212")
		public void boilingTemperatures(CqEvent event) {
			boilingTemperatureReadingsCounter.incrementAndGet();
		}

		@ContinuousQuery(name = "FreezingTemperatures",
			query = "SELECT * FROM /TemperatureReadings r WHERE r.temperature <= 32")
		public void freezingTemperatures(CqEvent event) {
			freezingTemperatureReadingsCounter.incrementAndGet();
		}
	}

	@ClientCacheApplication(logLevel = "error", subscriptionEnabled = true)
	static class GemFireClientConfiguration {

		@Bean
		ClientCacheConfigurer clientCachePoolPortConfigurer(
				@Value("${" + GEMFIRE_CACHE_SERVER_PORT_PROPERTY + ":40404}") int port) {

			return (bean, clientCacheFactoryBean) -> clientCacheFactoryBean.setServers(
				Collections.singletonList(new ConnectionEndpoint("localhost", port)));
		}

		@Bean(name = "TemperatureReadings")
		ClientRegionFactoryBean<Long, TemperatureReading> temperatureReadingsRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Long, TemperatureReading> temperatureReadings =
				new ClientRegionFactoryBean<>();

			temperatureReadings.setCache(gemfireCache);
			temperatureReadings.setCacheListeners(asArray(temperatureReadingCounterListener()));
			temperatureReadings.setClose(false);
			temperatureReadings.setShortcut(ClientRegionShortcut.PROXY);

			return temperatureReadings;
		}

		private CacheListener<Long, TemperatureReading> temperatureReadingCounterListener() {

			return new CacheListenerAdapter<Long, TemperatureReading>() {

				@Override
				public void afterCreate(EntryEvent<Long, TemperatureReading> event) {
					if (Operation.LOCAL_LOAD_CREATE.equals(event.getOperation())) {
						totalTemperatureReadingsCounter.incrementAndGet();
					}
				}
			};
		}
	}

	@CacheServerApplication(name = "EnableContinuousQueriesConfigurationIntegrationTests", logLevel = "error")
	static class GemFireServerConfiguration {

		public static void main(String[] args) {

			AnnotationConfigApplicationContext applicationContext =
				new AnnotationConfigApplicationContext(GemFireServerConfiguration.class);

			applicationContext.registerShutdownHook();
		}

		@Bean(name = "TemperatureReadings")
		PartitionedRegionFactoryBean<Long, TemperatureReading> temperatureReadingsRegion(GemFireCache gemfireCache) {

			PartitionedRegionFactoryBean<Long, TemperatureReading> temperatureReadings =
				new PartitionedRegionFactoryBean<>();

			temperatureReadings.setCache(gemfireCache);
			temperatureReadings.setCacheLoader(temperatureReadingsLoader());
			temperatureReadings.setClose(false);
			temperatureReadings.setPersistent(false);

			return temperatureReadings;
		}

		private CacheLoader<Long, TemperatureReading> temperatureReadingsLoader() {

			return new CacheLoader<Long, TemperatureReading>() {

				@Override
				public TemperatureReading load(LoaderHelper<Long, TemperatureReading> helper) throws CacheLoaderException {

					long key = helper.getKey();

					Region<Long, TemperatureReading> temperatureReadings = helper.getRegion();

					recordTemperature(temperatureReadings, ++key, 213);
					recordTemperature(temperatureReadings, ++key, 72);
					recordTemperature(temperatureReadings, ++key, 400);
					recordTemperature(temperatureReadings, ++key, 1024);
					recordTemperature(temperatureReadings, ++key, 43);
					recordTemperature(temperatureReadings, ++key, 0);
					recordTemperature(temperatureReadings, ++key, 33);
					recordTemperature(temperatureReadings, ++key, -45);
					recordTemperature(temperatureReadings, ++key, 67);

					return TemperatureReading.newTemperatureReading(99);
				}

				private void recordTemperature(Region<Long, TemperatureReading> temperatureReadings,
						long key, int temperature) {

					sleep(50);
					temperatureReadings.put(key, TemperatureReading.newTemperatureReading(temperature));
				}

				private void sleep(long milliseconds) {
					try {
						Thread.sleep(milliseconds);
					}
					catch (InterruptedException ignore) {
					}
				}

				@Override
				public void close() {
				}
			};
		}
	}

	@Data
	@RequiredArgsConstructor(staticName = "newTemperatureReading")
	public static class TemperatureReading implements Serializable {

		@NonNull
		private Integer temperature;

		@Override
		public String toString() {
			return String.format("%d °F", getTemperature());
		}
	}
}
