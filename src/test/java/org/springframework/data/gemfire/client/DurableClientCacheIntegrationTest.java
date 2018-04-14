/*
 * Copyright 2010-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.client;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.number.OrderingComparison.greaterThanOrEqualTo;
import static org.junit.Assert.assertThat;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EntryEvent;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.util.CacheListenerAdapter;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.AbstractGemFireClientServerIntegrationTest;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.data.gemfire.util.DistributedSystemUtils;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.Assert;
import org.springframework.util.SocketUtils;

/**
 * The DurableClientCacheIntegrationTest class is a test suite of test cases testing GemFire's Durable Client
 * functionality in the context of Spring Data GemFire.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.data.gemfire.process.ProcessWrapper
 * @see AbstractGemFireClientServerIntegrationTest
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.util.CacheListenerAdapter
 * @since 1.6.3
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("all")
public class DurableClientCacheIntegrationTest extends AbstractGemFireClientServerIntegrationTest {

	private static int serverPort;

	private static AtomicBoolean DIRTIES_CONTEXT = new AtomicBoolean(false);

	private static List<Integer> regionCacheListenerEventValues =
		Collections.synchronizedList(new ArrayList<Integer>());

	private static ProcessWrapper serverProcess;

	private static final String CACHE_SERVER_PORT_SYSTEM_PROPERTY =
		DurableClientCacheIntegrationTest.class.getName().concat(".cache-server-port");

	private static final String CLIENT_CACHE_INTERESTS_RESULT_POLICY_SYSTEM_PROPERTY =
		DurableClientCacheIntegrationTest.class.getName().concat(".interests-result-policy");

	private static final String DURABLE_CLIENT_TIMEOUT_SYSTEM_PROPERTY =
		DurableClientCacheIntegrationTest.class.getName().concat(".durable-client-timeout");

	private static final String SERVER_HOST = "localhost";

	@Autowired
	private ConfigurableApplicationContext applicationContext;

	@Autowired
	private ClientCache clientCache;

	@Resource(name = "Example")
	private Region<String, Integer> example;

	@BeforeClass
	public static void startGemFireServer() throws IOException {
		serverPort = setSystemProperty(CACHE_SERVER_PORT_SYSTEM_PROPERTY, SocketUtils.findAvailableTcpPort());
		serverProcess = startGemFireServer(DurableClientCacheIntegrationTest.class);
	}

	@AfterClass
	public static void stopGemFireServer() {
		stopGemFireServer(serverProcess);
		clearSystemProperties(DurableClientCacheIntegrationTest.class);
	}

	private static boolean isAfterDirtiesContext() {
		return DIRTIES_CONTEXT.get();
	}

	private static boolean isBeforeDirtiesContext() {
		return !isAfterDirtiesContext();
	}

	private boolean dirtiesContext() {
		return !DIRTIES_CONTEXT.getAndSet(true);
	}

	private <T> T valueBeforeAndAfterDirtiesContext(T before, T after) {
		return (isBeforeDirtiesContext() ? before : after);
	}

	@Before
	public void setup() {
		Properties distributedSystemProperties = clientCache.getDistributedSystem().getProperties();

		assertThat(distributedSystemProperties.getProperty(
			DistributedSystemUtils.DURABLE_CLIENT_ID_PROPERTY_NAME),
				is(equalTo(DurableClientCacheIntegrationTest.class.getSimpleName())));

		assertThat(clientCache.getDistributedSystem().getProperties().getProperty(
			DistributedSystemUtils.DURABLE_CLIENT_TIMEOUT_PROPERTY_NAME),
				is(equalTo(valueBeforeAndAfterDirtiesContext("300", "600"))));

		assertRegion(example, "Example", DataPolicy.NORMAL);
	}

	@After
	public void tearDown() {

		if (dirtiesContext()) {
			closeApplicationContext();
			runClientCacheProducer();
			setSystemProperties();
		}

		regionCacheListenerEventValues.clear();
	}

	protected void closeApplicationContext() {

		applicationContext.close();

		assertThat(applicationContext.isRunning(), is(false));
		assertThat(applicationContext.isActive(), is(false));
	}

	protected void runClientCacheProducer() {

		try {
			ClientCache gemfireClientCache = new ClientCacheFactory()
				.addPoolServer(SERVER_HOST, serverPort)
				.set("name", "ClientCacheProducer")
				.set("mcast-port", "0")
				.set("log-level", "warning")
				.create();

			Region<String, Integer> exampleRegion = gemfireClientCache.<String, Integer>createClientRegionFactory(
				ClientRegionShortcut.PROXY).create("Example");

			exampleRegion.put("four", 4);
			exampleRegion.put("five", 5);
		}
		finally {
			GemfireUtils.closeClientCache();
		}
	}

	private void setSystemProperties() {
		System.setProperty(CLIENT_CACHE_INTERESTS_RESULT_POLICY_SYSTEM_PROPERTY, InterestResultPolicyType.NONE.name());
		System.setProperty(DURABLE_CLIENT_TIMEOUT_SYSTEM_PROPERTY, "600");
	}

	private void assertRegion(Region<?, ?> region, String expectedName, DataPolicy expectedDataPolicy) {
		assertRegion(region, expectedName, String.format("%1$s%2$s", Region.SEPARATOR, expectedName),
			expectedDataPolicy);
	}

	private void assertRegion(Region<?, ?> region, String expectedName, String expectedPath,
			DataPolicy expectedDataPolicy) {

		assertThat(region, is(notNullValue()));
		assertThat(region.getName(), is(equalTo(expectedName)));
		assertThat(region.getFullPath(), is(equalTo(expectedPath)));
		assertThat(region.getAttributes(), is(notNullValue()));
		assertThat(region.getAttributes().getDataPolicy(), is(equalTo(expectedDataPolicy)));
	}

	private void assertRegionValues(Region<?, ?> region, Object... values) {

		assertThat(region.size(), is(equalTo(values.length)));

		for (Object value : values) {
			assertThat(region.containsValue(value), is(true));
		}
	}

	private void waitForRegionEntryEvents() {
		ThreadUtils.timedWait(TimeUnit.SECONDS.toMillis(5), TimeUnit.MILLISECONDS.toMillis(500),
			new ThreadUtils.WaitCondition() {
				@Override public boolean waiting() {
					return (regionCacheListenerEventValues.size() < 2);
				}
			}
		);
	}

	@Test
	@DirtiesContext
	public void durableClientGetsInitializedWithDataOnServer() {

		assumeTrue(isBeforeDirtiesContext());
		assertRegionValues(example, 1, 2, 3);
		assertThat(regionCacheListenerEventValues.isEmpty(), is(true));
	}

	@Test
	public void durableClientGetsUpdatesFromServerWhileClientWasOffline() {

		assumeTrue(isAfterDirtiesContext());
		assertThat(example.isEmpty(), is(true));

		waitForRegionEntryEvents();

		assertThat(regionCacheListenerEventValues.size(), is(equalTo(2)));
		assertThat(regionCacheListenerEventValues, is(equalTo(Arrays.asList(4, 5))));
		assertThat(example.isEmpty(), is(true));
	}

	public static class ClientCacheBeanPostProcessor implements BeanPostProcessor {

		@Override
		public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
			return bean;
		}

		@Override
		public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {

			if (bean instanceof Pool && "gemfireServerPool".equals(beanName)) {

				Pool gemfireServerPool = (Pool) bean;

				if (isBeforeDirtiesContext()) {
					// NOTE: A value of -2 indicates the client connected to the server for the first time...
					assertThat(gemfireServerPool.getPendingEventCount(), is(equalTo(-2)));
				}
				else {
					// NOTE: the pending event count could be 3 because it should minimally include the 2 puts
					// from the client cache producer and possibly a "marker" as well...
					assertThat(gemfireServerPool.getPendingEventCount(), is(greaterThanOrEqualTo(2)));
				}
			}

			return bean;
		}
	}

	public static class RegionDataLoadingBeanPostProcessor<K, V> implements BeanPostProcessor {

		private Map<K, V> regionData;

		private final String regionName;

		public RegionDataLoadingBeanPostProcessor(String regionName) {
			Assert.hasText(regionName, "Region name must be specified");
			this.regionName = regionName;
		}

		public void setRegionData(Map<K, V> regionData) {
			this.regionData = regionData;
		}

		protected Map<K, V> getRegionData() {
			Assert.state(regionData != null, "Region data was not properly initialized");
			return regionData;
		}

		protected String getRegionName() {
			return regionName;
		}

		protected void loadData(Region<K, V> region) {
			region.putAll(getRegionData());
		}

		@Override
		public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
			return bean;
		}

		@Override
		public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
			if (bean instanceof Region) {
				Region<K, V> region = (Region) bean;

				if (getRegionName().equals(region.getName())) {
					loadData(region);
				}
			}

			return bean;
		}
	}

	public static class RegionEntryEventRecordingCacheListener extends CacheListenerAdapter<String, Integer> {

		@Override
		public void afterCreate(EntryEvent<String, Integer> event) {
			regionCacheListenerEventValues.add(event.getNewValue());
		}
	}
}
