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

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import javax.annotation.Resource;

import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link EnableSsl} and {@link SslConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
 * @see org.springframework.data.gemfire.config.annotation.EnableSsl
 * @see org.springframework.data.gemfire.config.annotation.SslConfiguration
 * @since 2.1.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = EnableSslConfigurationIntegrationTests.ClientTestConfiguration.class)
@SuppressWarnings("all")
public class EnableSslConfigurationIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static final String LOG_LEVEL = "error";

	private static ProcessWrapper gemfireServer;

	@Resource(name = "Echo")
	private Region<String, String> echo;

	@BeforeClass
	public static void setupGemFireServer() throws Exception {

		String hostname = "localhost";

		int availablePort = findAvailablePort();

		gemfireServer = run(ServerTestConfiguration.class,
			String.format("-Dgemfire.name=%s", asApplicationName(EnableSslConfigurationIntegrationTests.class)),
			String.format("-Djavax.net.ssl.keyStore=%s", System.getProperty("javax.net.ssl.keyStore")),
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort));

		waitForServerToStart("localhost", availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.format("%s[%d]", hostname, availablePort));
		System.setProperty(GEMFIRE_POOL_SERVERS_PROPERTY, String.format("%s[%d]", hostname, availablePort));
	}

	@AfterClass
	public static void tearDownGemFireServer() {
		stop(gemfireServer);
	}

	@Test
	public void clientServerWithSslEnabledWorks() {
		assertThat(this.echo.get("testing123")).isEqualTo("testing123");
	}

	@ClientCacheApplication(logLevel = LOG_LEVEL)
	@EnableSsl(keystorePassword = "s3cr3t", truststorePassword = "s3cr3t")
	static class ClientTestConfiguration {

		@Bean
		ClientCacheConfigurer clientCacheSslConfigurer(
				@Value("${javax.net.ssl.keyStore:trusted.keystore}") String keystoreLocation) {

			return (beanName, bean) -> {
				bean.getProperties().setProperty("ssl-keystore", keystoreLocation);
				bean.getProperties().setProperty("ssl-truststore", keystoreLocation);
			};
		}

		@Bean("Echo")
		ClientRegionFactoryBean<String, String> echoRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<String, String> echoRegion = new ClientRegionFactoryBean<>();

			echoRegion.setCache(gemfireCache);
			echoRegion.setClose(false);
			echoRegion.setShortcut(ClientRegionShortcut.PROXY);

			return echoRegion;
		}
	}

	@CacheServerApplication(name = "EnableSslConfigurationIntegrationTests", logLevel = LOG_LEVEL)
	@EnableSsl(keystorePassword = "s3cr3t", truststorePassword = "s3cr3t")
	static class ServerTestConfiguration {

		public static void main(String[] args) {

			AnnotationConfigApplicationContext applicationContext =
				new AnnotationConfigApplicationContext(ServerTestConfiguration.class);

			applicationContext.registerShutdownHook();
		}

		@Bean
		PeerCacheConfigurer cacheServerSslConfigurer(
				@Value("${javax.net.ssl.keyStore:trusted.keystore}") String keystoreLocation) {

			return (beanName, bean) -> {
				bean.getProperties().setProperty("ssl-keystore", keystoreLocation);
				bean.getProperties().setProperty("ssl-truststore", keystoreLocation);
			};
		}

		@Bean("Echo")
		PartitionedRegionFactoryBean<String, String> echoRegion(GemFireCache gemfireCache) {

			PartitionedRegionFactoryBean<String, String> echoRegion = new PartitionedRegionFactoryBean<>();

			echoRegion.setCache(gemfireCache);
			echoRegion.setCacheLoader(echoCacheLoader());
			echoRegion.setClose(false);
			echoRegion.setPersistent(false);

			return echoRegion;
		}

		@Bean
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
