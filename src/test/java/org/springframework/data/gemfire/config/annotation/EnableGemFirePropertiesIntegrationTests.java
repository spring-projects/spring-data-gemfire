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
import static org.mockito.Mockito.mock;

import java.util.Optional;
import java.util.Properties;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.pdx.PdxSerializer;

import org.junit.After;
import org.junit.Test;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.mock.env.MockPropertySource;
import org.springframework.util.StringUtils;

/**
 * Integration tests for {@link EnableAuth}, {@link EnableGemFireProperties}, {@link EnableHttpService},
 * {@link EnableLocator}, {@link EnableLogging}, {@link EnableManager}, {@link EnableMemcachedServer},
 * {@link EnableOffHeap}, {@link EnableRedisServer}, {@link EnableSecurity}, {@link EnableSsl},
 * {@link EnableStatistics}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.core.env.PropertySource
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @since 2.0.0
 */
public class EnableGemFirePropertiesIntegrationTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return newApplicationContext(null, annotatedClasses);
	}

	private ConfigurableApplicationContext newApplicationContext(PropertySource<?> testPropertySource,
			Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();

		Optional.ofNullable(testPropertySource).ifPresent(it -> {

			MutablePropertySources propertySources = applicationContext.getEnvironment().getPropertySources();

			propertySources.addFirst(testPropertySource);
		});

		applicationContext.registerShutdownHook();
		applicationContext.register(annotatedClasses);
		applicationContext.refresh();

		return applicationContext;
	}

	@Test
	public void authGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.security.client.accessor", "example.client.AccessController")
			.withProperty("spring.data.gemfire.security.client.accessor-post-processor", "example.client.AccessControllerPostProcessor")
			.withProperty("spring.data.gemfire.security.client.authentication-initializer", "example.client.AuthenticationInitializer")
			.withProperty("spring.data.gemfire.security.client.authenticator", "example.client.Authenticator")
			.withProperty("spring.data.gemfire.security.client.diffie-hellman-algorithm", "SHA1")
			.withProperty("spring.data.gemfire.security.peer.authentication-initializer", "example.peer.AuthenticationInitializer")
			.withProperty("spring.data.gemfire.security.peer.authenticator", "example.peer.Authenticator")
			.withProperty("spring.data.gemfire.security.peer.verify-member-timeout", 120L)
			.withProperty("spring.data.gemfire.security.log.file", "/path/to/security.log")
			.withProperty("spring.data.gemfire.security.log.level", "info")
			.withProperty("spring.data.gemfire.security.properties-file", "/path/to/security.properties");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestAuthGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("gemfireSecurityPropertyFile")).isEqualTo("/path/to/security.properties");
		assertThat(gemfireProperties.getProperty("security-client-accessor")).isEqualTo("example.client.AccessController");
		assertThat(gemfireProperties.getProperty("security-client-accessor-pp")).isEqualTo("example.client.AccessControllerPostProcessor");
		assertThat(gemfireProperties.getProperty("security-client-auth-init")).isEqualTo("example.client.AuthenticationInitializer");
		assertThat(gemfireProperties.getProperty("security-client-authenticator")).isEqualTo("example.client.Authenticator");
		assertThat(gemfireProperties.getProperty("security-client-dhalgo")).isEqualTo("SHA1");
		assertThat(gemfireProperties.getProperty("security-peer-auth-init")).isEqualTo("example.peer.AuthenticationInitializer");
		assertThat(gemfireProperties.getProperty("security-peer-authenticator")).isEqualTo("example.peer.Authenticator");
		assertThat(gemfireProperties.getProperty("security-peer-verifymember-timeout")).isEqualTo("120");
		assertThat(gemfireProperties.getProperty("security-log-file")).isEqualTo("/path/to/security.log");
		assertThat(gemfireProperties.getProperty("security-log-level")).isEqualTo("info");
	}

	@Test
	public void httpGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.service.http.bind-address", "10.128.64.32")
			.withProperty("spring.data.gemfire.service.http.port", "8181")
			.withProperty("spring.data.gemfire.service.http.ssl-require-authentication", "true")
			.withProperty("spring.data.gemfire.service.http.dev-rest-api.start", "true");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestHttpGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("http-service-bind-address")).isEqualTo("10.128.64.32");
		assertThat(gemfireProperties.getProperty("http-service-port")).isEqualTo("8181");
		assertThat(gemfireProperties.getProperty("http-service-ssl-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("start-dev-rest-api")).isEqualTo("true");
	}

	@Test
	public void locatorGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.locator.host", "10.64.32.16")
			.withProperty("spring.data.gemfire.locator.port", "11235");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestLocatorGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("start-locator")).isEqualTo("10.64.32.16[11235]");
	}

	@Test
	public void loggingGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.logging.log-disk-space-limit", "100")
			.withProperty("spring.data.gemfire.logging.log-file", "/path/to/file.log")
			.withProperty("spring.data.gemfire.logging.log-file-size-limit", "10")
			.withProperty("spring.data.gemfire.logging.level", "info");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestLoggingGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("log-disk-space-limit")).isEqualTo("100");
		assertThat(gemfireProperties.getProperty("log-file")).isEqualTo("/path/to/file.log");
		assertThat(gemfireProperties.getProperty("log-file-size-limit")).isEqualTo("10");
		assertThat(gemfireProperties.getProperty("log-level")).isEqualTo("info");
	}

	@Test
	public void managerGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.manager.access-file", "/path/to/access.control")
			.withProperty("spring.data.gemfire.manager.bind-address", "10.32.16.8")
			.withProperty("spring.data.gemfire.manager.hostname-for-clients", "skullbox")
			.withProperty("spring.data.gemfire.manager.password-file", "/path/to/password.dat")
			.withProperty("spring.data.gemfire.manager.port", "1199")
			.withProperty("spring.data.gemfire.manager.start", "true")
			.withProperty("spring.data.gemfire.manager.update-rate", "1000");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestManagerGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("jmx-manager")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("jmx-manager-access-file")).isEqualTo("/path/to/access.control");
		assertThat(gemfireProperties.getProperty("jmx-manager-bind-address")).isEqualTo("10.32.16.8");
		assertThat(gemfireProperties.getProperty("jmx-manager-hostname-for-clients")).isEqualTo("skullbox");
		assertThat(gemfireProperties.getProperty("jmx-manager-password-file")).isEqualTo("/path/to/password.dat");
		assertThat(gemfireProperties.getProperty("jmx-manager-port")).isEqualTo("1199");
		assertThat(gemfireProperties.getProperty("jmx-manager-start")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("jmx-manager-update-rate")).isEqualTo("1000");
	}

	@Test
	public void memcachedServerGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.service.memcached.port", "2468")
			.withProperty("spring.data.gemfire.service.memcached.protocol", "BINARY");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestMemcachedServerGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("memcached-port")).isEqualTo("2468");
		assertThat(gemfireProperties.getProperty("memcached-protocol")).isEqualTo("BINARY");
	}

	@Test
	public void nameAndGroupsAnnotationBasedGemFirePropertiesConfiguration() {

		this.applicationContext =
			newApplicationContext(TestNameAndGroupsAnnotationBasedGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("gemfireProperties")).isTrue();

		Properties gemfireProperties = this.applicationContext.getBean("gemfireProperties", Properties.class);


		// TODO: uncomment when Spring Test for Apache Geode/Pivotal GemFire project replaces
		// the test infrastructure classes in SDG.
		/*
		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();
		*/

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.containsKey("name")).isTrue();
		assertThat(gemfireProperties.getProperty("name")).isEqualTo("TestName");
		assertThat(gemfireProperties.containsKey("groups")).isTrue();
		assertThat(gemfireProperties.getProperty("groups")).isEqualTo("TestGroupOne,TestGroupTwo");
	}

	@Test
	public void offHeapGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.cache.off-heap.memory-size", "1024g");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestOffHeapGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("off-heap-memory-size")).isEqualTo("1024g");
	}

	@Test
	public void pdxGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.pdx.disk-store-name", "TestDiskStore")
			.withProperty("spring.data.gemfire.pdx.ignore-unread-fields", "true")
			.withProperty("spring.data.gemfire.pdx.persistent", "true")
			.withProperty("spring.data.gemfire.pdx.read-serialized", "true")
			.withProperty("spring.data.gemfire.pdx.serializer-bean-name", "mockPdxSerializer");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestPdxGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("mockPdxSerializer")).isTrue();

		CacheFactoryBean gemfireCache = this.applicationContext.getBean("&gemfireCache", CacheFactoryBean.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getPdxDiskStoreName()).isEqualTo("TestDiskStore");
		assertThat(gemfireCache.getPdxIgnoreUnreadFields()).isTrue();
		assertThat(gemfireCache.getPdxPersistent()).isTrue();
		assertThat(gemfireCache.getPdxReadSerialized()).isTrue();

		PdxSerializer mockPdxSerializer = this.applicationContext.getBean("mockPdxSerializer", PdxSerializer.class);

		assertThat(mockPdxSerializer).isNotNull();
		assertThat(gemfireCache.getPdxSerializer()).isEqualTo(mockPdxSerializer);
	}

	@Test
	public void redisServerGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.service.redis.bind-address", "10.16.8.4")
			.withProperty("spring.data.gemfire.service.redis.port", "13579");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestRedisServerGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("redis-bind-address")).isEqualTo("10.16.8.4");
		assertThat(gemfireProperties.getProperty("redis-port")).isEqualTo("13579");
	}

	@Test
	public void securityGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.security.client.authentication-initializer", "example.security.client.AuthenticationInitializer")
			.withProperty("spring.data.gemfire.security.peer.authentication-initializer", "example.security.peer.AuthenticationInitializer")
			.withProperty("spring.data.gemfire.security.manager.class-name", "example.security.SecurityManager")
			.withProperty("spring.data.gemfire.security.postprocessor.class-name", "example.security.PostProcessor")
			.withProperty("spring.data.gemfire.security.shiro.ini-resource-path", "/path/to/shiro.ini");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestSecurityGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("security-client-auth-init")).isEqualTo("example.security.client.AuthenticationInitializer");
		assertThat(gemfireProperties.getProperty("security-peer-auth-init")).isEqualTo("example.security.peer.AuthenticationInitializer");
		assertThat(gemfireProperties.getProperty("security-manager")).isEqualTo("example.security.SecurityManager");
		assertThat(gemfireProperties.getProperty("security-post-processor")).isEqualTo("example.security.PostProcessor");
		assertThat(gemfireProperties.getProperty("security-shiro-init")).isEqualTo("/path/to/shiro.ini");
	}

	@Test
	public void serializableObjectFilterAndValidateSerializableObjectsGemFirePropertiesConfiguration() {

		this.applicationContext =
			newApplicationContext(TestSerializableObjectFilterAndValidateSerializableObjectsGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireProperties")).isTrue();

		Properties gemfireProperties = this.applicationContext.getBean("gemfireProperties", Properties.class);

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.containsKey("serializable-object-filter")).isTrue();
		assertThat(gemfireProperties.getProperty("serializable-object-filter"))
			.isEqualTo("example.app.model.TypeOne,example.app.model.TypeTwo");
		assertThat(gemfireProperties.containsKey("validate-serializable-objects")).isTrue();
		assertThat(gemfireProperties.getProperty("validate-serializable-objects")).isEqualTo("true");
	}

	@Test
	public void sslGemFirePropertiesConfiguration() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.security.ssl.ciphers", "DSA, RSA")
			.withProperty("spring.data.gemfire.security.ssl.certificate.alias.default", "TestCert")
			.withProperty("spring.data.gemfire.security.ssl.keystore", "/path/to/keystore")
			.withProperty("spring.data.gemfire.security.ssl.keystore.password", "p@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.keystore.type", "JKS")
			.withProperty("spring.data.gemfire.security.ssl.protocols", "IP, TCP/IP, UDP")
			.withProperty("spring.data.gemfire.security.ssl.require-authentication", "false")
			.withProperty("spring.data.gemfire.security.ssl.truststore", "/path/to/truststore")
			.withProperty("spring.data.gemfire.security.ssl.truststore.password", "p@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.truststore.type", "PKCS11")
			.withProperty("spring.data.gemfire.security.ssl.web-require-authentication", "true");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestSslGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("gemfireProperties")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		String sslEnabledComponents = Optional.ofNullable(gemfireProperties.getProperty("ssl-enabled-components"))
			.filter(StringUtils::hasText)
			.map(it -> StringUtils.arrayToCommaDelimitedString(ArrayUtils.sort(
				StringUtils.commaDelimitedListToStringArray(it))))
			.orElse(null);

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("ssl-ciphers")).isEqualTo("DSA, RSA");
		assertThat(sslEnabledComponents).isEqualTo("cluster,gateway,jmx,locator,server,web");
		assertThat(gemfireProperties.getProperty("ssl-default-alias")).isEqualTo("TestCert");
		assertThat(gemfireProperties.getProperty("ssl-keystore")).isEqualTo("/path/to/keystore");
		assertThat(gemfireProperties.getProperty("ssl-keystore-password")).isEqualTo("p@55w0rd");
		assertThat(gemfireProperties.getProperty("ssl-keystore-type")).isEqualTo("JKS");
		assertThat(gemfireProperties.getProperty("ssl-protocols")).isEqualTo("IP, TCP/IP, UDP");
		assertThat(gemfireProperties.getProperty("ssl-require-authentication")).isEqualTo("false");
		assertThat(gemfireProperties.getProperty("ssl-truststore")).isEqualTo("/path/to/truststore");
		assertThat(gemfireProperties.getProperty("ssl-truststore-password")).isEqualTo("p@55w0rd");
		assertThat(gemfireProperties.getProperty("ssl-truststore-type")).isEqualTo("PKCS11");
	}

	@Test
	public void statisticsGemFirePropertiesConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.stats.archive-disk-space-limit", "50")
			.withProperty("spring.data.gemfire.stats.archive-file", "/path/to/archive.stats")
			.withProperty("spring.data.gemfire.stats.archive-file-size-limit", "10")
			.withProperty("spring.data.gemfire.stats.enable-time-statistics", "true")
			.withProperty("spring.data.gemfire.stats.sample-rate", "100");

		this.applicationContext =
			newApplicationContext(testPropertySource, TestStatisticsGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("statistic-sampling-enabled")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("archive-disk-space-limit")).isEqualTo("50");
		assertThat(gemfireProperties.getProperty("statistic-archive-file")).isEqualTo("/path/to/archive.stats");
		assertThat(gemfireProperties.getProperty("archive-file-size-limit")).isEqualTo("10");
		assertThat(gemfireProperties.getProperty("enable-time-statistics")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("statistic-sample-rate")).isEqualTo("100");
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableAuth
	@EnableGemFireProperties
	static class TestAuthGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableHttpService
	static class TestHttpGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableLocator
	static class TestLocatorGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableLogging
	static class TestLoggingGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableManager
	static class TestManagerGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableMemcachedServer
	static class TestMemcachedServerGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties(name = "TestName", groups = { "TestGroupOne", "TestGroupTwo" })
	static class TestNameAndGroupsAnnotationBasedGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableOffHeap(memorySize = "64g")
	static class TestOffHeapGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnablePdx
	@SuppressWarnings("unused")
	static class TestPdxGemFirePropertiesConfiguration {

		@Bean
		PdxSerializer mockPdxSerializer() {
			return mock(PdxSerializer.class);
		}
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableRedisServer
	static class TestRedisServerGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableSecurity
	static class TestSecurityGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties(serializableObjectFilter = { "example.app.model.TypeOne", "example.app.model.TypeTwo" },
		validateSerializableObjects = true)
	static class TestSerializableObjectFilterAndValidateSerializableObjectsGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableSsl(ciphers = "FISH", components = {
		EnableSsl.Component.CLUSTER, EnableSsl.Component.GATEWAY, EnableSsl.Component.JMX,
		EnableSsl.Component.LOCATOR, EnableSsl.Component.SERVER, EnableSsl.Component.WEB
	}, componentCertificateAliases = {
		@EnableSsl.ComponentAlias(component = EnableSsl.Component.GATEWAY, alias = "WanCert"),
		@EnableSsl.ComponentAlias(component = EnableSsl.Component.WEB, alias = "HttpCert")
	}, defaultCertificateAlias = "MockCert", protocols = "HTTP")
	static class TestSslGemFirePropertiesConfiguration { }

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableStatistics
	static class TestStatisticsGemFirePropertiesConfiguration { }

}
