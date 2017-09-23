/*
 * Copyright 2017 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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

import java.util.Optional;
import java.util.Properties;

import org.apache.geode.cache.GemFireCache;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.mock.env.MockPropertySource;

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

	private ConfigurableApplicationContext newApplicationContext(PropertySource<?> testPropertySource,
		Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();

		MutablePropertySources propertySources = applicationContext.getEnvironment().getPropertySources();

		propertySources.addFirst(testPropertySource);

		applicationContext.registerShutdownHook();
		applicationContext.register(annotatedClasses);
		applicationContext.refresh();

		return applicationContext;
	}

	@Test
	public void authGemFirePropertiesConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
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

		this.applicationContext = newApplicationContext(testPropertySource, TestAuthGemFirePropertiesConfiguration.class);

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

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.service.http.bind-address", "10.128.64.32")
			.withProperty("spring.data.gemfire.service.http.port", "8181")
			.withProperty("spring.data.gemfire.service.http.ssl-require-authentication", "true")
			.withProperty("spring.data.gemfire.service.http.dev-rest-api.start", "true");

		this.applicationContext = newApplicationContext(testPropertySource, TestHttpGemFirePropertiesConfiguration.class);

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

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.locator.host", "10.64.32.16")
			.withProperty("spring.data.gemfire.locator.port", "11235");

		this.applicationContext = newApplicationContext(testPropertySource, TestLocatorGemFirePropertiesConfiguration.class);

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

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.logging.log-disk-space-limit", "100")
			.withProperty("spring.data.gemfire.logging.log-file", "/path/to/file.log")
			.withProperty("spring.data.gemfire.logging.log-file-size-limit", "10")
			.withProperty("spring.data.gemfire.logging.level", "info");

		this.applicationContext = newApplicationContext(testPropertySource, TestLoggingGemFirePropertiesConfiguration.class);

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

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.manager.access-file", "/path/to/access.control")
			.withProperty("spring.data.gemfire.manager.bind-address", "10.32.16.8")
			.withProperty("spring.data.gemfire.manager.hostname-for-clients", "skullbox")
			.withProperty("spring.data.gemfire.manager.password-file", "/path/to/password.dat")
			.withProperty("spring.data.gemfire.manager.port", "1199")
			.withProperty("spring.data.gemfire.manager.start", "true")
			.withProperty("spring.data.gemfire.manager.update-rate", "1000");

		this.applicationContext = newApplicationContext(testPropertySource, TestManagerGemFirePropertiesConfiguration.class);

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

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.service.memcached.port", "2468")
			.withProperty("spring.data.gemfire.service.memcached.protocol", "BINARY");

		this.applicationContext = newApplicationContext(testPropertySource, TestMemcachedServerGemFirePropertiesConfiguration.class);

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
	public void offHeapGemFirePropertiesConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.cache.off-heap-memory-size", "1024g");

		this.applicationContext = newApplicationContext(testPropertySource, TestOffHeapGemFirePropertiesConfiguration.class);

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
	public void redisServerGemFirePropertiesConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.service.redis.bind-address", "10.16.8.4")
			.withProperty("spring.data.gemfire.service.redis.port", "13579");

		this.applicationContext = newApplicationContext(testPropertySource, TestRedisServerGemFirePropertiesConfiguration.class);

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

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.security.client.authentication-initializer", "example.security.client.AuthenticationInitializer")
			.withProperty("spring.data.gemfire.security.peer.authentication-initializer", "example.security.peer.AuthenticationInitializer")
			.withProperty("spring.data.gemfire.security.manager.class-name", "example.security.SecurityManager")
			.withProperty("spring.data.gemfire.security.postprocessor.class-name", "example.security.PostProcessor")
			.withProperty("spring.data.gemfire.security.shiro.ini-resource-path", "/path/to/shiro.ini");

		this.applicationContext = newApplicationContext(testPropertySource, TestSecurityGemFirePropertiesConfiguration.class);

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
	public void sslGemFirePropertiesConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.security.ssl.ciphers", "DSA")
			.withProperty("spring.data.gemfire.security.ssl.keystore", "/path/to/keystore")
			.withProperty("spring.data.gemfire.security.ssl.keystore-password", "p@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.keystore-type", "JKS")
			.withProperty("spring.data.gemfire.security.ssl.protocols", "TLS")
			.withProperty("spring.data.gemfire.security.ssl.require-authentication", "false")
			.withProperty("spring.data.gemfire.security.ssl.truststore", "/path/to/truststore")
			.withProperty("spring.data.gemfire.security.ssl.truststore-password", "p@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.cluster.ciphers", "RSA")
			.withProperty("spring.data.gemfire.security.ssl.cluster.keystore", "/path/to/cluster/keystore")
			.withProperty("spring.data.gemfire.security.ssl.cluster.keystore-password", "clusterP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.cluster.keystore-type", "PKCS11")
			.withProperty("spring.data.gemfire.security.ssl.cluster.protocols", "TLS")
			.withProperty("spring.data.gemfire.security.ssl.cluster.require-authentication", "true")
			.withProperty("spring.data.gemfire.security.ssl.cluster.truststore", "/path/to/cluster/truststore")
			.withProperty("spring.data.gemfire.security.ssl.cluster.truststore-password", "clusterP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.gateway.ciphers", "AES")
			.withProperty("spring.data.gemfire.security.ssl.gateway.keystore", "/path/to/gateway/keystore")
			.withProperty("spring.data.gemfire.security.ssl.gateway.keystore-password", "gatewayP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.gateway.keystore-type", "PKCS11")
			.withProperty("spring.data.gemfire.security.ssl.gateway.protocols", "TLS")
			.withProperty("spring.data.gemfire.security.ssl.gateway.require-authentication", "true")
			.withProperty("spring.data.gemfire.security.ssl.gateway.truststore", "/path/to/gateway/truststore")
			.withProperty("spring.data.gemfire.security.ssl.gateway.truststore-password", "gatewayP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.http-service.ciphers", "DES")
			.withProperty("spring.data.gemfire.security.ssl.http-service.keystore", "/path/to/http/keystore")
			.withProperty("spring.data.gemfire.security.ssl.http-service.keystore-password", "httpP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.http-service.keystore-type", "JKS")
			.withProperty("spring.data.gemfire.security.ssl.http-service.protocols", "TLS")
			.withProperty("spring.data.gemfire.security.ssl.http-service.require-authentication", "true")
			.withProperty("spring.data.gemfire.security.ssl.http-service.truststore", "/path/to/http/truststore")
			.withProperty("spring.data.gemfire.security.ssl.http-service.truststore-password", "httpP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.jmx-manager.ciphers", "RC4")
			.withProperty("spring.data.gemfire.security.ssl.jmx-manager.keystore", "/path/to/jmx/keystore")
			.withProperty("spring.data.gemfire.security.ssl.jmx-manager.keystore-password", "jmxP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.jmx-manager.keystore-type", "PKCS11")
			.withProperty("spring.data.gemfire.security.ssl.jmx-manager.protocols", "TLS")
			.withProperty("spring.data.gemfire.security.ssl.jmx-manager.require-authentication", "true")
			.withProperty("spring.data.gemfire.security.ssl.jmx-manager.truststore", "/path/to/jmx/truststore")
			.withProperty("spring.data.gemfire.security.ssl.jmx-manager.truststore-password", "jmxP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.locator.ciphers", "IDEA")
			.withProperty("spring.data.gemfire.security.ssl.locator.keystore", "/path/to/locator/keystore")
			.withProperty("spring.data.gemfire.security.ssl.locator.keystore-password", "locatorP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.locator.keystore-type", "PKCS11")
			.withProperty("spring.data.gemfire.security.ssl.locator.protocols", "TLS")
			.withProperty("spring.data.gemfire.security.ssl.locator.require-authentication", "true")
			.withProperty("spring.data.gemfire.security.ssl.locator.truststore", "/path/to/locator/truststore")
			.withProperty("spring.data.gemfire.security.ssl.locator.truststore-password", "locatorP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.server.ciphers", "TSA")
			.withProperty("spring.data.gemfire.security.ssl.server.keystore", "/path/to/server/keystore")
			.withProperty("spring.data.gemfire.security.ssl.server.keystore-password", "serverP@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.server.keystore-type", "PKCS11")
			.withProperty("spring.data.gemfire.security.ssl.server.protocols", "TLS")
			.withProperty("spring.data.gemfire.security.ssl.server.require-authentication", "true")
			.withProperty("spring.data.gemfire.security.ssl.server.truststore", "/path/to/server/truststore")
			.withProperty("spring.data.gemfire.security.ssl.server.truststore-password", "serverP@55w0rd");

		this.applicationContext = newApplicationContext(testPropertySource, TestSslGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.contains("ssl-ciphers")).isFalse();
		assertThat(gemfireProperties.contains("ssl-keystore")).isFalse();
		assertThat(gemfireProperties.contains("ssl-keystore-password")).isFalse();
		assertThat(gemfireProperties.contains("ssl-keystore-type")).isFalse();
		assertThat(gemfireProperties.contains("ssl-protocols")).isFalse();
		assertThat(gemfireProperties.contains("ssl-require-authentication")).isFalse();
		assertThat(gemfireProperties.contains("ssl-truststore")).isFalse();
		assertThat(gemfireProperties.contains("ssl-truststore-password")).isFalse();
		assertThat(gemfireProperties.getProperty("cluster-ssl-ciphers")).isEqualTo("RSA");
		assertThat(gemfireProperties.getProperty("cluster-ssl-keystore")).isEqualTo("/path/to/cluster/keystore");
		assertThat(gemfireProperties.getProperty("cluster-ssl-keystore-password")).isEqualTo("clusterP@55w0rd");
		assertThat(gemfireProperties.getProperty("cluster-ssl-keystore-type")).isEqualTo("PKCS11");
		assertThat(gemfireProperties.getProperty("cluster-ssl-protocols")).isEqualTo("TLS");
		assertThat(gemfireProperties.getProperty("cluster-ssl-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("cluster-ssl-truststore")).isEqualTo("/path/to/cluster/truststore");
		assertThat(gemfireProperties.getProperty("cluster-ssl-truststore-password")).isEqualTo("clusterP@55w0rd");
		assertThat(gemfireProperties.getProperty("gateway-ssl-ciphers")).isEqualTo("AES");
		assertThat(gemfireProperties.getProperty("gateway-ssl-keystore")).isEqualTo("/path/to/gateway/keystore");
		assertThat(gemfireProperties.getProperty("gateway-ssl-keystore-password")).isEqualTo("gatewayP@55w0rd");
		assertThat(gemfireProperties.getProperty("gateway-ssl-keystore-type")).isEqualTo("PKCS11");
		assertThat(gemfireProperties.getProperty("gateway-ssl-protocols")).isEqualTo("TLS");
		assertThat(gemfireProperties.getProperty("gateway-ssl-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("gateway-ssl-truststore")).isEqualTo("/path/to/gateway/truststore");
		assertThat(gemfireProperties.getProperty("gateway-ssl-truststore-password")).isEqualTo("gatewayP@55w0rd");
		assertThat(gemfireProperties.getProperty("http-service-ssl-ciphers")).isEqualTo("DES");
		assertThat(gemfireProperties.getProperty("http-service-ssl-keystore")).isEqualTo("/path/to/http/keystore");
		assertThat(gemfireProperties.getProperty("http-service-ssl-keystore-password")).isEqualTo("httpP@55w0rd");
		assertThat(gemfireProperties.getProperty("http-service-ssl-keystore-type")).isEqualTo("JKS");
		assertThat(gemfireProperties.getProperty("http-service-ssl-protocols")).isEqualTo("TLS");
		assertThat(gemfireProperties.getProperty("http-service-ssl-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("http-service-ssl-truststore")).isEqualTo("/path/to/http/truststore");
		assertThat(gemfireProperties.getProperty("http-service-ssl-truststore-password")).isEqualTo("httpP@55w0rd");
		assertThat(gemfireProperties.getProperty("jmx-manager-ssl-ciphers")).isEqualTo("RC4");
		assertThat(gemfireProperties.getProperty("jmx-manager-ssl-keystore")).isEqualTo("/path/to/jmx/keystore");
		assertThat(gemfireProperties.getProperty("jmx-manager-ssl-keystore-password")).isEqualTo("jmxP@55w0rd");
		assertThat(gemfireProperties.getProperty("jmx-manager-ssl-keystore-type")).isEqualTo("PKCS11");
		assertThat(gemfireProperties.getProperty("jmx-manager-ssl-protocols")).isEqualTo("TLS");
		assertThat(gemfireProperties.getProperty("jmx-manager-ssl-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("jmx-manager-ssl-truststore")).isEqualTo("/path/to/jmx/truststore");
		assertThat(gemfireProperties.getProperty("jmx-manager-ssl-truststore-password")).isEqualTo("jmxP@55w0rd");
		assertThat(gemfireProperties.getProperty("locator-ssl-ciphers")).isEqualTo("IDEA");
		assertThat(gemfireProperties.getProperty("locator-ssl-keystore")).isEqualTo("/path/to/locator/keystore");
		assertThat(gemfireProperties.getProperty("locator-ssl-keystore-password")).isEqualTo("locatorP@55w0rd");
		assertThat(gemfireProperties.getProperty("locator-ssl-keystore-type")).isEqualTo("PKCS11");
		assertThat(gemfireProperties.getProperty("locator-ssl-protocols")).isEqualTo("TLS");
		assertThat(gemfireProperties.getProperty("locator-ssl-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("locator-ssl-truststore")).isEqualTo("/path/to/locator/truststore");
		assertThat(gemfireProperties.getProperty("locator-ssl-truststore-password")).isEqualTo("locatorP@55w0rd");
		assertThat(gemfireProperties.getProperty("server-ssl-ciphers")).isEqualTo("TSA");
		assertThat(gemfireProperties.getProperty("server-ssl-keystore")).isEqualTo("/path/to/server/keystore");
		assertThat(gemfireProperties.getProperty("server-ssl-keystore-password")).isEqualTo("serverP@55w0rd");
		assertThat(gemfireProperties.getProperty("server-ssl-keystore-type")).isEqualTo("PKCS11");
		assertThat(gemfireProperties.getProperty("server-ssl-protocols")).isEqualTo("TLS");
		assertThat(gemfireProperties.getProperty("server-ssl-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("server-ssl-truststore")).isEqualTo("/path/to/server/truststore");
		assertThat(gemfireProperties.getProperty("server-ssl-truststore-password")).isEqualTo("serverP@55w0rd");
	}

	@Test
	public void inheritedSslGemFirePropertiesConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.security.ssl.ciphers", "DSA")
			.withProperty("spring.data.gemfire.security.ssl.keystore", "/path/to/keystore")
			.withProperty("spring.data.gemfire.security.ssl.keystore-password", "p@55w0rd")
			.withProperty("spring.data.gemfire.security.ssl.keystore-type", "JKS")
			.withProperty("spring.data.gemfire.security.ssl.protocols", "TLS")
			.withProperty("spring.data.gemfire.security.ssl.require-authentication", "false")
			.withProperty("spring.data.gemfire.security.ssl.truststore", "/path/to/truststore")
			.withProperty("spring.data.gemfire.security.ssl.truststore-password", "p@55w0rd");

		this.applicationContext = newApplicationContext(testPropertySource, TestInheritedSslGemFirePropertiesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();

		Properties gemfireProperties = gemfireCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.contains("ssl-ciphers")).isFalse();
		assertThat(gemfireProperties.contains("ssl-keystore")).isFalse();
		assertThat(gemfireProperties.contains("ssl-keystore-password")).isFalse();
		assertThat(gemfireProperties.contains("ssl-keystore-type")).isFalse();
		assertThat(gemfireProperties.contains("ssl-protocols")).isFalse();
		assertThat(gemfireProperties.contains("ssl-require-authentication")).isFalse();
		assertThat(gemfireProperties.contains("ssl-truststore")).isFalse();
		assertThat(gemfireProperties.contains("ssl-truststore-password")).isFalse();

		for (EnableSsl.Component component : asArray(EnableSsl.Component.CLUSTER, EnableSsl.Component.HTTP)) {
			assertThat(gemfireProperties.contains(String.format("%s-ssl-ciphers", component))).isFalse();
			assertThat(gemfireProperties.contains(String.format("%s-ssl-keystore", component))).isFalse();
			assertThat(gemfireProperties.contains(String.format("%s-ssl-keystore-password", component))).isFalse();
			assertThat(gemfireProperties.contains(String.format("%s-ssl-keystore-type", component))).isFalse();
			assertThat(gemfireProperties.contains(String.format("%s-ssl-protocols", component))).isFalse();
			assertThat(gemfireProperties.contains(String.format("%s-ssl-require-authentication", component))).isFalse();
			assertThat(gemfireProperties.contains(String.format("%s-ssl-truststore", component))).isFalse();
			assertThat(gemfireProperties.contains(String.format("%s-ssl-truststore-password", component))).isFalse();
		}

		for (EnableSsl.Component component : asArray(EnableSsl.Component.GATEWAY, EnableSsl.Component.JMX,
				EnableSsl.Component.LOCATOR, EnableSsl.Component.SERVER)) {

			assertThat(gemfireProperties.getProperty(String.format("%s-ssl-ciphers", component))).isEqualTo("DSA");
			assertThat(gemfireProperties.getProperty(String.format("%s-ssl-keystore", component))).isEqualTo("/path/to/keystore");
			assertThat(gemfireProperties.getProperty(String.format("%s-ssl-keystore-password", component))).isEqualTo("p@55w0rd");
			assertThat(gemfireProperties.getProperty(String.format("%s-ssl-keystore-type", component))).isEqualTo("JKS");
			assertThat(gemfireProperties.getProperty(String.format("%s-ssl-protocols", component))).isEqualTo("TLS");
			assertThat(gemfireProperties.getProperty(String.format("%s-ssl-require-authentication", component))).isEqualTo("false");
			assertThat(gemfireProperties.getProperty(String.format("%s-ssl-truststore", component))).isEqualTo("/path/to/truststore");
			assertThat(gemfireProperties.getProperty(String.format("%s-ssl-truststore-password", component))).isEqualTo("p@55w0rd");
		}
	}

	@Test
	public void statisticsGemFirePropertiesConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.stats.archive-disk-space-limit", "50")
			.withProperty("spring.data.gemfire.stats.archive-file", "/path/to/archive.stats")
			.withProperty("spring.data.gemfire.stats.archive-file-size-limit", "10")
			.withProperty("spring.data.gemfire.stats.enable-time-statistics", "true")
			.withProperty("spring.data.gemfire.stats.sample-rate", "100");

		this.applicationContext = newApplicationContext(testPropertySource, TestStatisticsGemFirePropertiesConfiguration.class);

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
	static class TestAuthGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableHttpService
	static class TestHttpGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableLocator
	static class TestLocatorGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableLogging
	static class TestLoggingGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableManager
	static class TestManagerGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableMemcachedServer
	static class TestMemcachedServerGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableOffHeap(memorySize = "64g")
	static class TestOffHeapGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableRedisServer
	static class TestRedisServerGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableSecurity
	static class TestSecurityGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableSsl(components = { EnableSsl.Component.CLUSTER, EnableSsl.Component.GATEWAY, EnableSsl.Component.HTTP,
							  EnableSsl.Component.JMX, EnableSsl.Component.LOCATOR, EnableSsl.Component.SERVER })
	static class TestSslGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableSsl(components = { EnableSsl.Component.GATEWAY, EnableSsl.Component.JMX, EnableSsl.Component.LOCATOR,
							  EnableSsl.Component.SERVER })
	static class TestInheritedSslGemFirePropertiesConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnableGemFireProperties
	@EnableStatistics
	static class TestStatisticsGemFirePropertiesConfiguration {
	}
}
