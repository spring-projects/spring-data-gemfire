/*
 * Copyright 2018-2020 the original author or authors.
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

import java.util.Optional;
import java.util.Properties;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCache;

import org.junit.After;
import org.junit.Test;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.mock.env.MockPropertySource;
import org.springframework.util.StringUtils;

/**
 * Integration tests for {@link EnableSsl} and {@link SslConfiguration}.
 *
 * Integration tests in this class create a {@link ClientCache} or peer {@link Cache} and query Apache Geode
 * to see if the GemFire properties set with spring-data-geode are correctly set in Apache Geode.
 *
 * @author Srikanth Manvi
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.core.env.PropertySource
 * @see org.springframework.data.gemfire.config.annotation.EnableSsl
 * @see org.springframework.data.gemfire.config.annotation.SslConfiguration
 * @since 2.2.0
 */
public class EnableSslConfigurationDefaultContextIntegrationTests {

	private static final String GEMFIRE_LOG_LEVEL = "error";

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {

		Optional.ofNullable(this.applicationContext)
			.ifPresent(ConfigurableApplicationContext::close);
	}

	private void assertGemFirePropertiesCorrectlySet(Properties gemfireProperties) {

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("ssl-ciphers")).isEqualTo("FISH Scream SEAL SNOW");
		assertThat(gemfireProperties.getProperty("ssl-enabled-components").equals("server,gateway")
				|| gemfireProperties.getProperty("ssl-enabled-components").equals("gateway,server")).isTrue();
		assertThat(gemfireProperties.getProperty("ssl-default-alias")).isEqualTo("TestCert");
		assertThat(gemfireProperties.getProperty("ssl-gateway-alias")).isEqualTo("WanCert");
		assertThat(gemfireProperties.getProperty("ssl-keystore")).isEqualTo("/path/to/keystore.jks");
		assertThat(gemfireProperties.getProperty("ssl-keystore-password")).isEqualTo("s3cr3t!");
		assertThat(gemfireProperties.getProperty("ssl-keystore-type")).isEqualTo("JKS");
		assertThat(gemfireProperties.getProperty("ssl-protocols")).isEqualTo("TCP/IP HTTP");
		assertThat(gemfireProperties.getProperty("ssl-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("ssl-truststore")).isEqualTo("/path/to/truststore.jks");
		assertThat(gemfireProperties.getProperty("ssl-truststore-password")).isEqualTo("p@55w0rd!");
		assertThat(gemfireProperties.getProperty("ssl-truststore-type")).isEqualTo("PKCS11");
		assertThat(gemfireProperties.getProperty("ssl-web-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("ssl-use-default-context")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("ssl-endpoint-identification-enabled")).isEqualTo("true");
	}

	private ConfigurableApplicationContext newApplicationContext(PropertySource<?> testPropertySource,
			Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();

		MutablePropertySources propertySources = applicationContext.getEnvironment().getPropertySources();

		propertySources.addFirst(testPropertySource);

		applicationContext.register(annotatedClasses);
		applicationContext.registerShutdownHook();
		applicationContext.refresh();

		return applicationContext;
	}

	private PropertySource setSpringDataGemFireProperties() {

		return new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.security.ssl.ciphers", "Scream,SEAL,SNOW")
			.withProperty("spring.data.gemfire.security.ssl.components", "locator, server, gateway")
			.withProperty("spring.data.gemfire.security.ssl.certificate.alias.default", "MockCert")
			.withProperty("spring.data.gemfire.security.ssl.certificate.alias.gateway", "WanCert")
			.withProperty("spring.data.gemfire.security.ssl.certificate.alias.server", "ServerCert")
			.withProperty("spring.data.gemfire.security.ssl.enable-endpoint-identification", "true")
			.withProperty("spring.data.gemfire.security.ssl.keystore", "~/test/app/keystore.jks")
			.withProperty("spring.data.gemfire.security.ssl.keystore.password", "0p3nS@y5M3")
			.withProperty("spring.data.gemfire.security.ssl.keystore.type", "R2D2")
			.withProperty("spring.data.gemfire.security.ssl.protocols", "IP,TCP/IP,UDP")
			.withProperty("spring.data.gemfire.security.ssl.require-authentication", "false")
			.withProperty("spring.data.gemfire.security.ssl.truststore", "relative/path/to/trusted.keystore")
			.withProperty("spring.data.gemfire.security.ssl.truststore.password", "kn0ckKn0ck")
			.withProperty("spring.data.gemfire.security.ssl.truststore.type", "C3PO")
			.withProperty("spring.data.gemfire.security.ssl.web-require-authentication", "true")
			.withProperty("spring.data.gemfire.security.ssl.use-default-context", "true");
	}

	@Test
	public void sslAnnotationBasedClientConfigurationIsCorrect() {

		this.applicationContext = newApplicationContext(new MockPropertySource("TestPropertySource"),
			SslAnnotationBasedClientConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache"));
		assertThat(this.applicationContext.containsBean("gemfireProperties"));

		GemFireCache clientCache =
				this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		//Get Properties from GemFire
		Properties gemfireProperties = clientCache.getDistributedSystem().getProperties();

		assertThat(clientCache).isNotNull();

		assertGemFirePropertiesCorrectlySet(gemfireProperties);
	}

	@Test
	public void sslPropertyBasedClientConfigurationIsCorrect() {

		PropertySource testPropertySource = setSpringDataGemFireProperties();

		this.applicationContext = newApplicationContext(testPropertySource, SslPropertyBasedClientConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache"));
		assertThat(this.applicationContext.containsBean("gemfireProperties"));

		GemFireCache clientCache =
				this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(clientCache).isNotNull();

		Properties gemfireProperties = clientCache.getDistributedSystem().getProperties();

		String sslEnabledComponents = Optional.ofNullable(gemfireProperties.getProperty("ssl-enabled-components"))
			.filter(StringUtils::hasText)
			.map(it -> StringUtils.arrayToCommaDelimitedString(
				ArrayUtils.sort(StringUtils.commaDelimitedListToStringArray(it))))
			.orElse(null);

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("ssl-ciphers")).isEqualTo("Scream SEAL SNOW");
		assertThat(sslEnabledComponents).isEqualTo("gateway,locator,server");
		assertThat(gemfireProperties.getProperty("ssl-default-alias")).isEqualTo("MockCert");
		assertThat(gemfireProperties.getProperty("ssl-gateway-alias")).isEqualTo("WanCert");
		assertThat(gemfireProperties.getProperty("ssl-server-alias")).isEqualTo("ServerCert");
		assertThat(gemfireProperties.getProperty("ssl-endpoint-identification-enabled")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("ssl-keystore")).isEqualTo("~/test/app/keystore.jks");
		assertThat(gemfireProperties.getProperty("ssl-keystore-password")).isEqualTo("0p3nS@y5M3");
		assertThat(gemfireProperties.getProperty("ssl-keystore-type")).isEqualTo("R2D2");
		assertThat(gemfireProperties.getProperty("ssl-protocols")).isEqualTo("IP TCP/IP UDP");
		assertThat(gemfireProperties.getProperty("ssl-require-authentication")).isEqualTo("false");
		assertThat(gemfireProperties.getProperty("ssl-truststore")).isEqualTo("relative/path/to/trusted.keystore");
		assertThat(gemfireProperties.getProperty("ssl-truststore-password")).isEqualTo("kn0ckKn0ck");
		assertThat(gemfireProperties.getProperty("ssl-truststore-type")).isEqualTo("C3PO");
		assertThat(gemfireProperties.getProperty("ssl-web-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("ssl-use-default-context")).isEqualTo("true");
	}

	@Test
	public void sslAnnotationBasedPeerConfigurationIsCorrect(){

		this.applicationContext =
			newApplicationContext(new MockPropertySource("TestPropertySource"),
				SslAnnotationBasedPeerConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache"));
		assertThat(this.applicationContext.containsBean("gemfireProperties"));

		GemFireCache peerCache =
				this.applicationContext.getBean("gemfireCache", GemFireCache.class);
		assertThat(peerCache).isNotNull();

		Properties gemfireProperties = peerCache.getDistributedSystem().getProperties();

		assertGemFirePropertiesCorrectlySet(gemfireProperties);
	}

	@Test
	public void sslPropertyBasedPeerConfigurationIsCorrect() {

		PropertySource testPropertySource = setSpringDataGemFireProperties();

		this.applicationContext = newApplicationContext(testPropertySource, SslPropertyBasedPeerConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache"));
		assertThat(this.applicationContext.containsBean("gemfireProperties"));

		GemFireCache peerCache =
				this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(peerCache).isNotNull();

		Properties gemfireProperties = peerCache.getDistributedSystem().getProperties();

		String sslEnabledComponents = Optional.ofNullable(gemfireProperties.getProperty("ssl-enabled-components"))
				.filter(StringUtils::hasText)
				.map(it -> StringUtils.arrayToCommaDelimitedString(
					ArrayUtils.sort(StringUtils.commaDelimitedListToStringArray(it))))
				.orElse(null);

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("ssl-ciphers")).isEqualTo("Scream SEAL SNOW");
		assertThat(sslEnabledComponents).isEqualTo("gateway,locator,server");
		assertThat(gemfireProperties.getProperty("ssl-default-alias")).isEqualTo("MockCert");
		assertThat(gemfireProperties.getProperty("ssl-gateway-alias")).isEqualTo("WanCert");
		assertThat(gemfireProperties.getProperty("ssl-server-alias")).isEqualTo("ServerCert");
		assertThat(gemfireProperties.getProperty("ssl-keystore")).isEqualTo("~/test/app/keystore.jks");
		assertThat(gemfireProperties.getProperty("ssl-keystore-password")).isEqualTo("0p3nS@y5M3");
		assertThat(gemfireProperties.getProperty("ssl-keystore-type")).isEqualTo("R2D2");
		assertThat(gemfireProperties.getProperty("ssl-protocols")).isEqualTo("IP TCP/IP UDP");
		assertThat(gemfireProperties.getProperty("ssl-require-authentication")).isEqualTo("false");
		assertThat(gemfireProperties.getProperty("ssl-truststore")).isEqualTo("relative/path/to/trusted.keystore");
		assertThat(gemfireProperties.getProperty("ssl-truststore-password")).isEqualTo("kn0ckKn0ck");
		assertThat(gemfireProperties.getProperty("ssl-truststore-type")).isEqualTo("C3PO");
		assertThat(gemfireProperties.getProperty("ssl-web-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("ssl-use-default-context")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("ssl-endpoint-identification-enabled")).isEqualTo("true");
	}

	@ClientCacheApplication(logLevel = GEMFIRE_LOG_LEVEL)
	@EnableSsl(
		ciphers = { "FISH", "Scream", "SEAL", "SNOW" },
		components = { EnableSsl.Component.SERVER, EnableSsl.Component.GATEWAY },
		componentCertificateAliases = {
			@EnableSsl.ComponentAlias(component = EnableSsl.Component.GATEWAY, alias = "WanCert")
		},
		defaultCertificateAlias = "TestCert",
		enableEndpointIdentification = true,
		keystore = "/path/to/keystore.jks",
		keystorePassword = "s3cr3t!",
		protocols = { "TCP/IP", "HTTP" },
		truststore = "/path/to/truststore.jks",
		truststorePassword = "p@55w0rd!",
		truststoreType = "PKCS11",
		useDefaultContext = true,
		webRequireAuthentication = true
	)
	static class SslAnnotationBasedClientConfiguration { }

	@ClientCacheApplication(logLevel = GEMFIRE_LOG_LEVEL)
	@EnableSsl
	static class SslPropertyBasedClientConfiguration { }

	@PeerCacheApplication(logLevel = GEMFIRE_LOG_LEVEL)
	@EnableSsl(
		ciphers = { "FISH", "Scream", "SEAL", "SNOW" },
		components = { EnableSsl.Component.SERVER, EnableSsl.Component.GATEWAY },
		componentCertificateAliases = {
			@EnableSsl.ComponentAlias(component = EnableSsl.Component.GATEWAY, alias = "WanCert")
		},
		defaultCertificateAlias = "TestCert",
		enableEndpointIdentification = true,
		keystore = "/path/to/keystore.jks",
		keystorePassword = "s3cr3t!",
		protocols = { "TCP/IP", "HTTP" },
		truststore = "/path/to/truststore.jks",
		truststorePassword = "p@55w0rd!",
		truststoreType = "PKCS11",
		useDefaultContext = true,
		webRequireAuthentication = true
	)
	static class SslAnnotationBasedPeerConfiguration { }

	@PeerCacheApplication(logLevel = GEMFIRE_LOG_LEVEL)
	@EnableSsl
	static class SslPropertyBasedPeerConfiguration { }

}
