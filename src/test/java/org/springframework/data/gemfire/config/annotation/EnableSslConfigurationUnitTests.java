/*
 * Copyright 2018-2019 the original author or authors.
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

import java.util.Optional;
import java.util.Properties;

import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.mock.env.MockPropertySource;
import org.springframework.util.StringUtils;

/**
 * Unit tests for {@link EnableSsl} and {@link SslConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.springframework.data.gemfire.config.annotation.EnableSsl
 * @see org.springframework.data.gemfire.config.annotation.SslConfiguration
 * @since 2.1.0
 */
public class EnableSslConfigurationUnitTests {

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

		applicationContext.register(annotatedClasses);
		applicationContext.registerShutdownHook();
		applicationContext.refresh();

		return applicationContext;
	}

	@Test
	public void sslAnnotationBasedConfigurationIsCorrect() {

		this.applicationContext = newApplicationContext(new MockPropertySource("TestPropertySource"),
			SslAnnotationBasedConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache"));
		assertThat(this.applicationContext.containsBean("gemfireProperties"));

		ClientCacheFactoryBean clientCache =
			this.applicationContext.getBean("&gemfireCache", ClientCacheFactoryBean.class);

		assertThat(clientCache).isNotNull();

		Properties gemfireProperties = clientCache.getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("ssl-ciphers")).isEqualTo("FISH,Scream,SEAL,SNOW");
		assertThat(gemfireProperties.getProperty("ssl-enabled-components")).isEqualTo("server,gateway");
		assertThat(gemfireProperties.getProperty("ssl-default-alias")).isEqualTo("TestCert");
		assertThat(gemfireProperties.getProperty("ssl-gateway-alias")).isEqualTo("WanCert");
		assertThat(gemfireProperties.getProperty("ssl-keystore")).isEqualTo("/path/to/keystore.jks");
		assertThat(gemfireProperties.getProperty("ssl-keystore-password")).isEqualTo("s3cr3t!");
		assertThat(gemfireProperties.getProperty("ssl-keystore-type")).isEqualTo("JKS");
		assertThat(gemfireProperties.getProperty("ssl-protocols")).isEqualTo("TCP/IP,HTTP");
		assertThat(gemfireProperties.getProperty("ssl-require-authentication")).isEqualTo("true");
		assertThat(gemfireProperties.getProperty("ssl-truststore")).isEqualTo("/path/to/truststore.jks");
		assertThat(gemfireProperties.getProperty("ssl-truststore-password")).isEqualTo("p@55w0rd!");
		assertThat(gemfireProperties.getProperty("ssl-web-require-authentication")).isEqualTo("true");
	}

	@Test
	public void sslPropertyBasedConfigurationIsCorrect() {

		PropertySource testPropertySource = new MockPropertySource("TestPropertySource")
			.withProperty("spring.data.gemfire.security.ssl.ciphers", "Scream, SEAL, SNOW")
			.withProperty("spring.data.gemfire.security.ssl.components", "locator, server, gateway")
			.withProperty("spring.data.gemfire.security.ssl.certificate.alias.default", "MockCert")
			.withProperty("spring.data.gemfire.security.ssl.certificate.alias.gateway", "WanCert")
			.withProperty("spring.data.gemfire.security.ssl.certificate.alias.server", "ServerCert")
			.withProperty("spring.data.gemfire.security.ssl.keystore", "~/test/app/keystore.jks")
			.withProperty("spring.data.gemfire.security.ssl.keystore.password", "0p3nS@y5M3")
			.withProperty("spring.data.gemfire.security.ssl.keystore.type", "R2D2")
			.withProperty("spring.data.gemfire.security.ssl.protocols", "IP, TCP/IP, UDP")
			.withProperty("spring.data.gemfire.security.ssl.require-authentication", "false")
			.withProperty("spring.data.gemfire.security.ssl.truststore", "relative/path/to/trusted.keystore")
			.withProperty("spring.data.gemfire.security.ssl.truststore.password", "kn0ckKn0ck")
			.withProperty("spring.data.gemfire.security.ssl.web-require-authentication", "true");

		this.applicationContext = newApplicationContext(testPropertySource, SslPropertyBasedConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache"));
		assertThat(this.applicationContext.containsBean("gemfireProperties"));

		ClientCacheFactoryBean clientCache =
			this.applicationContext.getBean("&gemfireCache", ClientCacheFactoryBean.class);

		assertThat(clientCache).isNotNull();

		Properties gemfireProperties = clientCache.getProperties();

		String sslEnabledComponents = Optional.ofNullable(gemfireProperties.getProperty("ssl-enabled-components"))
			.filter(StringUtils::hasText)
			.map(it -> StringUtils.arrayToCommaDelimitedString(
					ArrayUtils.sort(StringUtils.commaDelimitedListToStringArray(it))))
			.orElse(null);

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("ssl-ciphers")).isEqualTo("Scream, SEAL, SNOW");
		assertThat(sslEnabledComponents).isEqualTo("gateway,locator,server");
		assertThat(gemfireProperties.getProperty("ssl-default-alias")).isEqualTo("MockCert");
		assertThat(gemfireProperties.getProperty("ssl-gateway-alias")).isEqualTo("WanCert");
		assertThat(gemfireProperties.getProperty("ssl-server-alias")).isEqualTo("ServerCert");
		assertThat(gemfireProperties.getProperty("ssl-keystore")).isEqualTo("~/test/app/keystore.jks");
		assertThat(gemfireProperties.getProperty("ssl-keystore-password")).isEqualTo("0p3nS@y5M3");
		assertThat(gemfireProperties.getProperty("ssl-keystore-type")).isEqualTo("R2D2");
		assertThat(gemfireProperties.getProperty("ssl-protocols")).isEqualTo("IP, TCP/IP, UDP");
		assertThat(gemfireProperties.getProperty("ssl-require-authentication")).isEqualTo("false");
		assertThat(gemfireProperties.getProperty("ssl-truststore")).isEqualTo("relative/path/to/trusted.keystore");
		assertThat(gemfireProperties.getProperty("ssl-truststore-password")).isEqualTo("kn0ckKn0ck");
		assertThat(gemfireProperties.getProperty("ssl-web-require-authentication")).isEqualTo("true");
	}

	@EnableGemFireMockObjects
	@ClientCacheApplication(logLevel = "error")
	@EnableSsl(
		ciphers = { "FISH", "Scream", "SEAL", "SNOW" },
		components = { EnableSsl.Component.SERVER, EnableSsl.Component.GATEWAY },
		componentCertificateAliases = {
			@EnableSsl.ComponentAlias(component = EnableSsl.Component.GATEWAY, alias = "WanCert")
		},
		defaultCertificateAlias = "TestCert",
		keystore = "/path/to/keystore.jks",
		keystorePassword = "s3cr3t!",
		protocols = { "TCP/IP", "HTTP" },
		truststore = "/path/to/truststore.jks",
		truststorePassword = "p@55w0rd!",
		webRequireAuthentication = true
	)
	static class SslAnnotationBasedConfiguration { }

	@EnableGemFireMockObjects
	@ClientCacheApplication(logLevel = "error")
	@EnableSsl
	static class SslPropertyBasedConfiguration { }

}
