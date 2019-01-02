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

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.gemfire.expiration.AnnotationBasedExpiration;
import org.springframework.data.gemfire.test.model.Person;

/**
 * The EnableExpirationConfigurationIntegrationTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class EnableExpirationConfigurationIntegrationTests {

	private static final String GEMFIRE_LOG_LEVEL = "error";

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		this.applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);
		return this.applicationContext;
	}

	@SuppressWarnings("unchecked")
	private <K, V> Region<K, V> getRegion(ConfigurableApplicationContext applicationContext, String beanName) {
		return applicationContext.getBean(beanName, Region.class);
	}

	private void assertRegionExpirationConfiguration(ConfigurableApplicationContext applicationContext,
			String regionBeanName) {

		Region<?, ?> region = getRegion(applicationContext, regionBeanName);

		assertThat(region).isNotNull();
		assertThat(region.getName()).isEqualTo(regionBeanName);
		assertThat(region.getAttributes()).isNotNull();
		assertThat(region.getAttributes().getDataPolicy()).isEqualTo(DataPolicy.NORMAL);
		assertThat(region.getAttributes().getStatisticsEnabled()).isTrue();
		assertThat(region.getAttributes().getCustomEntryIdleTimeout())
			.isInstanceOf(AnnotationBasedExpiration.class);
		assertThat(region.getAttributes().getCustomEntryTimeToLive())
			.isInstanceOf(AnnotationBasedExpiration.class);
	}

	@Test
	public void assertClientCacheRegionExpirationPoliciesAreCorrect() {
		assertRegionExpirationConfiguration(newApplicationContext(ClientCacheRegionExpirationConfiguration.class),
			"People");
	}

	@Test
	public void assertPeerCacheRegionExpirationPoliciesAreCorrect() {
		assertRegionExpirationConfiguration(newApplicationContext(PeerCacheRegionExpirationConfiguration.class),
			"People");
	}

	@ClientCacheApplication(name = "EnableExpirationConfigurationIntegrationTests", logLevel = GEMFIRE_LOG_LEVEL)
	@EnableEntityDefinedRegions(basePackageClasses = Person.class, clientRegionShortcut = ClientRegionShortcut.LOCAL)
	@EnableExpiration
	static class ClientCacheRegionExpirationConfiguration { }

	@PeerCacheApplication(name = "EnableExpirationConfigurationIntegrationTests", logLevel = GEMFIRE_LOG_LEVEL)
	@EnableEntityDefinedRegions(basePackageClasses = Person.class, serverRegionShortcut = RegionShortcut.LOCAL)
	@EnableExpiration
	static class PeerCacheRegionExpirationConfiguration { }

}
