/*
 * Copyright 2016 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationPolicy;
import static org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationType;

import java.util.Optional;

import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.expiration.ExpirationActionType;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.util.ArrayUtils;

/**
 * Unit tests for the {@link EnableExpiration} annotation and {@link ExpirationConfiguration} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.data.gemfire.config.annotation.EnableExpiration
 * @see org.springframework.data.gemfire.config.annotation.ExpirationConfiguration
 * @see org.apache.geode.cache.CustomExpiry
 * @see org.apache.geode.cache.ExpirationAttributes
 * @see org.apache.geode.cache.Region
 * @since 1.9.0
 */
public class EnableExpirationConfigurationUnitTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	@SuppressWarnings({ "unchecked", "unused" })
	private <K, V> void assertRegionExpiration(ExpirationAttributes expectedExpirationAttributes,
			Region<K, V> region, V... applicationDomainObjects) {

		assertIdleTimeoutExpiration(expectedExpirationAttributes, region, applicationDomainObjects);
		assertTimeToLiveExpiration(expectedExpirationAttributes, region, applicationDomainObjects);
	}

	@SuppressWarnings("unchecked")
	private <K, V> void assertIdleTimeoutExpiration(ExpirationAttributes expectedExpirationAttributes,
			Region<K, V> region, V... applicationDomainObjects) {

		assertExpiration(expectedExpirationAttributes, region.getAttributes().getCustomEntryIdleTimeout(),
			applicationDomainObjects);
	}

	private <K, V> void assertNoIdleTimeoutExpiration(Region<K, V> region) {
		assertThat(region.getAttributes().getCustomEntryIdleTimeout()).isNull();
	}

	@SuppressWarnings("unchecked")
	private <K, V> void assertTimeToLiveExpiration(ExpirationAttributes expectedExpirationAttributes,
			Region<K, V> region, V... applicationDomainObjects) {

		assertExpiration(expectedExpirationAttributes, region.getAttributes().getCustomEntryTimeToLive(),
			applicationDomainObjects);
	}

	private <K, V> void assertNoTimeToLiveExpiration(Region<K, V> region) {
		assertThat(region.getAttributes().getCustomEntryTimeToLive()).isNull();
	}

	@SuppressWarnings("unchecked")
	private <K, V> void assertExpiration(ExpirationAttributes expectedExpirationAttributes,
			CustomExpiry<K, V> customExpiry, V... applicationDomainObjects) {

		Region.Entry<K, V> regionEntry = mockRegionEntry(ArrayUtils.getFirst(applicationDomainObjects));

		assertExpiration(customExpiry.getExpiry(regionEntry), expectedExpirationAttributes);
	}

	@SuppressWarnings({ "unchecked", "unused" })
	private <K, V> void assertExpiration(ExpirationAttributes actualExpirationAttributes,
			ExpirationAttributes expectedExpirationAttributes) {

		assertThat(actualExpirationAttributes).isEqualTo(expectedExpirationAttributes);
	}

	@SuppressWarnings("unchecked")
	private <K, V> Region<K, V> getRegion(String beanName) {
		return this.applicationContext.getBean(beanName, Region.class);
	}

	private AnnotationConfigApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return new AnnotationConfigApplicationContext(annotatedClasses);
	}

	private  ExpirationAttributes newExpirationAttributes(int timeout, ExpirationActionType action) {
		return newExpirationAttributes(timeout, action.getExpirationAction());
	}

	private ExpirationAttributes newExpirationAttributes(int timeout, ExpirationAction action) {
		return new ExpirationAttributes(timeout, action);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void usesDefaultExpirationPolicyConfiguration() {

		this.applicationContext = newApplicationContext(DefaultExpirationPolicyConfiguration.class);

		ExpirationAttributes expectedExpiration = newExpirationAttributes(0, ExpirationActionType.INVALIDATE);

		Region one = getRegion("One");
		Region two = getRegion("Two");

		assertIdleTimeoutExpiration(expectedExpiration, one);
		assertIdleTimeoutExpiration(expectedExpiration, two);
		assertTimeToLiveExpiration(expectedExpiration, one);
		assertTimeToLiveExpiration(expectedExpiration, two);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void usesCustomIdleTimeoutExpirationPolicyConfiguration() {

		this.applicationContext = newApplicationContext(CustomIdleTimeoutExpirationPolicyConfiguration.class);

		ExpirationAttributes expectedExpiration = newExpirationAttributes(300, ExpirationActionType.LOCAL_DESTROY);

		Region one = getRegion("One");
		Region two = getRegion("Two");

		assertIdleTimeoutExpiration(expectedExpiration, one);
		assertIdleTimeoutExpiration(expectedExpiration, two);

		assertNoTimeToLiveExpiration(one);
		assertNoTimeToLiveExpiration(two);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void usesCustomTimeToLiveExpirationPolicyConfiguration() {

		this.applicationContext = newApplicationContext(CustomTimeToLiveTimeoutExpirationPolicyConfiguration.class);

		ExpirationAttributes expectedExpiration = newExpirationAttributes(900, ExpirationActionType.LOCAL_INVALIDATE);

		Region one = getRegion("One");
		Region two = getRegion("Two");

		assertTimeToLiveExpiration(expectedExpiration, one);
		assertTimeToLiveExpiration(expectedExpiration, two);

		assertNoIdleTimeoutExpiration(one);
		assertNoIdleTimeoutExpiration(two);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void usesRegionSpecificExpirationPolicyConfiguration() {

		this.applicationContext = newApplicationContext(RegionSpecificExpirationPolicyConfiguration.class);

		ExpirationAttributes expectedIdleTimeout = newExpirationAttributes(180, ExpirationActionType.INVALIDATE);
		ExpirationAttributes expectedTimeToLive = newExpirationAttributes(360, ExpirationActionType.DESTROY);

		Region one = getRegion("One");
		Region two = getRegion("Two");

		assertIdleTimeoutExpiration(expectedIdleTimeout, one);
		assertNoIdleTimeoutExpiration(two);

		assertNoTimeToLiveExpiration(one);
		assertTimeToLiveExpiration(expectedTimeToLive, two);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void usesMixedExpirationPolicyConfiguration() {

		this.applicationContext = newApplicationContext(MixedExpirationPolicyConfiguration.class);

		ExpirationAttributes expectedIdleTimeout = newExpirationAttributes(60, ExpirationActionType.LOCAL_INVALIDATE);
		ExpirationAttributes expectedTimeToLive = newExpirationAttributes(600, ExpirationActionType.DESTROY);

		Region one = getRegion("One");
		Region two = getRegion("Two");

		assertIdleTimeoutExpiration(expectedIdleTimeout, one);
		assertNoIdleTimeoutExpiration(two);

		assertTimeToLiveExpiration(expectedTimeToLive, one);
		assertTimeToLiveExpiration(expectedTimeToLive, two);
	}

	@SuppressWarnings("unchecked")
	static <K, V> Region.Entry<K, V> mockRegionEntry(V applicationDomainObject) {

		Region.Entry<K, V> mockRegionEntry = mock(Region.Entry.class);

		when(mockRegionEntry.getValue()).thenReturn(applicationDomainObject);

		return mockRegionEntry;
	}

	@PeerCacheApplication
	@EnableGemFireMockObjects
	@SuppressWarnings("unused")
	static class RegionConfiguration {

		@Bean("One")
		public LocalRegionFactoryBean<Object, Object> regionOne(GemFireCache gemfireCache) {

			LocalRegionFactoryBean<Object, Object> regionOne = new LocalRegionFactoryBean<>();

			regionOne.setCache(gemfireCache);
			regionOne.setClose(false);
			regionOne.setPersistent(false);

			return regionOne;
		}

		@Bean("Two")
		public LocalRegionFactoryBean<Object, Object> regionTwo(GemFireCache gemfireCache) {

			LocalRegionFactoryBean<Object, Object> regionTwo = new LocalRegionFactoryBean<>();

			regionTwo.setCache(gemfireCache);
			regionTwo.setClose(false);
			regionTwo.setPersistent(false);

			return regionTwo;
		}

	}

	@EnableExpiration
	static class DefaultExpirationPolicyConfiguration extends RegionConfiguration { }

	@EnableExpiration(policies = { @ExpirationPolicy(timeout = 300, action = ExpirationActionType.LOCAL_DESTROY) })
	static class CustomIdleTimeoutExpirationPolicyConfiguration extends RegionConfiguration { }

	@EnableExpiration(policies = { @ExpirationPolicy(timeout = 900, action = ExpirationActionType.LOCAL_INVALIDATE, types = ExpirationType.TIME_TO_LIVE) })
	static class CustomTimeToLiveTimeoutExpirationPolicyConfiguration extends RegionConfiguration { }

	@EnableExpiration(policies = {
		@ExpirationPolicy(timeout = 180, action = ExpirationActionType.INVALIDATE, regionNames = "One"),
		@ExpirationPolicy(timeout = 360, action = ExpirationActionType.DESTROY, regionNames = "Two", types = ExpirationType.TIME_TO_LIVE)
	})
	static class RegionSpecificExpirationPolicyConfiguration extends RegionConfiguration { }

	@EnableExpiration(policies = {
		@ExpirationPolicy(timeout = 60, action = ExpirationActionType.LOCAL_INVALIDATE, regionNames = "One", types = ExpirationType.IDLE_TIMEOUT),
		@ExpirationPolicy(timeout = 600, action = ExpirationActionType.DESTROY, types = ExpirationType.TIME_TO_LIVE)
	})
	static class MixedExpirationPolicyConfiguration extends RegionConfiguration { }

}
