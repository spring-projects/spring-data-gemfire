/*
 * Copyright 2016-2018 the original author or authors.
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
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationPolicy;
import static org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationType;

import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.AttributesMutator;
import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.junit.After;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.expiration.ExpirationActionType;
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
		if (applicationContext != null) {
			applicationContext.close();
		}
	}

	protected <K, V> void assertRegionExpiration(ExpirationAttributes expectedExpirationAttributes,
			Region<K, V> region, V... applicationDomainObjects) {

		assertIdleTimeoutExpiration(expectedExpirationAttributes, region, applicationDomainObjects);
		assertTimeToLiveExpiration(expectedExpirationAttributes, region, applicationDomainObjects);
	}

	protected <K, V> void assertIdleTimeoutExpiration(ExpirationAttributes expectedExpirationAttributes,
			Region<K, V> region, V... applicationDomainObjects) {

		assertExpiration(expectedExpirationAttributes, region.getAttributes().getCustomEntryIdleTimeout(),
			applicationDomainObjects);
	}

	protected <K, V> void assertNoIdleTimeoutExpiration(Region<K, V> region) {
		assertThat(region.getAttributes().getCustomEntryIdleTimeout()).isNull();
	}

	protected <K, V> void assertTimeToLiveExpiration(ExpirationAttributes expectedExpirationAttributes,
			Region<K, V> region, V... applicationDomainObjects) {

		assertExpiration(expectedExpirationAttributes, region.getAttributes().getCustomEntryTimeToLive(),
			applicationDomainObjects);
	}

	protected <K, V> void assertNoTimeToLiveExpiration(Region<K, V> region) {
		assertThat(region.getAttributes().getCustomEntryTimeToLive()).isNull();
	}

	@SuppressWarnings("unchecked")
	protected <K, V> void assertExpiration(ExpirationAttributes expectedExpirationAttributes,
			CustomExpiry<K, V> customExpiry, V... applicationDomainObjects) {

		Region.Entry<K, V> regionEntry = mockRegionEntry(ArrayUtils.getFirst(applicationDomainObjects));

		assertExpiration(customExpiry.getExpiry(regionEntry), expectedExpirationAttributes);
	}

	@SuppressWarnings({ "unchecked", "unused" })
	protected <K, V> void assertExpiration(ExpirationAttributes actualExpirationAttributes,
			ExpirationAttributes expectedExpirationAttributes) {

		assertThat(actualExpirationAttributes).isEqualTo(expectedExpirationAttributes);
	}

	@SuppressWarnings("unchecked")
	protected <K, V> Region<K, V> getRegion(String beanName) {
		return applicationContext.getBean(beanName, Region.class);
	}

	protected AnnotationConfigApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return new AnnotationConfigApplicationContext(annotatedClasses);
	}

	protected ExpirationAttributes newExpirationAttributes(int timeout, ExpirationActionType action) {
		return newExpirationAttributes(timeout, action.getExpirationAction());
	}

	protected ExpirationAttributes newExpirationAttributes(int timeout, ExpirationAction action) {
		return new ExpirationAttributes(timeout, action);
	}

	@Test
	public void usesDefaultExpirationPolicyConfiguration() {
		applicationContext = newApplicationContext(DefaultExpirationPolicyConfiguration.class);

		ExpirationAttributes expectedExpiration = newExpirationAttributes(0, ExpirationActionType.INVALIDATE);

		Region one = getRegion("One");
		Region two = getRegion("Two");

		assertIdleTimeoutExpiration(expectedExpiration, one);
		assertIdleTimeoutExpiration(expectedExpiration, two);

		assertNoTimeToLiveExpiration(one);
		assertNoTimeToLiveExpiration(two);
	}

	@Test
	public void usesCustomExpirationPolicyConfiguration() {
		applicationContext = newApplicationContext(CustomExpirationPolicyConfiguration.class);

		ExpirationAttributes expectedExpiration = newExpirationAttributes(300, ExpirationActionType.LOCAL_DESTROY);

		Region one = getRegion("One");
		Region two = getRegion("Two");

		assertIdleTimeoutExpiration(expectedExpiration, one);
		assertIdleTimeoutExpiration(expectedExpiration, two);

		assertNoTimeToLiveExpiration(one);
		assertNoTimeToLiveExpiration(two);
	}

	@Test
	public void usesRegionSpecificExpirationPolicyConfiguration() {
		applicationContext = newApplicationContext(RegionSpecificExpirationPolicyConfiguration.class);

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
	public void usesMixedExpirationPolicyConfiguration() {
		applicationContext = newApplicationContext(MixedExpirationPolicyConfiguration.class);

		ExpirationAttributes expectedIdleTimeout = newExpirationAttributes(120, ExpirationActionType.LOCAL_INVALIDATE);
		ExpirationAttributes expectedTimeToLive = newExpirationAttributes(600, ExpirationActionType.DESTROY);

		Region one = getRegion("One");
		Region two = getRegion("Two");

		assertIdleTimeoutExpiration(expectedIdleTimeout, one);
		assertNoIdleTimeoutExpiration(two);

		assertTimeToLiveExpiration(expectedTimeToLive, one);
		assertTimeToLiveExpiration(expectedTimeToLive, two);
	}

	@SuppressWarnings("unchecked")
	static <K, V> Region<K, V> mockRegion(String name) {
		Region<K, V> mockRegion = mock(Region.class);

		when(mockRegion.getName()).thenReturn(name);
		when(mockRegion.getFullPath()).thenReturn(String.format("%1$s%2$s", Region.SEPARATOR, name));

		final AtomicReference<CustomExpiry<K, V>> entryIdleTimeout = new AtomicReference<CustomExpiry<K, V>>(null);
		final AtomicReference<CustomExpiry<K, V>> entryTimeToLive = new AtomicReference<CustomExpiry<K, V>>(null);

		AttributesMutator<K, V> mockAttributesMutator = mock(AttributesMutator.class);

		when(mockRegion.getAttributesMutator()).thenReturn(mockAttributesMutator);

		doAnswer(new Answer<CustomExpiry<K, V>>() {
			@Override
			public CustomExpiry<K, V> answer(InvocationOnMock invocation) throws Throwable {
				CustomExpiry<K, V> customExpiry = invocation.getArgument(0);
				entryIdleTimeout.set(customExpiry);
				return customExpiry;
			}
		}).when(mockAttributesMutator).setCustomEntryIdleTimeout(any(CustomExpiry.class));

		doAnswer(new Answer<CustomExpiry<K, V>>() {
			@Override
			public CustomExpiry<K, V> answer(InvocationOnMock invocation) throws Throwable {
				CustomExpiry<K, V> customExpiry = invocation.getArgument(0);
				entryTimeToLive.set(customExpiry);
				return customExpiry;
			}
		}).when(mockAttributesMutator).setCustomEntryTimeToLive(any(CustomExpiry.class));

		RegionAttributes<K, V> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);

		when(mockRegionAttributes.getCustomEntryIdleTimeout()).thenAnswer(new Answer<CustomExpiry<K, V>>() {
			@Override public CustomExpiry<K, V> answer(InvocationOnMock invocation) throws Throwable {
				return entryIdleTimeout.get();
			}
		});

		when(mockRegionAttributes.getCustomEntryTimeToLive()).thenAnswer(new Answer<CustomExpiry<K, V>>() {
			@Override public CustomExpiry<K, V> answer(InvocationOnMock invocation) throws Throwable {
				return entryTimeToLive.get();
			}
		});

		return mockRegion;
	}

	@SuppressWarnings("unchecked")
	static <K, V> Region.Entry<K, V> mockRegionEntry(V applicationDomainObject) {
		Region.Entry<K, V> mockRegionEntry = mock(Region.Entry.class);
		when(mockRegionEntry.getValue()).thenReturn(applicationDomainObject);
		return mockRegionEntry;
	}

	@Configuration
	@SuppressWarnings("unused")
	static class RegionConfiguration {

		@Bean("One")
		Region<Object, Object> regionOne() {
			return mockRegion("One");
		}

		@Bean("Two")
		Region<Object, Object> regionTwo() {
			return mockRegion("Two");
		}
	}

	@EnableExpiration
	static class DefaultExpirationPolicyConfiguration extends RegionConfiguration {
	}

	@EnableExpiration(policies = { @ExpirationPolicy(timeout = 300, action = ExpirationActionType.LOCAL_DESTROY) })
	static class CustomExpirationPolicyConfiguration extends RegionConfiguration {
	}

	@EnableExpiration(policies = {
		@ExpirationPolicy(timeout = 180, action = ExpirationActionType.INVALIDATE, regionNames = "One"),
		@ExpirationPolicy(timeout = 360, action = ExpirationActionType.DESTROY, regionNames = "Two", types = ExpirationType.TIME_TO_LIVE)
	})
	static class RegionSpecificExpirationPolicyConfiguration extends RegionConfiguration {
	}

	@EnableExpiration(policies = {
		@ExpirationPolicy(timeout = 120, action = ExpirationActionType.LOCAL_INVALIDATE, regionNames = "One", types = ExpirationType.IDLE_TIMEOUT),
		@ExpirationPolicy(timeout = 600, action = ExpirationActionType.DESTROY, types = ExpirationType.TIME_TO_LIVE)
	})
	static class MixedExpirationPolicyConfiguration extends RegionConfiguration {
	}
}
