/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.support;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.isA;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.expression.EvaluationException;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.CustomExpiry;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.ExpirationAttributes;
import com.gemstone.gemfire.cache.Region;

/**
 * The AnnotationBasedExpirationConfigurationIntegrationTest class is a test suite of test cases testing
 * the configuration of Annotation-defined expiration policies on Region Entry TTL and TTI
 * custom expiration settings.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see com.gemstone.gemfire.cache.CustomExpiry
 * @see com.gemstone.gemfire.cache.ExpirationAction
 * @see com.gemstone.gemfire.cache.ExpirationAttributes
 * @since 1.7.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class AnnotationBasedExpirationConfigurationIntegrationTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Autowired
	@Qualifier("genericExpiration")
	private AnnotationBasedExpiration<Object, Object> genericExpiration;

	@Autowired
	@Qualifier("ttiExpiration")
	private AnnotationBasedExpiration<Object, Object> idleTimeoutExpiration;

	@Autowired
	@Qualifier("ttlExpiration")
	private AnnotationBasedExpiration<Object, Object> timeToLiveExpiration;

	@Autowired
	private ExpirationAttributes defaultExpirationAttributes;

	@Resource(name = "Example")
	private Region<Object, Object> example;

	@Before
	public void setup() {
		assertThat(example, is(not(nullValue())));
		assertThat(example.getName(), is(equalTo("Example")));
		assertThat(example.getFullPath(), is(equalTo(String.format("%1$s%2$s", Region.SEPARATOR, "Example"))));
		assertThat(example.getAttributes(), is(not(nullValue())));
		assertThat(defaultExpirationAttributes, is(not(nullValue())));
		assertThat(defaultExpirationAttributes.getTimeout(), is(equalTo(600)));
		assertThat(defaultExpirationAttributes.getAction(), is(equalTo(ExpirationAction.DESTROY)));
		assertThat(genericExpiration, is(instanceOf(CustomExpiry.class)));
		assertThat(genericExpiration.getDefaultExpirationAttributes(), is(nullValue()));
		assertThat(idleTimeoutExpiration, is(instanceOf(CustomExpiry.class)));
		assertThat(idleTimeoutExpiration.getDefaultExpirationAttributes(), is(nullValue()));
		assertThat(timeToLiveExpiration, is(instanceOf(CustomExpiry.class)));
		assertThat(timeToLiveExpiration.getDefaultExpirationAttributes(), is(sameInstance(defaultExpirationAttributes)));
	}

	protected void assertExpiration(ExpirationAttributes expected, ExpirationAttributes actual) {
		assertExpiration(actual, expected.getTimeout(), expected.getAction());
	}

	protected void assertExpiration(ExpirationAttributes expirationAttributes, int expectedTimeout,
			ExpirationAction expectedAction) {

		assertThat(expirationAttributes, is(not(nullValue())));
		assertThat(expirationAttributes.getTimeout(), is(equalTo(expectedTimeout)));
		assertThat(expirationAttributes.getAction(), is(equalTo(expectedAction)));
	}

	@SuppressWarnings("unchecked")
	protected Region.Entry<Object, Object> mockRegionEntry(Object value) {
		Region.Entry<Object, Object> mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(value);

		return mockRegionEntry;
	}

	@Test
	public void exampleRegionIdleTimeoutExpirationPolicy() {
		CustomExpiry<Object, Object> expiration = example.getAttributes().getCustomEntryIdleTimeout();

		assertExpiration(expiration.getExpiry(mockRegionEntry(new ApplicationDomainObjectWithTimeToLiveAndGenericExpirationPolicies())),
			60, ExpirationAction.INVALIDATE);
		assertExpiration(expiration.getExpiry(mockRegionEntry(new ApplicationDomainObjectWithIdleTimeoutExpirationPolicy())),
			120, ExpirationAction.INVALIDATE);
		assertExpiration(expiration.getExpiry(mockRegionEntry(new ApplicationDomainObjectWithGenericExpirationPolicy())),
			60, ExpirationAction.DESTROY);
		assertThat(expiration.getExpiry(mockRegionEntry(new ApplicationDomainObjectWithNoExpirationPolicy())), is(nullValue()));
		assertThat(expiration.getExpiry(mockRegionEntry(new RegionEntryTimeToLiveExpirationPolicy())), is(nullValue()));
		assertExpiration(expiration.getExpiry(mockRegionEntry(new RegionEntryIdleTimeoutExpirationPolicy())),
			60, ExpirationAction.INVALIDATE);
		assertExpiration(expiration.getExpiry(mockRegionEntry(new RegionEntryGenericExpirationPolicy())),
			60, ExpirationAction.DESTROY);
	}

	@Test
	public void exampleRegionTimeToLiveExpirationPolicy() {
		CustomExpiry<Object, Object> expiration = example.getAttributes().getCustomEntryTimeToLive();

		assertExpiration(expiration.getExpiry(mockRegionEntry(new ApplicationDomainObjectWithTimeToLiveAndGenericExpirationPolicies())),
			300, ExpirationAction.DESTROY);
		assertExpiration(defaultExpirationAttributes, expiration.getExpiry(mockRegionEntry(new ApplicationDomainObjectWithIdleTimeoutExpirationPolicy())));
		assertExpiration(expiration.getExpiry(mockRegionEntry(new ApplicationDomainObjectWithGenericExpirationPolicy())),
			60, ExpirationAction.DESTROY);
		assertExpiration(defaultExpirationAttributes, expiration.getExpiry(mockRegionEntry(new ApplicationDomainObjectWithNoExpirationPolicy())));
		assertExpiration(expiration.getExpiry(mockRegionEntry(new RegionEntryTimeToLiveExpirationPolicy())),
			60, ExpirationAction.LOCAL_INVALIDATE);
		assertExpiration(defaultExpirationAttributes, expiration.getExpiry(mockRegionEntry(new RegionEntryIdleTimeoutExpirationPolicy())));
		assertExpiration(expiration.getExpiry(mockRegionEntry(new RegionEntryGenericExpirationPolicy())),
			60, ExpirationAction.DESTROY);
	}

	@Test
	public void genericExpirationPolicy() {
		assertExpiration(genericExpiration.getExpiry(mockRegionEntry(
			new ApplicationDomainObjectWithTimeToLiveAndGenericExpirationPolicies())), 60, ExpirationAction.INVALIDATE);
		assertThat(genericExpiration.getExpiry(mockRegionEntry(
			new ApplicationDomainObjectWithIdleTimeoutExpirationPolicy())), is(nullValue()));
		assertExpiration(genericExpiration.getExpiry(mockRegionEntry(
			new ApplicationDomainObjectWithGenericExpirationPolicy())), 60, ExpirationAction.DESTROY);
		assertThat(genericExpiration.getExpiry(mockRegionEntry(
			new ApplicationDomainObjectWithNoExpirationPolicy())), is(nullValue()));
		assertThat(genericExpiration.getExpiry(mockRegionEntry(
			new RegionEntryTimeToLiveExpirationPolicy())), is(nullValue()));
		assertThat(genericExpiration.getExpiry(mockRegionEntry(
			new RegionEntryIdleTimeoutExpirationPolicy())), is(nullValue()));
		assertExpiration(genericExpiration.getExpiry(mockRegionEntry(
			new RegionEntryGenericExpirationPolicy())), 60, ExpirationAction.DESTROY);
	}

	@Test
	public void invalidExpirationAction() {
		expectedException.expect(EvaluationException.class);
		expectedException.expectCause(isA(IllegalArgumentException.class));
		expectedException.expectMessage(String.format("[%s] is not resolvable as an ExpirationAction(Type)",
			"@expirationProperties['gemfire.region.entry.expiration.invalid.action.string']"));
		genericExpiration.getExpiry(mockRegionEntry(new RegionEntryWithInvalidExpirationAction()));
	}

	@Test
	public void invalidExpirationTimeout() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		genericExpiration.getExpiry(mockRegionEntry(new RegionEntryWithInvalidExpirationTimeout()));
	}

	@Expiration(timeout = "60", action = "INVALIDATE")
	@TimeToLiveExpiration(timeout = "300", action = "DESTROY")
	protected static class ApplicationDomainObjectWithTimeToLiveAndGenericExpirationPolicies {
	}

	@IdleTimeoutExpiration(timeout = "120", action = "INVALIDATE")
	protected static class ApplicationDomainObjectWithIdleTimeoutExpirationPolicy {
	}

	@Expiration(timeout = "60", action = "DESTROY")
	protected static class ApplicationDomainObjectWithGenericExpirationPolicy {
	}

	protected static class ApplicationDomainObjectWithNoExpirationPolicy {
	}

	@TimeToLiveExpiration(timeout = "${gemfire.region.entry.expiration.timeout}",
		action = "${gemfire.region.entry.expiration.action.string}")
	protected static class RegionEntryTimeToLiveExpirationPolicy {
	}

	@IdleTimeoutExpiration(timeout = "@expirationProperties['gemfire.region.entry.expiration.timeout']",
		action = "@expirationProperties['gemfire.region.entry.expiration.action.gemfire.type']")
	protected static class RegionEntryIdleTimeoutExpirationPolicy {
	}

	@Expiration(timeout = "${gemfire.region.entry.expiration.timeout}",
		action = "@expirationProperties['gemfire.region.entry.expiration.action.spring.type']")
	protected static class RegionEntryGenericExpirationPolicy {
	}

	@Expiration(timeout = "${gemfire.region.entry.expiration.timeout}",
		action = "@expirationProperties['gemfire.region.entry.expiration.invalid.action.string']")
	protected static class RegionEntryWithInvalidExpirationAction {
	}

	@Expiration(timeout = "${gemfire.region.entry.expiration.invalid.timeout}",
		action = "@expirationProperties['gemfire.region.entry.expiration.action.spring.type']")
	protected static class RegionEntryWithInvalidExpirationTimeout {
	}

}
