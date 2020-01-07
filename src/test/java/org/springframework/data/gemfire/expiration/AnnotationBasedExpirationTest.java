/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.expiration;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.isA;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.expiration.AnnotationBasedExpiration.ExpirationMetaData;

import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;

import org.junit.Test;
import org.mockito.Matchers;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.expression.BeanResolver;
import org.springframework.expression.PropertyAccessor;
import org.springframework.expression.TypeConverter;
import org.springframework.expression.TypeLocator;
import org.springframework.expression.spel.support.StandardEvaluationContext;

/**
 * The AnnotationBasedExpirationTest class is a test suite of test cases testing the contract and functionality
 * of the AnnotationBasedExpirationTest.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see AnnotationBasedExpiration
 * @since 1.7.0
 */
@SuppressWarnings({ "rawtypes", "unchecked", "unused" })
public class AnnotationBasedExpirationTest {

	private AnnotationBasedExpiration noDefaultExpiration = new AnnotationBasedExpiration();

	protected void assertExpiration(ExpirationAttributes expirationAttributes, int expectedTimeout,
			ExpirationAction expectedAction) {

		assertThat(expirationAttributes, is(not(nullValue())));
		assertThat(expirationAttributes.getTimeout(), is(equalTo(expectedTimeout)));
		assertThat(expirationAttributes.getAction(), is(equalTo(expectedAction)));
	}

	protected void assertExpiration(ExpirationMetaData expirationMetaData, int expectedTimeout,
			ExpirationActionType expectedExpirationAction) {

		assertThat(expirationMetaData, is(not(nullValue())));
		assertThat(expirationMetaData.timeout(), is(equalTo(expectedTimeout)));
		assertThat(expirationMetaData.action(), is(equalTo(expectedExpirationAction)));
	}

	@Test
	public void constructUninitializedAnnotationBasedExpirationInstance() {
		AnnotationBasedExpiration expiration = new AnnotationBasedExpiration();

		assertThat(expiration.getDefaultExpirationAttributes(), is(nullValue()));
	}

	@Test
	public void constructInitializedAnnotationBasedExpirationInstance() {
		AnnotationBasedExpiration expiration = new AnnotationBasedExpiration(ExpirationAttributes.DEFAULT);

		assertThat(expiration.getDefaultExpirationAttributes(), is(equalTo(ExpirationAttributes.DEFAULT)));
	}

	@Test
	public void forIdleTimeoutNoDefaultExpiration() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		AnnotationBasedExpiration expiration = AnnotationBasedExpiration.forIdleTimeout();

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutExpiration());
		assertExpiration(expiration.getExpiry(mockRegionEntry), 120, ExpirationAction.LOCAL_INVALIDATE);
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveGenericExpiration());
		assertExpiration(expiration.getExpiry(mockRegionEntry), 60, ExpirationAction.INVALIDATE);
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(expiration.getExpiry(mockRegionEntry), is(nullValue()));
		verify(mockRegionEntry, atLeast(3)).getValue();
	}

	@Test
	public void forIdleTimeoutWithDefaultExpiration() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		ExpirationAttributes defaultExpiration = new ExpirationAttributes(300, ExpirationAction.DESTROY);

		AnnotationBasedExpiration expiration = AnnotationBasedExpiration.forIdleTimeout(defaultExpiration);

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutExpiration());
		assertExpiration(expiration.getExpiry(mockRegionEntry), 120, ExpirationAction.LOCAL_INVALIDATE);
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveGenericExpiration());
		assertExpiration(expiration.getExpiry(mockRegionEntry), 60, ExpirationAction.INVALIDATE);
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(expiration.getExpiry(mockRegionEntry), is(equalTo(defaultExpiration)));
		verify(mockRegionEntry, atLeast(3)).getValue();
	}

	@Test
	public void forTimeToLiveNoDefaultExpiration() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		AnnotationBasedExpiration expiration = AnnotationBasedExpiration.forTimeToLive();

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutExpiration());
		assertExpiration(expiration.getExpiry(mockRegionEntry), 300, ExpirationAction.LOCAL_DESTROY);
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithIdleTimeoutGenericExpiration());
		assertExpiration(expiration.getExpiry(mockRegionEntry), 60, ExpirationAction.INVALIDATE);
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(expiration.getExpiry(mockRegionEntry), is(nullValue()));
		verify(mockRegionEntry, atLeast(3)).getValue();
	}

	@Test
	public void forTimeToLiveWithDefaultExpiration() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		ExpirationAttributes defaultExpiration = new ExpirationAttributes(300, ExpirationAction.DESTROY);

		AnnotationBasedExpiration expiration = AnnotationBasedExpiration.forTimeToLive(defaultExpiration);

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutExpiration());
		assertExpiration(expiration.getExpiry(mockRegionEntry), 300, ExpirationAction.LOCAL_DESTROY);
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithIdleTimeoutGenericExpiration());
		assertExpiration(expiration.getExpiry(mockRegionEntry), 60, ExpirationAction.INVALIDATE);
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(expiration.getExpiry(mockRegionEntry), is(equalTo(defaultExpiration)));
		verify(mockRegionEntry, atLeast(3)).getValue();
	}

	@Test
	public void setAndGetBeanFactory() {
		final StandardEvaluationContext mockEvaluationContext = mock(StandardEvaluationContext.class,
			"MockStandardEvaluationContext");

		ConversionService mockConversionService = mock(ConversionService.class, "MockConversionService");

		final ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class, "MockBeanFactory");

		when(mockBeanFactory.getConversionService()).thenReturn(mockConversionService);
		when(mockBeanFactory.getBeanClassLoader()).thenReturn(Thread.currentThread().getContextClassLoader());

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				BeanResolver beanResolver = invocation.getArgument(0);
				assertThat(beanResolver, is(instanceOf(BeanFactoryResolver.class)));
				assertThat(TestUtils.<ConfigurableBeanFactory>readField("beanFactory", beanResolver),
					is(equalTo(mockBeanFactory)));
				return null;
			}
		}).when(mockEvaluationContext).setBeanResolver(Matchers.any(BeanResolver.class));

		AnnotationBasedExpiration<Object, Object> annotationBasedExpiration = new AnnotationBasedExpiration<Object, Object>() {
			@Override StandardEvaluationContext newEvaluationContext() {
				return mockEvaluationContext;
			}
		};

		annotationBasedExpiration.setBeanFactory(mockBeanFactory);

		assertSame(mockBeanFactory, annotationBasedExpiration.getBeanFactory());

		verify(mockEvaluationContext, times(3)).addPropertyAccessor(Matchers.any(PropertyAccessor.class));
		verify(mockEvaluationContext, times(1)).setTypeConverter(Matchers.any(TypeConverter.class));
		verify(mockEvaluationContext, times(1)).setTypeLocator(Matchers.any(TypeLocator.class));
		verify(mockEvaluationContext, times(1)).setBeanResolver(Matchers.any(BeanResolver.class));
		verify(mockBeanFactory, times(1)).getConversionService();
		verify(mockBeanFactory, times(1)).getBeanClassLoader();
	}

	@Test(expected = IllegalStateException.class)
	public void getUninitializedBeanFactory() {
		new AnnotationBasedExpiration<Object, Object>().getBeanFactory();
	}

	@Test
	public void setAndGetDefaultExpirationAttributes() {
		ExpirationAttributes expectedExpirationAttributes = new ExpirationAttributes(120, ExpirationAction.INVALIDATE);
		AnnotationBasedExpiration expiration = new AnnotationBasedExpiration();

		expiration.setDefaultExpirationAttributes(expectedExpirationAttributes);

		assertThat(expiration.getDefaultExpirationAttributes(), is(equalTo(expectedExpirationAttributes)));

		expiration.setDefaultExpirationAttributes(null);

		assertThat(expiration.getDefaultExpirationAttributes(), is(nullValue()));

		expiration.setDefaultExpirationAttributes(ExpirationAttributes.DEFAULT);

		assertThat(expiration.getDefaultExpirationAttributes(), is(equalTo(ExpirationAttributes.DEFAULT)));
	}

	@Test
	public void getExpiryCallsGetExpirationMetaDataOnRegionEntryFollowedByNewExpirationAttributes() {
		final ExpirationAttributes expectedExpirationAttributes = new ExpirationAttributes(60,
			ExpirationAction.LOCAL_DESTROY);

		final Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		AnnotationBasedExpiration expiration = new AnnotationBasedExpiration() {
			@Override protected ExpirationMetaData getExpirationMetaData(Region.Entry entry) {
				assertThat(entry, is(sameInstance(mockRegionEntry)));
				return ExpirationMetaData.from(expectedExpirationAttributes);
			}

			@Override protected ExpirationAttributes newExpirationAttributes(ExpirationMetaData expirationMetaData) {
				assertThat(expirationMetaData.timeout(), is(equalTo(expectedExpirationAttributes.getTimeout())));
				assertThat(expirationMetaData.expirationAction(), is(equalTo(expectedExpirationAttributes.getAction())));
				return expectedExpirationAttributes;
			}
		};

		assertThat(expiration.getExpiry(mockRegionEntry), is(equalTo(expectedExpirationAttributes)));
	}

	@Test
	public void isExpirationConfiguredWithGenericExpirationBasedRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithGenericExpiration());
		assertThat(noDefaultExpiration.isExpirationConfigured(mockRegionEntry), is(true));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutGenericExpiration());
		assertThat(noDefaultExpiration.isExpirationConfigured(mockRegionEntry), is(true));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void isExpirationConfiguredWithNoGenericExpirationRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutExpiration());
		assertThat(noDefaultExpiration.isExpirationConfigured(mockRegionEntry), is(false));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(noDefaultExpiration.isExpirationConfigured(mockRegionEntry), is(false));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void isIdleTimeoutConfiguredWithIdleTimeoutExpirationBasedRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithIdleTimeoutExpiration());
		assertThat(noDefaultExpiration.isIdleTimeoutConfigured(mockRegionEntry), is(true));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutGenericExpiration());
		assertThat(noDefaultExpiration.isIdleTimeoutConfigured(mockRegionEntry), is(true));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void isIdleTimeoutConfiguredWithNoIdleTimeoutExpirationRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveGenericExpiration());
		assertThat(noDefaultExpiration.isIdleTimeoutConfigured(mockRegionEntry), is(false));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(noDefaultExpiration.isIdleTimeoutConfigured(mockRegionEntry), is(false));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void isTimeToLiveConfiguredWithTimeToLiveExpirationBasedRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveExpiration());
		assertThat(noDefaultExpiration.isTimeToLiveConfigured(mockRegionEntry), is(true));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutGenericExpiration());
		assertThat(noDefaultExpiration.isTimeToLiveConfigured(mockRegionEntry), is(true));
		verify(mockRegionEntry, times(2)).getValue();
	}

	public void isTimeToLiveConfiguredWithNoTimeToLiveExpirationRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithIdleTimeoutGenericExpiration());
		assertThat(noDefaultExpiration.isTimeToLiveConfigured(mockRegionEntry), is(false));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(noDefaultExpiration.isTimeToLiveConfigured(mockRegionEntry), is(false));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void getExpirationWithGenericExpirationBasedRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithGenericExpiration());
		assertThat(noDefaultExpiration.getExpiration(mockRegionEntry), isA(Expiration.class));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutGenericExpiration());
		assertThat(noDefaultExpiration.getExpiration(mockRegionEntry), isA(Expiration.class));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void getExpirationWithNoGenericExpirationRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutExpiration());
		assertThat(noDefaultExpiration.getExpiration(mockRegionEntry), is(nullValue()));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(noDefaultExpiration.getExpiration(mockRegionEntry), is(nullValue()));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void getIdleTimeoutWithIdleTimeoutExpirationBasedRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithIdleTimeoutExpiration());
		assertThat(noDefaultExpiration.getIdleTimeout(mockRegionEntry), isA(IdleTimeoutExpiration.class));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutGenericExpiration());
		assertThat(noDefaultExpiration.getIdleTimeout(mockRegionEntry), isA(IdleTimeoutExpiration.class));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void getIdleTimeoutWithNoIdleTimeoutExpirationRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveGenericExpiration());
		assertThat(noDefaultExpiration.getIdleTimeout(mockRegionEntry), is(nullValue()));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(noDefaultExpiration.getIdleTimeout(mockRegionEntry), is(nullValue()));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void getTimeToLiveWithTimeToLiveExpirationBasedRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveExpiration());
		assertThat(noDefaultExpiration.getTimeToLive(mockRegionEntry), isA(TimeToLiveExpiration.class));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithTimeToLiveIdleTimeoutGenericExpiration());
		assertThat(noDefaultExpiration.getTimeToLive(mockRegionEntry), isA(TimeToLiveExpiration.class));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void getTimeToLiveWithNoTimeToLiveExpirationRegionEntry() {
		Region.Entry mockRegionEntry = mock(Region.Entry.class, "MockRegionEntry");

		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithIdleTimeoutGenericExpiration());
		assertThat(noDefaultExpiration.getTimeToLive(mockRegionEntry), is(nullValue()));
		when(mockRegionEntry.getValue()).thenReturn(new RegionEntryValueWithNoExpiration());
		assertThat(noDefaultExpiration.getTimeToLive(mockRegionEntry), is(nullValue()));
		verify(mockRegionEntry, times(2)).getValue();
	}

	@Test
	public void fromExpiration() {
		ExpirationMetaData expirationMetaData = ExpirationMetaData.from(
			RegionEntryValueWithGenericExpiration.class.getAnnotation(Expiration.class));

		assertExpiration(expirationMetaData, 60, ExpirationActionType.INVALIDATE);
	}

	@Test
	public void fromExpirationIdleTimeout() {
		ExpirationMetaData expirationMetaData = ExpirationMetaData.from(
			RegionEntryValueWithIdleTimeoutExpiration.class.getAnnotation(IdleTimeoutExpiration.class));

		assertExpiration(expirationMetaData, 120, ExpirationActionType.LOCAL_INVALIDATE);
	}

	@Test
	public void fromExpirationTimeToLive() {
		ExpirationMetaData expirationMetaData = ExpirationMetaData.from(
			RegionEntryValueWithTimeToLiveExpiration.class.getAnnotation(TimeToLiveExpiration.class));

		assertExpiration(expirationMetaData, 300, ExpirationActionType.LOCAL_DESTROY);
	}

	@Test
	public void toExpirationAttributes() {
		ExpirationMetaData expirationMetaData = new ExpirationMetaData(90, ExpirationActionType.DESTROY);

		assertExpiration(expirationMetaData.toExpirationAttributes(), expirationMetaData.timeout(),
			expirationMetaData.expirationAction());
	}

	@Expiration(timeout = "60", action = "INVALIDATE")
	@IdleTimeoutExpiration(timeout = "120", action = "LOCAL_INVALIDATE")
	@TimeToLiveExpiration(timeout = "300", action = "LOCAL_DESTROY")
	public static class RegionEntryValueWithTimeToLiveIdleTimeoutGenericExpiration {
	}

	@IdleTimeoutExpiration(timeout = "120", action = "LOCAL_INVALIDATE")
	@TimeToLiveExpiration(timeout = "300", action = "LOCAL_DESTROY")
	public static class RegionEntryValueWithTimeToLiveIdleTimeoutExpiration {
	}

	@Expiration(timeout = "60", action = "INVALIDATE")
	@TimeToLiveExpiration(timeout = "300", action = "LOCAL_DESTROY")
	public static class RegionEntryValueWithTimeToLiveGenericExpiration {
	}

	@TimeToLiveExpiration(timeout = "300", action = "LOCAL_DESTROY")
	public static class RegionEntryValueWithTimeToLiveExpiration {
	}

	@Expiration(timeout = "60", action = "INVALIDATE")
	@IdleTimeoutExpiration(timeout = "120", action = "LOCAL_INVALIDATE")
	public static class RegionEntryValueWithIdleTimeoutGenericExpiration {
	}

	@IdleTimeoutExpiration(timeout = "120", action = "LOCAL_INVALIDATE")
	public static class RegionEntryValueWithIdleTimeoutExpiration {
	}

	@Expiration(timeout = "60", action = "INVALIDATE")
	public static class RegionEntryValueWithGenericExpiration {
	}

	public static class RegionEntryValueWithNoExpiration {
	}

}
