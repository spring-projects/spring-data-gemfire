/*
 * Copyright 2017-2019 the original author or authors.
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
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.geode.cache.GemFireCache;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.annotation.AdviceMode;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.annotation.support.GemFireAsLastResourceConnectionAcquiringAspect;
import org.springframework.data.gemfire.config.annotation.support.GemFireAsLastResourceConnectionClosingAspect;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 * Unit tests for {@link GemFireAsLastResourceConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.annotation.GemFireAsLastResourceConfiguration
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class GemFireAsLastResourceConfigurationUnitTests {

	@Mock
	private AnnotationMetadata mockImportMetadata;

	private GemFireAsLastResourceConfiguration configuration;

	@Before
	public void setup() {
		configuration = new GemFireAsLastResourceConfiguration();
	}

	private AnnotationMetadata mockEnableTransactionManagementWithOrder(Integer order) {

		Map<String, Object> enableTransactionManagementAttributes = Collections.singletonMap("order", order);

		when(mockImportMetadata.getAnnotationAttributes(eq(EnableTransactionManagement.class.getName())))
			.thenReturn(enableTransactionManagementAttributes);

		return mockImportMetadata;
	}

	@Test
	public void resolveEnableTransactionManagementAttributes() {

		Map<String, Object> enableTransactionManagementAttributesMap = new HashMap<>();

		enableTransactionManagementAttributesMap.put("mode", AdviceMode.ASPECTJ);
		enableTransactionManagementAttributesMap.put("order", 1);
		enableTransactionManagementAttributesMap.put("proxyTargetClass", true);

		when(mockImportMetadata.getAnnotationAttributes(eq(EnableTransactionManagement.class.getName())))
			.thenReturn(enableTransactionManagementAttributesMap);

		AnnotationAttributes enableTransactionManagementAttributes =
			configuration.resolveEnableTransactionManagementAttributes(mockImportMetadata);

		assertThat(enableTransactionManagementAttributes).isNotNull();
		assertThat(enableTransactionManagementAttributes.<AdviceMode>getEnum("mode"))
			.isEqualTo(AdviceMode.ASPECTJ);
		assertThat(enableTransactionManagementAttributes.getBoolean("proxyTargetClass")).isTrue();

		verify(mockImportMetadata, times(1))
			.getAnnotationAttributes(eq(EnableTransactionManagement.class.getName()));
	}

	@Test(expected = IllegalStateException.class)
	public void resolveEnableTransactionManagementAttributesThrowsIllegalStateException() {
		try {
			when(mockImportMetadata.getAnnotationAttributes(anyString())).thenReturn(null);

			configuration.resolveEnableTransactionManagementAttributes(mockImportMetadata);
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage(String.format(
				"The @%1$s annotation may only be used on a Spring application @%2$s class"
					+ " that is also annotated with @%3$s having an explicit [order] set",
				EnableGemFireAsLastResource.class.getSimpleName(), Configuration.class.getSimpleName(),
				EnableTransactionManagement.class.getSimpleName()));

			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void resolveEnableTransactionManagementOrderEqualsOne() {
		assertThat(configuration.resolveEnableTransactionManagementOrder(
			mockEnableTransactionManagementWithOrder(1))).isEqualTo(1);
	}

	@Test(expected = IllegalArgumentException.class)
	public void resolveEnableTransactionManagementOrderThrowsIllegalArgumentExceptionForIntegerMaxValue() {
		try {
			configuration.resolveEnableTransactionManagementOrder(
				mockEnableTransactionManagementWithOrder(Integer.MAX_VALUE));
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage(String.format(
				"The @%1$s(order) attribute value [%2$d] must be explicitly set to a value other than"
					+ " Integer.MAX_VALUE or Integer.MIN_VALUE",
				EnableTransactionManagement.class.getSimpleName(), Integer.MAX_VALUE));

			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void resolveEnableTransactionManagementOrderThrowsIllegalArgumentExceptionForIntegerMinValue() {
		try {
			configuration.resolveEnableTransactionManagementOrder(
				mockEnableTransactionManagementWithOrder(Integer.MIN_VALUE));
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage(String.format(
				"The @%1$s(order) attribute value [%2$d] must be explicitly set to a value other than"
					+ " Integer.MAX_VALUE or Integer.MIN_VALUE",
				EnableTransactionManagement.class.getSimpleName(), Integer.MIN_VALUE));

			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void getEnableTransactionManagementOrderReturnsValue() {

		configuration.setImportMetadata(mockEnableTransactionManagementWithOrder(101));

		assertThat(configuration.getEnableTransactionManagementOrder()).isEqualTo(101);
	}

	@Test(expected = IllegalStateException.class)
	public void getEnableTransactionManagementOrderThrowsIllegalStateException() {
		try {
			configuration.getEnableTransactionManagementOrder();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage(String.format("The @%1$s(order) attribute was not properly set [null];"
				+ " Also, please make your Spring application @%2$s annotated class is annotated with both @%3$s and @%1$s",
				EnableTransactionManagement.class.getSimpleName(), Configuration.class.getSimpleName(),
				EnableGemFireAsLastResource.class.getSimpleName()));

			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void gemfireCachePostProcessorSetsCopyOnReadToTrue() {

		GemFireCache mockGemFireCache = mock(GemFireCache.class);

		assertThat(configuration.gemfireCachePostProcessor(mockGemFireCache)).isNull();

		verify(mockGemFireCache, times(1)).setCopyOnRead(eq(true));
	}

	@Test
	public void gemfireCachePostProcessorHandlesNull() {
		assertThat(configuration.gemfireCachePostProcessor(null)).isNull();
	}

	@Test
	public void gemfireJcaConnectionAcquiringAspectIsOrderPlusOne() {

		configuration.setImportMetadata(mockEnableTransactionManagementWithOrder(0));

		GemFireAsLastResourceConnectionAcquiringAspect aspect = configuration.gemfireJcaConnectionAcquiringAspect();

		assertThat(aspect).isNotNull();
		assertThat(aspect.getOrder()).isEqualTo(1);
	}

	@Test
	public void gemfireJcaConnectionClosingAspectIsOrderMinusOne() {

		configuration.setImportMetadata(mockEnableTransactionManagementWithOrder(0));

		GemFireAsLastResourceConnectionClosingAspect aspect = configuration.gemfireJcaConnectionClosingAspect();

		assertThat(aspect).isNotNull();
		assertThat(aspect.getOrder()).isEqualTo(-1);
	}
}
