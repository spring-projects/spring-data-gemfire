/*
 * Copyright 2012-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.ArrayUtils.asArray;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newRuntimeException;

import java.util.Collections;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.MutablePropertyValues;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.RuntimeBeanReference;

/**
 * Unit tests for {@link SpringUtils}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.util.SpringUtils
 * @since 1.9.0
 */
@RunWith(MockitoJUnitRunner.class)
public class SpringUtilsUnitTests {

	@Mock
	private BeanDefinition mockBeanDefinition;

	@Test
	public void addDependsOnToExistingDependencies() {

		when(mockBeanDefinition.getDependsOn()).thenReturn(asArray("testBeanNameOne", "testBeanNameTwo"));

		assertThat(SpringUtils.addDependsOn(mockBeanDefinition, "testBeanNameThree"))
			.isSameAs(mockBeanDefinition);

		verify(mockBeanDefinition, times(1)).getDependsOn();
		verify(mockBeanDefinition, times(1))
			.setDependsOn("testBeanNameOne", "testBeanNameTwo", "testBeanNameThree");
	}

	@Test
	public void addDependsOnToNonExistingDependencies() {

		when(mockBeanDefinition.getDependsOn()).thenReturn(null);

		assertThat(SpringUtils.addDependsOn(mockBeanDefinition, "testBeanName"))
			.isSameAs(mockBeanDefinition);

		verify(mockBeanDefinition, times(1)).getDependsOn();
		verify(mockBeanDefinition, times(1)).setDependsOn("testBeanName");
	}

	@Test
	public void addDependsOnWithMultipleDependenciesWithExistingDependencies() {

		when(mockBeanDefinition.getDependsOn()).thenReturn(asArray("testBeanNameOne", "testBeanNameTwo"));

		assertThat(SpringUtils.addDependsOn(mockBeanDefinition, "testBeanNameThree", "testBeanNameFour"))
			.isSameAs(mockBeanDefinition);

		verify(mockBeanDefinition, times(1)).getDependsOn();
		verify(mockBeanDefinition, times(1))
			.setDependsOn("testBeanNameOne", "testBeanNameTwo", "testBeanNameThree", "testBeanNameFour");
	}

	@Test
	public void getPropertyValueForExistingPropertyHavingValueReturnsValue() {

		MutablePropertyValues propertyValues =
			new MutablePropertyValues(Collections.singletonMap("testProperty", "testValue"));

		when(mockBeanDefinition.getPropertyValues()).thenReturn(propertyValues);

		assertThat(SpringUtils.getPropertyValue(mockBeanDefinition, "testProperty").orElse(null))
			.isEqualTo("testValue");

		verify(mockBeanDefinition, times(1)).getPropertyValues();
	}

	@Test
	public void getPropertyValueForExistingPropertyHavingNullValueReturnsNull() {

		MutablePropertyValues testPropertyValues = spy(new MutablePropertyValues());

		PropertyValue testPropertyValue = spy(new PropertyValue("testProperty", null));

		when(mockBeanDefinition.getPropertyValues()).thenReturn(testPropertyValues);
		doReturn(testPropertyValue).when(testPropertyValues).getPropertyValue(anyString());

		assertThat(SpringUtils.getPropertyValue(mockBeanDefinition, "testProperty").orElse(null))
			.isNull();

		verify(mockBeanDefinition, times(1)).getPropertyValues();
		verify(testPropertyValues, times(1)).getPropertyValue(eq("testProperty"));
		verify(testPropertyValue, times(1)).getValue();
	}

	@Test
	public void getPropertyValueForNonExistingPropertyReturnsNull() {

		MutablePropertyValues testPropertyValues = spy(new MutablePropertyValues());

		when(mockBeanDefinition.getPropertyValues()).thenReturn(testPropertyValues);

		assertThat(SpringUtils.getPropertyValue(mockBeanDefinition, "testProperty").orElse(null))
			.isNull();

		verify(mockBeanDefinition, times(1)).getPropertyValues();
		verify(testPropertyValues, times(1)).getPropertyValue(eq("testProperty"));
	}

	@Test
	public void getPropertyValueWithNullPropertyValuesReturnsNull() {

		when(mockBeanDefinition.getPropertyValues()).thenReturn(null);

		assertThat(SpringUtils.getPropertyValue(mockBeanDefinition, "testProperty").orElse(null))
			.isNull();

		verify(mockBeanDefinition, times(1)).getPropertyValues();
	}

	@Test
	public void getPropertyValueWithNullBeanDefinitionReturnsNull() {
		assertThat(SpringUtils.getPropertyValue(null, "testProperty").orElse(null))
			.isNull();
	}

	@Test
	@SuppressWarnings("all")
	public void setBeanDefinitionPropertyReference() {

		BeanDefinition mockBeanDefinition = mock(BeanDefinition.class);

		MutablePropertyValues mutablePropertyValues = new MutablePropertyValues();

		when(mockBeanDefinition.getPropertyValues()).thenReturn(mutablePropertyValues);

		assertThat(mutablePropertyValues.size()).isEqualTo(0);

		SpringUtils.setPropertyReference(mockBeanDefinition, "testProperty", "testBean");

		assertThat(mutablePropertyValues.size()).isEqualTo(1);
		assertThat(mutablePropertyValues.getPropertyValue("testProperty")).isNotNull();
		assertThat(mutablePropertyValues.getPropertyValue("testProperty").getValue())
			.isInstanceOf(RuntimeBeanReference.class);
		assertThat(((RuntimeBeanReference) mutablePropertyValues.getPropertyValue("testProperty").getValue()).getBeanName())
			.isEqualTo("testBean");
	}

	@Test
	@SuppressWarnings("all")
	public void setBeanDefinitionPropertyValue() {

		BeanDefinition mockBeanDefinition = mock(BeanDefinition.class);

		MutablePropertyValues mutablePropertyValues = new MutablePropertyValues();

		when(mockBeanDefinition.getPropertyValues()).thenReturn(mutablePropertyValues);

		assertThat(mutablePropertyValues.size()).isEqualTo(0);

		SpringUtils.setPropertyValue(mockBeanDefinition, "testProperty", "testValue");

		assertThat(mutablePropertyValues.size()).isEqualTo(1);
		assertThat(mutablePropertyValues.getPropertyValue("testProperty")).isNotNull();
		assertThat(mutablePropertyValues.getPropertyValue("testProperty").getValue())
			.isEqualTo("testValue");
	}

	@Test
	public void defaultIfEmptyReturnsValue() {

		assertThat(SpringUtils.defaultIfEmpty("test", "DEFAULT")).isEqualTo("test");
		assertThat(SpringUtils.defaultIfEmpty("abc123", "DEFAULT")).isEqualTo("abc123");
		assertThat(SpringUtils.defaultIfEmpty("123", "DEFAULT")).isEqualTo("123");
		assertThat(SpringUtils.defaultIfEmpty("X", "DEFAULT")).isEqualTo("X");
		assertThat(SpringUtils.defaultIfEmpty("$", "DEFAULT")).isEqualTo("$");
		assertThat(SpringUtils.defaultIfEmpty("_", "DEFAULT")).isEqualTo("_");
		assertThat(SpringUtils.defaultIfEmpty("nil", "DEFAULT")).isEqualTo("nil");
		assertThat(SpringUtils.defaultIfEmpty("null", "DEFAULT")).isEqualTo("null");
	}

	@Test
	public void defaultIfEmptyReturnsDefault() {

		assertThat(SpringUtils.defaultIfEmpty("  ", "DEFAULT")).isEqualTo("DEFAULT");
		assertThat(SpringUtils.defaultIfEmpty("", "DEFAULT")).isEqualTo("DEFAULT");
		assertThat(SpringUtils.defaultIfEmpty(null, "DEFAULT")).isEqualTo("DEFAULT");
	}

	@Test
	public void defaultIfNullReturnsValue() {

		assertThat(SpringUtils.defaultIfNull(true, false)).isTrue();
		assertThat(SpringUtils.defaultIfNull('x', 'A')).isEqualTo('x');
		assertThat(SpringUtils.defaultIfNull(1, 2)).isEqualTo(1);
		assertThat(SpringUtils.defaultIfNull(Math.PI, 2.0d)).isEqualTo(Math.PI);
		assertThat(SpringUtils.defaultIfNull("test", "DEFAULT")).isEqualTo("test");
	}

	@Test
	public void defaultIfNullReturnsDefault() {

		assertThat(SpringUtils.defaultIfNull(null, false)).isFalse();
		assertThat(SpringUtils.defaultIfNull(null, 'A')).isEqualTo('A');
		assertThat(SpringUtils.defaultIfNull(null, 2)).isEqualTo(2);
		assertThat(SpringUtils.defaultIfNull(null, 2.0d)).isEqualTo(2.0d);
		assertThat(SpringUtils.defaultIfNull(null, "DEFAULT")).isEqualTo("DEFAULT");
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWithSupplierReturnsValue() {

		Supplier<String> mockSupplier = mock(Supplier.class);

		assertThat(SpringUtils.defaultIfNull("value", mockSupplier)).isEqualTo("value");

		verify(mockSupplier, never()).get();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWithSupplierReturnsSupplierValue() {

		Supplier<String> mockSupplier = mock(Supplier.class);

		when(mockSupplier.get()).thenReturn("supplier");

		assertThat(SpringUtils.defaultIfNull(null, mockSupplier)).isEqualTo("supplier");

		verify(mockSupplier, times(1)).get();
	}

	@Test
	public void dereferenceBean() {
		assertThat(SpringUtils.dereferenceBean("example")).isEqualTo("&example");
	}

	@Test
	public void equalsIgnoreNullIsTrue() {

		assertThat(SpringUtils.equalsIgnoreNull(null, null)).isTrue();
		assertThat(SpringUtils.equalsIgnoreNull(true, true)).isTrue();
		assertThat(SpringUtils.equalsIgnoreNull('x', 'x')).isTrue();
		assertThat(SpringUtils.equalsIgnoreNull(1, 1)).isTrue();
		assertThat(SpringUtils.equalsIgnoreNull(Math.PI, Math.PI)).isTrue();
		assertThat(SpringUtils.equalsIgnoreNull("null", "null")).isTrue();
		assertThat(SpringUtils.equalsIgnoreNull("test", "test")).isTrue();
	}

	@Test
	public void equalsIgnoreNullIsFalse() {

		assertThat(SpringUtils.equalsIgnoreNull(null, "null")).isFalse();
		assertThat(SpringUtils.equalsIgnoreNull(true, false)).isFalse();
		assertThat(SpringUtils.equalsIgnoreNull('x', 'X')).isFalse();
		assertThat(SpringUtils.equalsIgnoreNull(1, 2)).isFalse();
		assertThat(SpringUtils.equalsIgnoreNull(3.14159d, Math.PI)).isFalse();
		assertThat(SpringUtils.equalsIgnoreNull("nil", "null")).isFalse();
	}

	@Test
	public void nullOrEqualsWithEqualObjectsIsTrue() {
		assertThat(SpringUtils.nullOrEquals("test", "test")).isTrue();
	}

	@Test
	public void nullOrEqualsWithNonNullObjectAndNullIsFalse() {
		assertThat(SpringUtils.nullOrEquals("test", null)).isFalse();
	}

	@Test
	public void nullOrEqualsWithNullIsTrue() {
		assertThat(SpringUtils.nullOrEquals(null, "test")).isTrue();
	}

	@Test
	public void nullOrEqualsWithUnequalObjectsIsFalse() {
		assertThat(SpringUtils.nullOrEquals("test", "mock")).isFalse();
	}

	@Test
	public void nullSafeEqualsWithEqualObjectsIsTrue() {
		assertThat(SpringUtils.nullSafeEquals("test", "test")).isTrue();
	}

	@Test
	public void nullSafeEqualsWithNullObjectsIsFalse() {
		assertThat(SpringUtils.nullSafeEquals(null, "test")).isFalse();
		assertThat(SpringUtils.nullSafeEquals("test", null)).isFalse();
	}

	@Test
	public void nullSafeEqualsWithUnequalObjectsIsFalse() {
		assertThat(SpringUtils.nullSafeEquals("test", "mock")).isFalse();
	}

	@Test
	public void safeGetValueReturnsSuppliedValue() {
		assertThat(SpringUtils.safeGetValue(() -> "test")).isEqualTo("test");
	}

	@Test
	public void safeGetValueReturnsNull() {
		assertThat(SpringUtils.<Object>safeGetValue(() -> { throw newRuntimeException("error"); })).isNull();
	}

	@Test
	public void safeGetValueReturnsDefaultValue() {
		assertThat(SpringUtils.safeGetValue(() -> { throw newRuntimeException("error"); },  "test"))
			.isEqualTo("test");
	}

	@Test
	public void safeGetValueReturnsSuppliedDefaultValue() {

		Supplier<String> exceptionThrowingSupplier = () -> { throw newRuntimeException("error"); };
		Supplier<String> defaultValueSupplier = () -> "test";

		assertThat(SpringUtils.safeGetValue(exceptionThrowingSupplier, defaultValueSupplier)).isEqualTo("test");
	}

	@Test
	public void safeGetValueHandlesExceptionReturnsValue() {

		Supplier<String> exceptionThrowingSupplier = () -> { throw newRuntimeException("error"); };

		Function<Throwable, String> exceptionHandler = exception -> {

			assertThat(exception).isInstanceOf(RuntimeException.class);
			assertThat(exception).hasMessage("error");
			assertThat(exception).hasNoCause();

			return "test";
		};

		assertThat(SpringUtils.safeGetValue(exceptionThrowingSupplier, exceptionHandler)).isEqualTo("test");
	}

	@Test(expected = IllegalStateException.class)
	public void safeGetValueHandlesExceptionAndCanThrowException() {

		Supplier<String> exceptionThrowingSupplier = () -> { throw newRuntimeException("error"); };

		Function<Throwable, String> exceptionHandler = exception -> {

			assertThat(exception).isInstanceOf(RuntimeException.class);
			assertThat(exception).hasMessage("error");
			assertThat(exception).hasNoCause();

			throw newIllegalStateException(exception, "test");
		};

		try {
			SpringUtils.safeGetValue(exceptionThrowingSupplier, exceptionHandler);
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage("test");
			assertThat(expected).hasCauseInstanceOf(RuntimeException.class);
			assertThat(expected.getCause()).hasMessage("error");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;
		}
	}
}
