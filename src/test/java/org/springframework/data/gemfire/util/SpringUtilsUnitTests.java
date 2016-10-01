/*
 * Copyright 2012 the original author or authors.
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

package org.springframework.data.gemfire.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.data.gemfire.test.support.MockitoMatchers;

/**
 * Unit tests for {@link SpringUtils}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.util.SpringUtils
 * @since 1.9.0
 */
@RunWith(MockitoJUnitRunner.class)
public class SpringUtilsUnitTests {

	@Mock
	private BeanDefinition mockBeanDefinition;

	@Test
	public void addDependsOnToExistingDependencies() {
		when(mockBeanDefinition.getDependsOn()).thenReturn(ArrayUtils.asArray("testBeanNameOne", "testBeanNameTwo"));
		assertThat(SpringUtils.addDependsOn(mockBeanDefinition, "testBeanNameThree")).isSameAs(mockBeanDefinition);
		verify(mockBeanDefinition, times(1)).getDependsOn();
		verify(mockBeanDefinition, times(1)).setDependsOn(argThat(
			MockitoMatchers.stringArrayMatcher("testBeanNameOne", "testBeanNameTwo", "testBeanNameThree")));
	}

	@Test
	public void addDependsOnToNonExistingDependencies() {
		when(mockBeanDefinition.getDependsOn()).thenReturn(null);
		assertThat(SpringUtils.addDependsOn(mockBeanDefinition, "testBeanName")).isSameAs(mockBeanDefinition);
		verify(mockBeanDefinition, times(1)).getDependsOn();
		verify(mockBeanDefinition, times(1)).setDependsOn(argThat(MockitoMatchers.stringArrayMatcher("testBeanName")));
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
	public void dereferenceBean() {
		assertThat(SpringUtils.dereferenceBean("example")).isEqualTo("&example");
	}
}
