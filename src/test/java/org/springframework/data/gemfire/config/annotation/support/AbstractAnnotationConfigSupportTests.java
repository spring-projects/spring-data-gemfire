/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link AbstractAnnotationConfigSupport}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @since 1.1.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractAnnotationConfigSupportTests {

	@Mock
	private AbstractAnnotationConfigSupport support;

	@Test
	public void requirePropertyWithNonStringValueIsSuccessful() {
		when(support.requireProperty(anyString(), any())).thenCallRealMethod();
		when(support.resolveProperty(anyString(), eq(Integer.class), eq(null))).thenReturn(1);

		assertThat(support.requireProperty("key", Integer.class)).isEqualTo(1);

		verify(support, times(1))
			.resolveProperty(eq("key"), eq(Integer.class), eq(null));
	}

	@Test
	public void requirePropertyWithStringValueIsSuccessful() {
		when(support.requireProperty(anyString(), any())).thenCallRealMethod();
		when(support.resolveProperty(anyString(), eq(String.class), eq(null))).thenReturn("test");

		assertThat(support.requireProperty("key", String.class)).isEqualTo("test");

		verify(support, times(1))
			.resolveProperty(eq("key"), eq(String.class), eq(null));
	}

	@Test(expected = IllegalArgumentException.class)
	public void requirePropertyWithEmptyStringThrowsIllegalArgumentException() {
		when(support.requireProperty(anyString(), any())).thenCallRealMethod();
		when(support.resolveProperty(anyString(), eq(String.class), eq(null))).thenReturn("  ");

		try {
			support.requireProperty("key", String.class);
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("Property [key] is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(support, times(1))
				.resolveProperty(eq("key"), eq(String.class), eq(null));
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void requirePropertyWithNullValueThrowsIllegalArgumentException() {
		when(support.requireProperty(anyString(), any())).thenCallRealMethod();
		when(support.resolveProperty(anyString(), any(), eq(null))).thenReturn(null);

		try {
			support.requireProperty("key", Integer.class);
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("Property [key] is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(support, times(1))
				.resolveProperty(eq("key"), eq(Integer.class), eq(null));
		}
	}
}
