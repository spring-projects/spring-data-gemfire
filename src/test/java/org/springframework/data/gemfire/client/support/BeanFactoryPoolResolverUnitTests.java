/*
 * Copyright 2020 the original author or authors.
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
package org.springframework.data.gemfire.client.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.apache.geode.cache.client.Pool;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanNotOfRequiredTypeException;

/**
 * Unit Tests for {@link BeanFactoryPoolResolver}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.data.gemfire.client.support.BeanFactoryPoolResolver
 * @since 2.3.0
 */
@RunWith(MockitoJUnitRunner.class)
public class BeanFactoryPoolResolverUnitTests {

	@Mock
	private BeanFactory mockBeanFactory;

	private BeanFactoryPoolResolver poolResolver;

	@Mock
	private Pool mockPool;

	@Before
	public void setup() {
		this.poolResolver = new BeanFactoryPoolResolver(this.mockBeanFactory);
	}

	@Test
	public void constructBeanFactoryPoolResolverWithBeanFactory() {

		BeanFactoryPoolResolver poolResolver = new BeanFactoryPoolResolver(this.mockBeanFactory);

		assertThat(poolResolver).isNotNull();
		assertThat(poolResolver.getBeanFactory()).isEqualTo(this.mockBeanFactory);
	}

	@SuppressWarnings("all")
	@Test(expected = IllegalArgumentException.class)
	public void constructBeanFactoryPoolResolverWithNullThrowsIllegalArgumentException() {

		try {
			new BeanFactoryPoolResolver(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("BeanFactory must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void setAndGetBeanFactory() {

		BeanFactory mockBeanFactoryTwo = mock(BeanFactory.class);

		assertThat(this.poolResolver.getBeanFactory()).isEqualTo(this.mockBeanFactory);

		this.poolResolver.setBeanFactory(mockBeanFactoryTwo);

		assertThat(this.poolResolver.getBeanFactory()).isEqualTo(mockBeanFactoryTwo);
	}

	@SuppressWarnings("all")
	@Test(expected = IllegalArgumentException.class)
	public void setBeanFactoryToNullThrowsIllegalArgumentException() {

		try {
			assertThat(this.poolResolver.getBeanFactory()).isEqualTo(this.mockBeanFactory);
			this.poolResolver.setBeanFactory(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("BeanFactory must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			assertThat(this.poolResolver.getBeanFactory()).isEqualTo(this.mockBeanFactory);
		}
	}

	@Test
	public void resolveResolvablePoolByName() {

		when(this.mockBeanFactory.containsBean(eq("TestPool"))).thenReturn(true);
		when(this.mockBeanFactory.getBean(eq("TestPool"), eq(Pool.class))).thenReturn(this.mockPool);

		assertThat(this.poolResolver.resolve("TestPool")).isEqualTo(this.mockPool);

		verify(this.mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(this.mockBeanFactory, times(1)).getBean(eq("TestPool"), eq(Pool.class));
		verifyZeroInteractions(this.mockPool);
	}

	private Pool testResolveUnresolvablePoolByInvalidName(String poolName) {

		try {
			return this.poolResolver.resolve(poolName);
		}
		finally {
			verify(this.mockBeanFactory, never()).containsBean(anyString());
			verify(this.mockBeanFactory, never()).getBean(anyString(), any(Pool.class));
		}
	}

	@Test
	public void resolveUnresolvablePoolByBlankNameReturnsNull() {
		assertThat(testResolveUnresolvablePoolByInvalidName("  ")).isNull();
	}

	@Test
	public void resolveUnresolvablePoolByEmptyNameReturnsNull() {
		assertThat(testResolveUnresolvablePoolByInvalidName("")).isNull();
	}

	@Test
	public void resolveUnresolvablePoolByNullNameReturnsNull() {
		assertThat(testResolveUnresolvablePoolByInvalidName(null)).isNull();
	}

	@Test
	public void resolvePoolWhenBeanFactoryDoesNotContainPoolBeanByNameReturnsNull() {

		when(this.mockBeanFactory.containsBean(anyString())).thenReturn(false);

		assertThat(this.poolResolver.resolve("TestPool")).isNull();

		verify(this.mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(this.mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
	}

	@Test(expected = BeanNotOfRequiredTypeException.class)
	public void resolvePoolWhenBeanFactoryContainsBeanByPoolNameButNotAsAPoolType() {

		when(this.mockBeanFactory.containsBean(anyString())).thenReturn(true);
		when(this.mockBeanFactory.getBean(anyString(), eq(Pool.class)))
			.thenThrow(new BeanNotOfRequiredTypeException("TestPool", Pool.class, Object.class));

		this.poolResolver.resolve("TestPool");
	}
}
