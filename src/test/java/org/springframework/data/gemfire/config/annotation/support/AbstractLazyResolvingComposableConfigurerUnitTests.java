/*
 * Copyright 2018 the original author or authors.
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
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.core.Ordered;
import org.springframework.lang.Nullable;

/**
 * Unit Tests for {@link AbstractLazyResolvingComposableConfigurer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.ListableBeanFactory
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer
 * @since 2.2.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractLazyResolvingComposableConfigurerUnitTests {

	@Mock
	private ListableBeanFactory mockBeanFactory;

	private AbstractLazyResolvingComposableConfigurer<TestFactoryBean, TestConfigurer> configurer;

	@Before
	public void setup() {
		this.configurer = new TestLazyWiringResolvingComposableConfigurer();
	}

	@Test
	public void withBeanFactoryReturnsThis() {

		assertThat(this.configurer.getBeanFactory().orElse(null)).isNull();
		assertThat(this.configurer.<AbstractLazyResolvingComposableConfigurer<TestFactoryBean, TestConfigurer>>with(this.mockBeanFactory))
			.isEqualTo(this.configurer);
		assertThat(this.configurer.getBeanFactory().orElse(null)).isEqualTo(this.mockBeanFactory);
	}

	@Test
	public void setAndGetBeanFactoryIsCorrect() {

		assertThat(this.configurer.getBeanFactory().orElse(null)).isNull();

		this.configurer.setBeanFactory(this.mockBeanFactory);

		assertThat(this.configurer.getBeanFactory().orElse(null)).isEqualTo(this.mockBeanFactory);

		this.configurer.setBeanFactory(null);

		assertThat(this.configurer.getBeanFactory().orElse(null)).isNull();
	}

	@Test
	public void resolvesConfigurersIsCorrect() {

		TestConfigurer mockConfigurerOne = mock(TestConfigurer.class);
		TestConfigurer mockConfigurerTwo = mock(TestConfigurer.class);

		Map<String, TestConfigurer> beansOfType = new HashMap<>();

		beansOfType.put("MockConfigurerOne", mockConfigurerOne);
		beansOfType.put("MockConfigurerTwo", mockConfigurerTwo);

		when(this.mockBeanFactory.getBeansOfType(eq(TestConfigurer.class), anyBoolean(), anyBoolean()))
			.thenReturn(beansOfType);

		this.configurer.setBeanFactory(this.mockBeanFactory);

		Stream<TestConfigurer> streamOne = this.configurer.resolveConfigurers();

		assertThat(streamOne).isNotNull();
		assertThat(streamOne).containsExactlyInAnyOrder(mockConfigurerOne, mockConfigurerTwo);

		Stream<TestConfigurer> streamTwo = this.configurer.resolveConfigurers();

		assertThat(streamTwo).isNotNull();
		assertThat(streamTwo).containsExactlyInAnyOrder(mockConfigurerOne, mockConfigurerTwo);

		verify(this.mockBeanFactory, times(1))
			.getBeansOfType(eq(TestConfigurer.class), eq(true), eq(false));
		verify(mockConfigurerOne, atLeastOnce()).getOrder();
		verify(mockConfigurerTwo, atLeastOnce()).getOrder();
	}

	@Test
	public void resolvesConfigurersWithNonListableBeanFactoryReturnsEmptyStream() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		this.configurer.setBeanFactory(mockBeanFactory);

		assertThat(this.configurer.getBeanFactory().orElse(null)).isEqualTo(mockBeanFactory);
		assertThat(this.configurer.getBeanFactory().orElse(null)).isNotInstanceOf(ListableBeanFactory.class);

		Stream<TestConfigurer> stream = this.configurer.resolveConfigurers();

		assertThat(stream).isNotNull();
		assertThat(stream).isEmpty();
	}

	@Test
	public void resolvesConfigurersWithNullBeanFactoryReturnsEmptyStream() {

		assertThat(this.configurer.getBeanFactory().orElse(null)).isNull();

		Stream<TestConfigurer> stream = this.configurer.resolveConfigurers();

		assertThat(stream).isNotNull();
		assertThat(stream).isEmpty();
	}

	@Test
	public void configureConfiguresTestFactoryBeanWithComposedConfigurers() {

		TestConfigurer mockConfigurerOne = mock(TestConfigurer.class);
		TestConfigurer mockConfigurerTwo = mock(TestConfigurer.class);

		when(mockConfigurerOne.getOrder()).thenReturn(2);
		when(mockConfigurerTwo.getOrder()).thenReturn(1);

		Map<String, TestConfigurer> beansOfType = new HashMap<>();

		beansOfType.put("MockConfigurerOne", mockConfigurerOne);
		beansOfType.put("MockConfigurerTwo", mockConfigurerTwo);

		when(this.mockBeanFactory.getBeansOfType(eq(TestConfigurer.class), anyBoolean(), anyBoolean()))
			.thenReturn(beansOfType);

		TestFactoryBean testFactoryBean = new TestFactoryBean();

		this.configurer.setBeanFactory(this.mockBeanFactory);
		this.configurer.configure("TestBean", testFactoryBean);

		InOrder inOrder = Mockito.inOrder(this.mockBeanFactory, mockConfigurerOne, mockConfigurerTwo);

		inOrder.verify(this.mockBeanFactory, times(1))
			.getBeansOfType(eq(TestConfigurer.class), eq(true), eq(false));

		inOrder.verify(mockConfigurerTwo, times(1))
			.configure(eq("TestBean"), eq(testFactoryBean));

		inOrder.verify(mockConfigurerOne, times(1))
			.configure(eq("TestBean"), eq(testFactoryBean));
	}

	class TestFactoryBean implements FactoryBean<Object> {

		@Nullable @Override
		public Object getObject() {
			return null;
		}

		@Nullable @Override
		public Class<?> getObjectType() {
			return Object.class;
		}
	}

	interface TestConfigurer extends Configurer<TestFactoryBean>, Ordered { }

	class TestLazyWiringResolvingComposableConfigurer
			extends AbstractLazyResolvingComposableConfigurer<TestFactoryBean, TestConfigurer> {

		@Override
		protected Class<TestConfigurer> getConfigurerType() {
			return TestConfigurer.class;
		}
	}
}
