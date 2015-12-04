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
 */

package org.springframework.data.gemfire.repository.support;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Matchers;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;
import org.springframework.data.mapping.context.MappingContext;

import com.gemstone.gemfire.cache.Region;

/**
 * The GemfireRepositoryFactoryBeanTest class is test suite of test cases testing the contract and functionality
 * of the GemfireRepositoryFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean
 * @since 1.6.3
 */
public class GemfireRepositoryFactoryBeanTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private GemfireRepositoryFactoryBean repositoryFactoryBean;

	@Before
	public void setup() {
		repositoryFactoryBean = new GemfireRepositoryFactoryBean();
	}

	protected <T> List<T> toList(Iterable<T> iterable) {
		List<T> list = new ArrayList<T>();

		if (iterable != null) {
			for (T element : iterable) {
				list.add(element);
			}
		}

		return list;
	}

	@Test
	@SuppressWarnings("unchecked")
	public void setAndGetApplicationContextSuccessfully() {
		Map<String, Region> expectedRegions = new HashMap<String, Region>(2);

		expectedRegions.put("regionOne", mock(Region.class));
		expectedRegions.put("regionTwo", mock(Region.class));

		ApplicationContext mockApplicationContext = mock(ApplicationContext.class);

		when(mockApplicationContext.getBeansOfType(eq(Region.class))).thenReturn(expectedRegions);

		repositoryFactoryBean.setApplicationContext(mockApplicationContext);

		Iterable<Region> actualRegions = repositoryFactoryBean.getRegions();

		assertThat(actualRegions, is(notNullValue()));

		List<Region> regions = toList(actualRegions);

		assertThat(regions.size(), is(equalTo(2)));
		assertThat(regions.containsAll(expectedRegions.values()), is(true));

		ApplicationContext actualApplicationContext = repositoryFactoryBean.getApplicationContext();

		assertThat(actualApplicationContext, is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, times(1)).getBeansOfType(eq(Region.class));
	}

	@Test
	public void getNullApplicationContextThrowsIllegalStateException() {
		expectedException.expect(IllegalStateException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("The Spring ApplicationContext was not properly initialized");
		repositoryFactoryBean.getApplicationContext();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void setGemfireMappingContextAlsoSetsMappingContext() throws Exception {
		MappingContext mockMappingContext = mock(MappingContext.class);

		repositoryFactoryBean.setGemfireMappingContext(mockMappingContext);

		MappingContext actualMappingContext = repositoryFactoryBean.getGemfireMappingContext();

		assertThat(actualMappingContext, is(sameInstance(mockMappingContext)));

		actualMappingContext = TestUtils.readField("mappingContext", repositoryFactoryBean);

		assertThat(actualMappingContext, is(sameInstance(mockMappingContext)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void resolveConfiguredMappingContext() {
		final MappingContext mockMappingContext = mock(MappingContext.class);

		repositoryFactoryBean = new GemfireRepositoryFactoryBean() {
			@Override protected MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> getGemfireMappingContext() {
				return mockMappingContext;
			}

			@Override public void setGemfireMappingContext(MappingContext mappingContext) {
				throw new IllegalStateException(String.format(
					"setGemfireMappingContext with (%1$s) should not have been called", mappingContext));
			}
		};

		repositoryFactoryBean.resolveMappingContext();

		assertThat(repositoryFactoryBean.getGemfireMappingContext(), is(sameInstance(mockMappingContext)));
	}

	@Test
	public void resolveMappingContextFromApplicationContext() throws Exception {
		final ApplicationContext mockApplicationContext = mock(ApplicationContext.class);

		GemfireMappingContext mockMappingContext = mock(GemfireMappingContext.class);

		when(mockApplicationContext.getBean(eq(GemfireRepositoryFactoryBean.DEFAULT_MAPPING_CONTEXT_BEAN_NAME), eq(
			GemfireMappingContext.class))).thenReturn(mockMappingContext);

		repositoryFactoryBean = new GemfireRepositoryFactoryBean() {
			@Override protected ApplicationContext getApplicationContext() {
				return mockApplicationContext;
			}
		};

		assertThat(repositoryFactoryBean.getGemfireMappingContext(), is(nullValue()));

		repositoryFactoryBean.resolveMappingContext();

		assertThat(repositoryFactoryBean.getGemfireMappingContext(), is(sameInstance(
			(MappingContext) mockMappingContext)));

		MappingContext actualMappingContext = TestUtils.readField("mappingContext", repositoryFactoryBean);

		assertThat(actualMappingContext, is(sameInstance((MappingContext) mockMappingContext)));

		verify(mockApplicationContext, times(1)).getBean(eq(GemfireRepositoryFactoryBean.DEFAULT_MAPPING_CONTEXT_BEAN_NAME),
			eq(GemfireMappingContext.class));
	}

	@Test
	public void resolveNewMappingContext() throws Exception {
		final ApplicationContext mockApplicationContext = mock(ApplicationContext.class);

		when(mockApplicationContext.getBean(eq(GemfireRepositoryFactoryBean.DEFAULT_MAPPING_CONTEXT_BEAN_NAME), eq(
			GemfireMappingContext.class))).thenThrow(new NoSuchBeanDefinitionException(
				GemfireRepositoryFactoryBean.DEFAULT_MAPPING_CONTEXT_BEAN_NAME));

		repositoryFactoryBean = new GemfireRepositoryFactoryBean() {
			@Override protected ApplicationContext getApplicationContext() {
				return mockApplicationContext;
			}
		};

		assertThat(repositoryFactoryBean.getGemfireMappingContext(), is(nullValue()));

		repositoryFactoryBean.resolveMappingContext();

		assertThat(repositoryFactoryBean.getGemfireMappingContext(), is(instanceOf(GemfireMappingContext.class)));

		MappingContext actualMappingContext = TestUtils.readField("mappingContext", repositoryFactoryBean);

		assertThat(actualMappingContext, is(instanceOf(GemfireMappingContext.class)));

		verify(mockApplicationContext, times(1)).getBean(eq(GemfireRepositoryFactoryBean.DEFAULT_MAPPING_CONTEXT_BEAN_NAME),
			eq(GemfireMappingContext.class));
	}

	@Test
	public void resolveNewMappingContextAndRegistersSingleton() throws Exception {
		final ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class);

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class);

		when(mockApplicationContext.getBean(eq(GemfireRepositoryFactoryBean.DEFAULT_MAPPING_CONTEXT_BEAN_NAME), eq(
			GemfireMappingContext.class))).thenThrow(new NoSuchBeanDefinitionException(
				GemfireRepositoryFactoryBean.DEFAULT_MAPPING_CONTEXT_BEAN_NAME));

		when(mockApplicationContext.getBeanFactory()).thenReturn(mockBeanFactory);

		repositoryFactoryBean = new GemfireRepositoryFactoryBean() {
			@Override protected ApplicationContext getApplicationContext() {
				return mockApplicationContext;
			}
		};

		assertThat(repositoryFactoryBean.getGemfireMappingContext(), is(nullValue()));

		repositoryFactoryBean.resolveMappingContext();

		assertThat(repositoryFactoryBean.getGemfireMappingContext(), is(instanceOf(GemfireMappingContext.class)));

		MappingContext actualMappingContext = TestUtils.readField("mappingContext", repositoryFactoryBean);

		assertThat(actualMappingContext, is(instanceOf(GemfireMappingContext.class)));

		verify(mockApplicationContext, times(1)).getBean(eq(GemfireRepositoryFactoryBean.DEFAULT_MAPPING_CONTEXT_BEAN_NAME),
			eq(GemfireMappingContext.class));
		verify(mockApplicationContext, times(1)).getBeanFactory();
		verify(mockBeanFactory, times(1)).registerSingleton(eq(GemfireRepositoryFactoryBean.DEFAULT_MAPPING_CONTEXT_BEAN_NAME),
			Matchers.isA(GemfireMappingContext.class));
	}

}
