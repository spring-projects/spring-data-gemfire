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

package org.springframework.data.gemfire.config;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;

/**
 * The ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessorTest class is a test suite of test cases testing
 * the contract and functionality of the ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessor class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessor
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessorTest {

	protected static final String PROPERTIES_PROPERTY_NAME_SHORTCUT =
		ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessor.PROPERTIES_PROPERTY_NAME;

	private ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessor beanFactoryPostProcessor =
		new ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessor();

	@Mock
	private ConfigurableListableBeanFactory mockBeanFactory;

	protected String getBeanReference(Object reference) {
		return (reference instanceof RuntimeBeanReference ? ((RuntimeBeanReference) reference).getBeanName() : null);
	}

	protected BeanDefinition newBeanDefinition(Class<?> type) {
		return BeanDefinitionBuilder.genericBeanDefinition(type).getBeanDefinition();
	}

	protected <T> T[] toArray(T... array) {
		return array;
	}

	@Test
	public void postProcessSyncsGemfireProperties() {
		BeanDefinition objectBean = newBeanDefinition(Object.class);
		BeanDefinition clientCacheBean = newBeanDefinition(ClientCacheFactoryBean.class);

		clientCacheBean.getPropertyValues().addPropertyValue(PROPERTIES_PROPERTY_NAME_SHORTCUT,
			new RuntimeBeanReference("gemfireProperties"));

		BeanDefinition poolBean = newBeanDefinition(PoolFactoryBean.class);

		when(mockBeanFactory.getBeanDefinitionNames()).thenReturn(toArray("objectBean", "clientCacheBean", "poolBean"));
		when(mockBeanFactory.getBeanDefinition(eq("objectBean"))).thenReturn(objectBean);
		when(mockBeanFactory.getBeanDefinition(eq("clientCacheBean"))).thenReturn(clientCacheBean);
		when(mockBeanFactory.getBeanDefinition(eq("poolBean"))).thenReturn(poolBean);

		assertThat(poolBean.getPropertyValues().getPropertyValue(PROPERTIES_PROPERTY_NAME_SHORTCUT), is(nullValue()));

		beanFactoryPostProcessor.postProcessBeanFactory(mockBeanFactory);

		assertThat(getBeanReference(poolBean.getPropertyValues().getPropertyValue(PROPERTIES_PROPERTY_NAME_SHORTCUT).getValue()),
			is(equalTo("gemfireProperties")));

		verify(mockBeanFactory, times(1)).getBeanDefinitionNames();
		verify(mockBeanFactory, times(1)).getBeanDefinition(eq("objectBean"));
		verify(mockBeanFactory, times(1)).getBeanDefinition(eq("clientCacheBean"));
		verify(mockBeanFactory, times(1)).getBeanDefinition(eq("poolBean"));
	}

	@Test
	public void postProcessDoesNotSyncGemfirePropertiesWhenNoClientCacheFactoryBeanIsDeclared() {
		BeanDefinition poolBean = newBeanDefinition(PoolFactoryBean.class);

		when(mockBeanFactory.getBeanDefinitionNames()).thenReturn(toArray("poolBean"));
		when(mockBeanFactory.getBeanDefinition(eq("poolBean"))).thenReturn(poolBean);

		assertThat(poolBean.getPropertyValues().getPropertyValue(PROPERTIES_PROPERTY_NAME_SHORTCUT), is(nullValue()));

		beanFactoryPostProcessor.postProcessBeanFactory(mockBeanFactory);

		assertThat(poolBean.getPropertyValues().getPropertyValue(PROPERTIES_PROPERTY_NAME_SHORTCUT), is(nullValue()));

		verify(mockBeanFactory, times(1)).getBeanDefinitionNames();
		verify(mockBeanFactory, times(1)).getBeanDefinition(eq("poolBean"));
	}

	@Test
	public void postProcessDoesNotSyncGemfirePropertiesWhenNoPoolFactoryBeanIsDeclared() {
		BeanDefinition clientCacheBean = newBeanDefinition(ClientCacheFactoryBean.class);

		clientCacheBean.getPropertyValues().addPropertyValue(PROPERTIES_PROPERTY_NAME_SHORTCUT,
			new RuntimeBeanReference("gemfireProperties"));

		when(mockBeanFactory.getBeanDefinitionNames()).thenReturn(toArray("clientCacheBean"));
		when(mockBeanFactory.getBeanDefinition(eq("clientCacheBean"))).thenReturn(clientCacheBean);

		beanFactoryPostProcessor.postProcessBeanFactory(mockBeanFactory);

		verify(mockBeanFactory, times(1)).getBeanDefinitionNames();
		verify(mockBeanFactory, times(1)).getBeanDefinition(eq("clientCacheBean"));
	}

	@Test
	public void postProcessDoesNotSyncWhenGemfirePropertiesAreUnspecified() {
		BeanDefinition clientCacheBean = newBeanDefinition(ClientCacheFactoryBean.class);
		BeanDefinition poolBean = newBeanDefinition(PoolFactoryBean.class);

		when(mockBeanFactory.getBeanDefinitionNames()).thenReturn(toArray("clientCacheBean", "poolBean"));
		when(mockBeanFactory.getBeanDefinition(eq("clientCacheBean"))).thenReturn(clientCacheBean);
		when(mockBeanFactory.getBeanDefinition(eq("poolBean"))).thenReturn(poolBean);

		assertThat(poolBean.getPropertyValues().getPropertyValue(PROPERTIES_PROPERTY_NAME_SHORTCUT), is(nullValue()));

		beanFactoryPostProcessor.postProcessBeanFactory(mockBeanFactory);

		assertThat(poolBean.getPropertyValues().getPropertyValue(PROPERTIES_PROPERTY_NAME_SHORTCUT), is(nullValue()));

		verify(mockBeanFactory, times(1)).getBeanDefinitionNames();
		verify(mockBeanFactory, times(1)).getBeanDefinition(eq("clientCacheBean"));
		verify(mockBeanFactory, times(1)).getBeanDefinition(eq("poolBean"));
	}

}
