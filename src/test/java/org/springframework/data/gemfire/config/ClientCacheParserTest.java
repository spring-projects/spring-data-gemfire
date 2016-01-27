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
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isNotNull;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.atomic.AtomicReference;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.runners.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.springframework.beans.PropertyValues;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * The ClientCacheParserTest class is a test suite of test cases testing the contract and functionality of the
 * ClientCacheParser class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @since 1.8.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ClientCacheParserTest {

	@Mock
	private Element mockElement;

	@Test
	@SuppressWarnings("all")
	public void beanClassEqualsClientCacheFactoryBean() {
		assertThat((Class<ClientCacheFactoryBean>) new ClientCacheParser().getBeanClass(mockElement),
			is(equalTo(ClientCacheFactoryBean.class)));
	}

	@Test
	public void doParseSetsPropertiesAndRegistersClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessor() {
		NodeList mockNodeList = mock(NodeList.class);

		when(mockNodeList.getLength()).thenReturn(0);
		when(mockElement.getAttribute(eq("durable-client-id"))).thenReturn("123");
		when(mockElement.getAttribute(eq("durable-client-timeout"))).thenReturn("60");
		when(mockElement.getAttribute(eq("keep-alive"))).thenReturn("false");
		when(mockElement.getAttribute(eq("pool-name"))).thenReturn("testPool");
		when(mockElement.getAttribute(eq("ready-for-events"))).thenReturn(null);
		when(mockElement.getChildNodes()).thenReturn(mockNodeList);

		final AtomicReference<BeanDefinition> beanDefinitionReference = new AtomicReference<BeanDefinition>(null);

		final BeanDefinitionRegistry mockRegistry = mock(BeanDefinitionRegistry.class);

		doAnswer(new Answer<Void>() {
			public Void answer(InvocationOnMock invocation) throws Throwable {
				BeanDefinition beanFactoryPostProcessorBeanDefinition =
					invocation.getArgumentAt(1, BeanDefinition.class);

				if (ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessor.class.getName().equals(
						beanFactoryPostProcessorBeanDefinition.getBeanClassName())) {
					beanDefinitionReference.compareAndSet(null, beanFactoryPostProcessorBeanDefinition);
				}

				return null;
			}
		}).when(mockRegistry).registerBeanDefinition(isNotNull(String.class), any(BeanDefinition.class));

		when(mockRegistry.containsBeanDefinition(anyString())).thenReturn(false);

		BeanDefinitionBuilder clientCacheBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		ClientCacheParser clientCacheParser = new ClientCacheParser() {
			@Override protected BeanDefinitionRegistry getRegistry(ParserContext parserContext) {
				return mockRegistry;
			}
		};

		clientCacheParser.doParse(mockElement, null, clientCacheBuilder);

		BeanDefinition clientCacheBeanDefinition = clientCacheBuilder.getBeanDefinition();

		assertThat(clientCacheBeanDefinition, is(notNullValue()));

		PropertyValues propertyValues = clientCacheBeanDefinition.getPropertyValues();

		assertThat(propertyValues, is(notNullValue()));
		assertThat((String) propertyValues.getPropertyValue("durableClientId").getValue(), is(equalTo("123")));
		assertThat((String) propertyValues.getPropertyValue("durableClientTimeout").getValue(), is(equalTo("60")));
		assertThat((String) propertyValues.getPropertyValue("keepAlive").getValue(), is(equalTo("false")));
		assertThat((String) propertyValues.getPropertyValue("poolName").getValue(), is(equalTo("testPool")));
		assertThat(propertyValues.getPropertyValue("readyForEvents"), is(nullValue()));
		assertThat(beanDefinitionReference.get(), is(notNullValue()));

		verify(mockElement, times(1)).getAttribute(eq("durable-client-id"));
		verify(mockElement, times(1)).getAttribute(eq("durable-client-timeout"));
		verify(mockElement, times(1)).getAttribute(eq("keep-alive"));
		verify(mockElement, times(1)).getAttribute(eq("pool-name"));
		verify(mockElement, times(1)).getAttribute(eq("ready-for-events"));
		verify(mockRegistry, times(1)).registerBeanDefinition(isNotNull(String.class),
			same(beanDefinitionReference.get()));
	}

	@Test
	public void postProcessDynamicRegionSupportParsesPoolName() {
		when(mockElement.getAttribute(eq("pool-name"))).thenReturn("testPool");

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition();

		new ClientCacheParser().postProcessDynamicRegionSupport(mockElement, builder);

		BeanDefinition beanDefinition = builder.getBeanDefinition();

		assertThat(beanDefinition, is(notNullValue()));
		assertThat(beanDefinition.getPropertyValues(), is(notNullValue()));
		assertThat(beanDefinition.getPropertyValues().getPropertyValue("poolName"), is(notNullValue()));
		assertThat((String) beanDefinition.getPropertyValues().getPropertyValue("poolName").getValue(), is(equalTo("testPool")));

		verify(mockElement, times(1)).getAttribute(eq("pool-name"));
	}

}
