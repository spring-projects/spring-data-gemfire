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

package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.beans.PropertyValues;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Unit tests for {@link ClientCacheParser}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.xml.ClientCacheParser
 * @since 1.8.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ClientCacheParserUnitTests {

	@Mock
	private Element mockElement;

	protected void assertPropertyIsPresent(BeanDefinition beanDefinition, String propertyName) {
		assertThat(beanDefinition.getPropertyValues().contains(propertyName)).isTrue();
	}

	protected void assertPropertyIsNotPresent(BeanDefinition beanDefinition, String propertyName) {
		assertThat(beanDefinition.getPropertyValues().contains(propertyName)).isFalse();
	}

	protected void assertPropertyValueEquals(BeanDefinition beanDefinition, String propertyName,
			Object expectedPropertyValue) {

		assertPropertyIsPresent(beanDefinition, propertyName);
		assertThat(beanDefinition.getPropertyValues().getPropertyValue(propertyName).getValue())
			.isEqualTo(expectedPropertyValue);
	}

	@Test
	@SuppressWarnings("all")
	public void beanClassEqualsClientCacheFactoryBean() {
		assertThat(new ClientCacheParser().getBeanClass(mockElement)).isEqualTo(ClientCacheFactoryBean.class);
	}

	@Test
	public void doParseSetsProperties() {
		NodeList mockNodeList = mock(NodeList.class);

		when(mockElement.getAttribute(eq("durable-client-id"))).thenReturn("123");
		when(mockElement.getAttribute(eq("durable-client-timeout"))).thenReturn("60");
		when(mockElement.getAttribute(eq("keep-alive"))).thenReturn("false");
		when(mockElement.getAttribute(eq("pool-name"))).thenReturn("TestPool");
		when(mockElement.getAttribute(eq("ready-for-events"))).thenReturn(null);
		when(mockElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(0);

		BeanDefinitionBuilder clientCacheBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		final BeanDefinitionRegistry mockRegistry = mock(BeanDefinitionRegistry.class);

		when(mockRegistry.containsBeanDefinition(anyString())).thenReturn(false);

		ClientCacheParser clientCacheParser = new ClientCacheParser() {
			@Override protected BeanDefinitionRegistry getRegistry(ParserContext parserContext) {
				return mockRegistry;
			}
		};

		clientCacheParser.doParse(mockElement, null, clientCacheBuilder);

		BeanDefinition clientCacheBeanDefinition = clientCacheBuilder.getBeanDefinition();

		assertThat(clientCacheBeanDefinition).isNotNull();

		PropertyValues propertyValues = clientCacheBeanDefinition.getPropertyValues();

		assertThat(propertyValues).isNotNull();
		assertPropertyValueEquals(clientCacheBeanDefinition, "durableClientId", "123");
		assertPropertyValueEquals(clientCacheBeanDefinition, "durableClientTimeout", "60");
		assertPropertyValueEquals(clientCacheBeanDefinition, "keepAlive", "false");
		assertPropertyValueEquals(clientCacheBeanDefinition, "poolName", "TestPool");
		assertPropertyIsNotPresent(clientCacheBeanDefinition, "readyForEvents");

		verify(mockElement, times(1)).getAttribute(eq("durable-client-id"));
		verify(mockElement, times(1)).getAttribute(eq("durable-client-timeout"));
		verify(mockElement, times(1)).getAttribute(eq("keep-alive"));
		verify(mockElement, times(1)).getAttribute(eq("pool-name"));
		verify(mockElement, times(1)).getAttribute(eq("ready-for-events"));
	}

	@Test
	public void postProcessDynamicRegionSupportParsesPoolName() {
		when(mockElement.getAttribute(eq("pool-name"))).thenReturn("TestPool");

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition();

		new ClientCacheParser().postProcessDynamicRegionSupport(mockElement, builder);

		BeanDefinition beanDefinition = builder.getBeanDefinition();

		assertThat(beanDefinition).isNotNull();
		assertPropertyValueEquals(beanDefinition, "poolName", "TestPool");

		verify(mockElement, times(1)).getAttribute(eq("pool-name"));
	}
}
