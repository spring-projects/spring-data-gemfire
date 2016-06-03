/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.runners.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.springframework.beans.PropertyValues;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConstructorArgumentValues;
import org.springframework.beans.factory.config.MethodInvokingBean;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * The PoolParserTest class is a test suite of test cases testing the contract and functionality
 * of the PoolParser class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see org.springframework.data.gemfire.config.PoolParser
 * @since 1.7.0
 */
@RunWith(MockitoJUnitRunner.class)
public class PoolParserTest {

	@Mock
	private BeanDefinitionRegistry mockRegistry;

	private PoolParser parser;

	@Before
	public void setup() {
		PoolParser.INFRASTRUCTURE_REGISTRATION.set(true);

		parser = new PoolParser() {
			@Override BeanDefinitionRegistry getRegistry(final ParserContext parserContext) {
				return mockRegistry;
			}
		};
	}

	protected void assertBeanDefinition(BeanDefinition beanDefinition, String expectedHost, String expectedPort) {
		assertThat(beanDefinition, is(notNullValue()));
		assertThat(beanDefinition.getBeanClassName(), is(equalTo(ConnectionEndpoint.class.getName())));
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentCount(), is(equalTo(2)));
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentValue(0, String.class).getValue().toString(),
			is(equalTo(expectedHost)));
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentValue(1, String.class).getValue().toString(),
			is(equalTo(expectedPort)));
	}

	protected void assertPropertyValue(PropertyValues propertyValues, String propertyName, Object propertyValue) {
		assertThat(propertyValues.getPropertyValue(propertyName).getValue(), is(equalTo(propertyValue)));
	}

	protected String generatedBeanName(Class<?> type) {
		return generatedBeanName(type.getName());
	}

	protected String generatedBeanName(String beanClassName) {
		return String.format("%1$s%2$s%3$d", beanClassName, BeanDefinitionReaderUtils.GENERATED_BEAN_NAME_SEPARATOR, 0);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getBeanClass() {
		assertThat((Class<PoolFactoryBean>) parser.getBeanClass(null), is(equalTo(PoolFactoryBean.class)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doParse() {
		Element mockPoolElement = mock(Element.class, "testDoParse.MockPoolElement");
		Element mockLocatorElementOne = mock(Element.class, "testDoParse.MockLocatorElementOne");
		Element mockLocatorElementTwo = mock(Element.class, "testDoParse.MockLocatorElementTwo");
		Element mockServerElement = mock(Element.class, "testDoParse.MockServerElement");

		NodeList mockNodeList = mock(NodeList.class);

		when(mockPoolElement.getAttribute(eq("free-connection-timeout"))).thenReturn("5000");
		when(mockPoolElement.getAttribute(eq("idle-timeout"))).thenReturn("120000");
		when(mockPoolElement.getAttribute(eq("keep-alive"))).thenReturn("true");
		when(mockPoolElement.getAttribute(eq("load-conditioning-interval"))).thenReturn("300000");
		when(mockPoolElement.getAttribute(eq("max-connections"))).thenReturn("500");
		when(mockPoolElement.getAttribute(eq("min-connections"))).thenReturn("50");
		when(mockPoolElement.getAttribute(eq("multi-user-authentication"))).thenReturn("true");
		when(mockPoolElement.getAttribute(eq("ping-interval"))).thenReturn("15000");
		when(mockPoolElement.getAttribute(eq("pr-single-hop-enabled"))).thenReturn("true");
		when(mockPoolElement.getAttribute(eq("read-timeout"))).thenReturn("20000");
		when(mockPoolElement.getAttribute(eq("retry-attempts"))).thenReturn("1");
		when(mockPoolElement.getAttribute(eq("server-group"))).thenReturn("TestGroup");
		when(mockPoolElement.getAttribute(eq("socket-buffer-size"))).thenReturn("16384");
		when(mockPoolElement.getAttribute(eq("statistic-interval"))).thenReturn("500");
		when(mockPoolElement.getAttribute(eq("subscription-ack-interval"))).thenReturn("200");
		when(mockPoolElement.getAttribute(eq("subscription-enabled"))).thenReturn("true");
		when(mockPoolElement.getAttribute(eq("subscription-message-tracking-timeout"))).thenReturn("30000");
		when(mockPoolElement.getAttribute(eq("subscription-redundancy"))).thenReturn("2");
		when(mockPoolElement.getAttribute(eq("thread-local-connections"))).thenReturn("false");
		when(mockPoolElement.hasAttribute(PoolParser.LOCATORS_ATTRIBUTE_NAME)).thenReturn(true);
		when(mockPoolElement.getAttribute(PoolParser.LOCATORS_ATTRIBUTE_NAME)).thenReturn("nebula[1234]");
		when(mockPoolElement.hasAttribute(PoolParser.SERVERS_ATTRIBUTE_NAME)).thenReturn(true);
		when(mockPoolElement.getAttribute(PoolParser.SERVERS_ATTRIBUTE_NAME)).thenReturn("skullbox[9876], backspace");
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(3);
		when(mockNodeList.item(eq(0))).thenReturn(mockLocatorElementOne);
		when(mockNodeList.item(eq(1))).thenReturn(mockServerElement);
		when(mockNodeList.item(eq(2))).thenReturn(mockLocatorElementTwo);
		when(mockLocatorElementOne.getLocalName()).thenReturn(PoolParser.LOCATOR_ELEMENT_NAME);
		when(mockLocatorElementOne.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("comet");
		when(mockLocatorElementOne.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn("1025");
		when(mockLocatorElementTwo.getLocalName()).thenReturn(PoolParser.LOCATOR_ELEMENT_NAME);
		when(mockLocatorElementTwo.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("quasar");
		when(mockLocatorElementTwo.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn(" ");
		when(mockServerElement.getLocalName()).thenReturn(PoolParser.SERVER_ELEMENT_NAME);
		when(mockServerElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("rightshift");
		when(mockServerElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn("4556");

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, null, builder);

		BeanDefinition poolDefinition = builder.getBeanDefinition();

		assertThat(poolDefinition, is(notNullValue()));

		PropertyValues poolPropertyValues = poolDefinition.getPropertyValues();

		assertPropertyValue(poolPropertyValues, "freeConnectionTimeout", "5000");
		assertPropertyValue(poolPropertyValues, "idleTimeout", "120000");
		assertPropertyValue(poolPropertyValues, "keepAlive", "true");
		assertPropertyValue(poolPropertyValues, "loadConditioningInterval", "300000");
		assertPropertyValue(poolPropertyValues, "maxConnections", "500");
		assertPropertyValue(poolPropertyValues, "minConnections", "50");
		assertPropertyValue(poolPropertyValues, "multiUserAuthentication", "true");
		assertPropertyValue(poolPropertyValues, "pingInterval", "15000");
		assertPropertyValue(poolPropertyValues, "prSingleHopEnabled", "true");
		assertPropertyValue(poolPropertyValues, "readTimeout", "20000");
		assertPropertyValue(poolPropertyValues, "retryAttempts", "1");
		assertPropertyValue(poolPropertyValues, "serverGroup", "TestGroup");
		assertPropertyValue(poolPropertyValues, "socketBufferSize", "16384");
		assertPropertyValue(poolPropertyValues, "statisticInterval", "500");
		assertPropertyValue(poolPropertyValues, "subscriptionAckInterval", "200");
		assertPropertyValue(poolPropertyValues, "subscriptionEnabled", "true");
		assertPropertyValue(poolPropertyValues, "subscriptionMessageTrackingTimeout", "30000");
		assertPropertyValue(poolPropertyValues, "subscriptionRedundancy", "2");
		assertPropertyValue(poolPropertyValues, "threadLocalConnections", "false");
		assertThat(poolPropertyValues.contains("locators"), is(true));
		assertThat(poolPropertyValues.contains("servers"), is(true));

		ManagedList<BeanDefinition> locators = (ManagedList<BeanDefinition>)
			poolPropertyValues.getPropertyValue("locators").getValue();

		assertThat(locators, is(notNullValue()));
		assertThat(locators.size(), is(equalTo(3)));
		assertBeanDefinition(locators.get(0), "comet", "1025");
		assertBeanDefinition(locators.get(1), "quasar", String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(locators.get(2), "nebula", "1234");

		ManagedList<BeanDefinition> servers = (ManagedList<BeanDefinition>) poolDefinition.getPropertyValues()
			.getPropertyValue("servers").getValue();

		assertThat(servers, is(notNullValue()));
		assertThat(servers.size(), is(equalTo(3)));
		assertBeanDefinition(servers.get(0), "rightshift", "4556");
		assertBeanDefinition(servers.get(1), "skullbox", "9876");
		assertBeanDefinition(servers.get(2), "backspace", String.valueOf(PoolParser.DEFAULT_SERVER_PORT));

		verify(mockPoolElement, times(1)).getAttribute(eq("free-connection-timeout"));
		verify(mockPoolElement, times(1)).getAttribute(eq("idle-timeout"));
		verify(mockPoolElement, times(1)).getAttribute(eq("keep-alive"));
		verify(mockPoolElement, times(1)).getAttribute(eq("load-conditioning-interval"));
		verify(mockPoolElement, times(1)).getAttribute(eq("max-connections"));
		verify(mockPoolElement, times(1)).getAttribute(eq("min-connections"));
		verify(mockPoolElement, times(1)).getAttribute(eq("multi-user-authentication"));
		verify(mockPoolElement, times(1)).getAttribute(eq("ping-interval"));
		verify(mockPoolElement, times(1)).getAttribute(eq("pr-single-hop-enabled"));
		verify(mockPoolElement, times(1)).getAttribute(eq("read-timeout"));
		verify(mockPoolElement, times(1)).getAttribute(eq("retry-attempts"));
		verify(mockPoolElement, times(1)).getAttribute(eq("server-group"));
		verify(mockPoolElement, times(1)).getAttribute(eq("socket-buffer-size"));
		verify(mockPoolElement, times(1)).getAttribute(eq("statistic-interval"));
		verify(mockPoolElement, times(1)).getAttribute(eq("subscription-ack-interval"));
		verify(mockPoolElement, times(1)).getAttribute(eq("subscription-enabled"));
		verify(mockPoolElement, times(1)).getAttribute(eq("subscription-message-tracking-timeout"));
		verify(mockPoolElement, times(1)).getAttribute(eq("subscription-redundancy"));
		verify(mockPoolElement, times(1)).getAttribute(eq("thread-local-connections"));
		verify(mockPoolElement, times(1)).getChildNodes();
		verify(mockNodeList, times(4)).getLength();
		verify(mockNodeList, times(1)).item(eq(0));
		verify(mockNodeList, times(1)).item(eq(1));
		verify(mockNodeList, times(1)).item(eq(2));
		verify(mockPoolElement, never()).hasAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, never()).hasAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockLocatorElementOne, times(1)).getLocalName();
		verify(mockLocatorElementOne, times(1)).getAttribute(PoolParser.HOST_ATTRIBUTE_NAME);
		verify(mockLocatorElementOne, times(1)).getAttribute(PoolParser.PORT_ATTRIBUTE_NAME);
		verify(mockLocatorElementTwo, times(1)).getLocalName();
		verify(mockLocatorElementTwo, times(1)).getAttribute(PoolParser.HOST_ATTRIBUTE_NAME);
		verify(mockLocatorElementTwo, times(1)).getAttribute(PoolParser.PORT_ATTRIBUTE_NAME);
		verify(mockServerElement, times(1)).getLocalName();
		verify(mockServerElement, times(1)).getAttribute(PoolParser.HOST_ATTRIBUTE_NAME);
		verify(mockServerElement, times(1)).getAttribute(PoolParser.PORT_ATTRIBUTE_NAME);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doParseWithNoLocatorsOrServersSpecified() {
		Element mockPoolElement = mock(Element.class);
		NodeList mockNodeList = mock(NodeList.class);

		when(mockPoolElement.hasAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(false);
		when(mockPoolElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn("");
		when(mockPoolElement.hasAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(false);
		when(mockPoolElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn("");
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(0);

		BeanDefinitionBuilder poolBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, null, poolBuilder);

		BeanDefinition poolDefinition = poolBuilder.getBeanDefinition();

		assertThat(poolDefinition, is(notNullValue()));

		PropertyValues poolPropertyValues = poolDefinition.getPropertyValues();

		assertThat(poolPropertyValues.contains("locators"), is(false));
		assertThat(poolPropertyValues.contains("servers"), is(true));

		ManagedList<BeanDefinition> servers = (ManagedList<BeanDefinition>)
			poolPropertyValues.getPropertyValue("servers").getValue();

		assertThat(servers, is(notNullValue()));
		assertThat(servers.size(), is(equalTo(1)));
		assertBeanDefinition(servers.get(0), PoolParser.DEFAULT_HOST, String.valueOf(PoolParser.DEFAULT_SERVER_PORT));

		verify(mockPoolElement, times(1)).getChildNodes();
		verify(mockNodeList, times(1)).getLength();
		verify(mockNodeList, never()).item(anyInt());
		verify(mockPoolElement, times(1)).hasAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).hasAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
	}

	@Test
	public void doParseWithServersAttributeValueSpecifiedAsAPropertyPlaceholder() {
		Element mockPoolElement = mock(Element.class);
		NodeList mockNodeList = mock(NodeList.class);

		when(mockPoolElement.getAttribute(PoolParser.ID_ATTRIBUTE)).thenReturn("TestPool");
		when(mockPoolElement.hasAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(false);
		when(mockPoolElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn("");
		when(mockPoolElement.hasAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(true);
		when(mockPoolElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(
			"${gemfire.server.hosts-and-ports}");
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(0);

		BeanDefinitionBuilder poolBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, null, poolBuilder);

		BeanDefinition poolDefinition = poolBuilder.getBeanDefinition();

		assertThat(poolDefinition, is(notNullValue()));

		PropertyValues poolPropertyValues = poolDefinition.getPropertyValues();

		assertThat(poolPropertyValues.contains("locators"), is(false));
		assertThat(poolPropertyValues.contains("servers"), is(false));

		when(mockRegistry.containsBeanDefinition(anyString())).thenReturn(false);

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				String generatedName = invocation.getArgumentAt(0, String.class);
				BeanDefinition addServersDefinition = invocation.getArgumentAt(1, BeanDefinition.class);

				assertThat(StringUtils.hasText(generatedName), is(true));
				assertThat(generatedName, is(equalTo(generatedBeanName(addServersDefinition.getBeanClassName()))));
				assertThat(addServersDefinition, is(notNull()));
				assertThat(addServersDefinition.getBeanClassName(), is(equalTo(MethodInvokingBean.class.getName())));

				PropertyValues addServersPropertyValues = addServersDefinition.getPropertyValues();

				assertThat(addServersPropertyValues, is(notNullValue()));
				assertPropertyValue(addServersPropertyValues, "targetObject", new RuntimeBeanReference("&TestPool"));
				assertPropertyValue(addServersPropertyValues, "targetMethod", "addServers");

				Object arguments = addServersPropertyValues.getPropertyValue("arguments").getValue();

				assertThat(arguments, is(instanceOf(BeanDefinition.class)));

				BeanDefinition argumentsDefinition = (BeanDefinition) arguments;

				assertThat(argumentsDefinition.getBeanClassName(), is(equalTo(ConnectionEndpointList.class.getName())));

				ConstructorArgumentValues constructorArguments = argumentsDefinition.getConstructorArgumentValues();

				assertThat(constructorArguments, is(notNullValue()));
				assertThat(constructorArguments.getArgumentCount(), is(equalTo(2)));
				assertThat((Integer) constructorArguments.getArgumentValue(0, Integer.class).getValue(),
					is(equalTo(PoolParser.DEFAULT_SERVER_PORT)));
				assertThat(String.valueOf(constructorArguments.getArgumentValue(1, String.class).getValue()),
					is(equalTo("${gemfire.server.hosts-and-ports}")));

				return null;
			}
		}).when(mockRegistry).registerBeanDefinition(eq(generatedBeanName(MethodInvokingBean.class)),
			any(BeanDefinition.class));

		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.ID_ATTRIBUTE));
		verify(mockPoolElement, times(1)).hasAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).hasAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getChildNodes();
		verify(mockNodeList, times(1)).getLength();
		verify(mockNodeList, never()).item(anyInt());
		verify(mockRegistry, times(1)).containsBeanDefinition(eq(generatedBeanName(MethodInvokingBean.class)));
		verify(mockRegistry, times(1)).registerBeanDefinition(eq(generatedBeanName(MethodInvokingBean.class)),
			isA(BeanDefinition.class));
	}

	@Test
	public void hasAttributesReturnsTrueAndShortcircuts() {
		Element mockElement = mock(Element.class);

		when(mockElement.hasAttribute("attributeOne")).thenReturn(true);
		when(mockElement.hasAttribute("attributeTwo")).thenReturn(true);

		assertThat(parser.hasAttributes(mockElement, "attributeThree", "attributeTwo", "attributeOne"), is(true));

		verify(mockElement, times(1)).hasAttribute(eq("attributeThree"));
		verify(mockElement, times(1)).hasAttribute(eq("attributeTwo"));
		verify(mockElement, never()).hasAttribute(eq("attributeOne"));
	}

	@Test
	public void hasAttributesReturnsFalse() {
		Element mockElement = mock(Element.class);

		when(mockElement.hasAttribute(anyString())).thenReturn(false);

		assertThat(parser.hasAttributes(mockElement, "one", "two", "three", "four"), is(false));

		verify(mockElement, times(1)).hasAttribute(eq("one"));
		verify(mockElement, times(1)).hasAttribute(eq("two"));
		verify(mockElement, times(1)).hasAttribute(eq("three"));
		verify(mockElement, times(1)).hasAttribute(eq("four"));
	}

	@Test
	public void buildConnectionsUsingLocator() {
		BeanDefinition beanDefinition = parser.buildConnections("${test}", false);

		assertThat(beanDefinition, is(notNullValue()));
		assertThat(beanDefinition.getBeanClassName(), is(equalTo(ConnectionEndpointList.class.getName())));

		ConstructorArgumentValues constructorArguments = beanDefinition.getConstructorArgumentValues();

		assertThat(constructorArguments, is(notNullValue()));
		assertThat(constructorArguments.getArgumentValue(0, Integer.class).getValue().toString(),
			is(equalTo(String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT))));
		assertThat(constructorArguments.getArgumentValue(1, String.class).getValue().toString(),
			is(equalTo("${test}")));
	}

	@Test
	public void buildConnectionsUsingServer() {
		BeanDefinition beanDefinition = parser.buildConnections("${test}", true);

		assertThat(beanDefinition, is(notNullValue()));
		assertThat(beanDefinition.getBeanClassName(), is(equalTo(ConnectionEndpointList.class.getName())));

		ConstructorArgumentValues constructorArguments = beanDefinition.getConstructorArgumentValues();

		assertThat(constructorArguments, is(notNullValue()));
		assertThat(constructorArguments.getArgumentValue(0, Integer.class).getValue().toString(),
			is(equalTo(String.valueOf(PoolParser.DEFAULT_SERVER_PORT))));
		assertThat(constructorArguments.getArgumentValue(1, String.class).getValue().toString(),
			is(equalTo("${test}")));
	}

	@Test
	public void buildConnection() {
		assertBeanDefinition(parser.buildConnection("skullbox", "1234", true), "skullbox", "1234");
		assertBeanDefinition(parser.buildConnection("saturn", " ", true), "saturn",
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(parser.buildConnection("  ", "1234", true), PoolParser.DEFAULT_HOST, "1234");
		assertBeanDefinition(parser.buildConnection("  ", "", true), PoolParser.DEFAULT_HOST,
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(parser.buildConnection("neptune", "9876", false), "neptune", "9876");
		assertBeanDefinition(parser.buildConnection("jupiter", null, false), "jupiter",
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(parser.buildConnection(null, "9876", false), PoolParser.DEFAULT_HOST, "9876");
		assertBeanDefinition(parser.buildConnection("", "  ", false), PoolParser.DEFAULT_HOST,
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
	}

	@Test
	public void defaultHost() {
		assertEquals("skullbox", parser.defaultHost("skullbox"));
		assertEquals("localhost", parser.defaultHost(null));
		assertEquals("localhost", parser.defaultHost(""));
		assertEquals("localhost", parser.defaultHost("  "));
	}

	@Test
	public void defaultPort() {
		assertEquals("1234", parser.defaultPort("1234", true));
		assertEquals("9876", parser.defaultPort("9876", false));
		assertEquals(String.valueOf(PoolParser.DEFAULT_SERVER_PORT), parser.defaultPort(null, true));
		assertEquals(String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT), parser.defaultPort("", false));
		assertEquals(String.valueOf(PoolParser.DEFAULT_SERVER_PORT), parser.defaultPort("  ", true));
	}

	@Test
	public void parseConnection() {
		assertBeanDefinition(parser.parseConnection("skullbox[1234]", true), "skullbox", "1234");
		assertBeanDefinition(parser.parseConnection("saturn", true), "saturn",
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(parser.parseConnection("neptune[]", false), "neptune",
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(parser.parseConnection("[9876]", true), PoolParser.DEFAULT_HOST, "9876");
		assertBeanDefinition(parser.parseConnection("[]", false), PoolParser.DEFAULT_HOST,
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
	}

	@Test
	public void parseSingleConnection() {
		List<BeanDefinition> beans = parser.parseConnections("skullbox[1234]", true);

		assertNotNull(beans);
		assertFalse(beans.isEmpty());
		assertEquals(1, beans.size());
		assertBeanDefinition(beans.get(0), "skullbox", "1234");
	}

	@Test
	public void parseMultipleConnections() {
		List<BeanDefinition> beans = parser.parseConnections(
			"skullbox[1234],neptune,saturn[ ],jupiter[SlO], [9876],v3nU5[4_567],  localhost [1 01  0] ", true);

		assertNotNull(beans);
		assertFalse(beans.isEmpty());
		assertEquals(7, beans.size());
		assertBeanDefinition(beans.get(0), "skullbox", "1234");
		assertBeanDefinition(beans.get(1), "neptune", String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(beans.get(2), "saturn", String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(beans.get(3), "jupiter", String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(beans.get(4), "localhost", "9876");
		assertBeanDefinition(beans.get(5), "v3nU5", "4567");
		assertBeanDefinition(beans.get(6), "localhost", "1010");
	}

	@Test
	public void parseDigits() {
		assertEquals("1234", parser.parseDigits("1234"));
		assertEquals("4567", parser.parseDigits("  4567 "));
		assertEquals("78901", parser.parseDigits("7  89 0  1 "));
		assertEquals("8080", parser.parseDigits("[8080]"));
		assertEquals("443", parser.parseDigits(":443"));
		assertEquals("", parser.parseDigits(""));
		assertEquals("", parser.parseDigits("  "));
		assertEquals("", parser.parseDigits("[]"));
		assertEquals("", parser.parseDigits("oneTwoThree"));
	}

	@Test
	public void isPropertyPlaceholder() {
		assertThat(parser.isPropertyPlaceholder("${some.property}"), is(true));
		assertThat(parser.isPropertyPlaceholder("${test}"), is(true));
		assertThat(parser.isPropertyPlaceholder("${p}"), is(true));
		assertThat(parser.isPropertyPlaceholder("$PROPERTY"), is(false));
		assertThat(parser.isPropertyPlaceholder("%PROPERTY%"), is(false));
		assertThat(parser.isPropertyPlaceholder("$"), is(false));
		assertThat(parser.isPropertyPlaceholder("${"), is(false));
		assertThat(parser.isPropertyPlaceholder("$}"), is(false));
		assertThat(parser.isPropertyPlaceholder("{}"), is(false));
		assertThat(parser.isPropertyPlaceholder("${}"), is(false));
		assertThat(parser.isPropertyPlaceholder("  "), is(false));
		assertThat(parser.isPropertyPlaceholder(""), is(false));
	}

	@Test
	public void parseLocator() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn("skullbox");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("1234");

		assertBeanDefinition(parser.parseLocator(mockElement), "skullbox", "1234");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseLocatorWithNoHostPort() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("");
		when(mockElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn(null);

		assertBeanDefinition(parser.parseLocator(mockElement), "localhost", "10334");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseLocators() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(
			"jupiter, saturn[1234],  [9876] ");

		List<BeanDefinition> locators = parser.parseLocators(mockElement, mockRegistry);

		assertThat(locators, is(notNullValue()));
		assertThat(locators.size(), is(equalTo(3)));
		assertBeanDefinition(locators.get(0), "jupiter", String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(locators.get(1), "saturn", "1234");
		assertBeanDefinition(locators.get(2), "localhost", "9876");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
	}

	@Test
	public void parseLocatorsWithPropertyPlaceholder() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.ID_ATTRIBUTE))).thenReturn(null);
		when(mockElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(
			"${gemfire.locators.hosts-and-ports}");
		when(mockRegistry.containsBeanDefinition(anyString())).thenReturn(false);

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				String generatedName = invocation.getArgumentAt(0, String.class);
				BeanDefinition addServersDefinition = invocation.getArgumentAt(1, BeanDefinition.class);

				assertThat(StringUtils.hasText(generatedName), is(true));
				assertThat(generatedName, is(equalTo(generatedBeanName(addServersDefinition.getBeanClassName()))));
				assertThat(addServersDefinition, is(notNullValue()));
				assertThat(addServersDefinition.getBeanClassName(), is(equalTo(MethodInvokingBean.class.getName())));

				PropertyValues addServersPropertyValues = addServersDefinition.getPropertyValues();

				assertThat(addServersPropertyValues, is(notNullValue()));
				assertPropertyValue(addServersPropertyValues, "targetObject", new RuntimeBeanReference("&gemfirePool"));
 				assertPropertyValue(addServersPropertyValues, "targetMethod", "addLocators");

				Object arguments = addServersPropertyValues.getPropertyValue("arguments").getValue();

				assertThat(arguments, is(instanceOf(BeanDefinition.class)));

				BeanDefinition argumentsDefinition = (BeanDefinition) arguments;

				assertThat(argumentsDefinition.getBeanClassName(), is(equalTo(ConnectionEndpointList.class.getName())));

				ConstructorArgumentValues constructorArguments = argumentsDefinition.getConstructorArgumentValues();

				assertThat(constructorArguments, is(notNullValue()));
				assertThat(constructorArguments.getArgumentCount(), is(equalTo(2)));
				assertThat(String.valueOf(constructorArguments.getArgumentValue(0, Integer.class).getValue()),
					is(equalTo(String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT))));
				assertThat(String.valueOf(constructorArguments.getArgumentValue(1, String.class).getValue()),
					is(equalTo("${gemfire.locators.hosts-and-ports}")));

				return null;
			}
		}).when(mockRegistry).registerBeanDefinition(eq(generatedBeanName(MethodInvokingBean.class)),
			any(BeanDefinition.class));

		List<BeanDefinition> locators = parser.parseLocators(mockElement, mockRegistry);

		assertThat(locators, is(notNullValue()));
		assertThat(locators.isEmpty(), is(true));

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.ID_ATTRIBUTE));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockRegistry, times(1)).containsBeanDefinition(eq(generatedBeanName(MethodInvokingBean.class)));
		verify(mockRegistry, times(1)).registerBeanDefinition(eq(generatedBeanName(MethodInvokingBean.class)),
			isA(BeanDefinition.class));
	}

	@Test
	public void parseServer() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn("plato");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("9876");

		assertBeanDefinition(parser.parseServer(mockElement), "plato", "9876");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseServerWithNoHostPort() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn(" ");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("");

		assertBeanDefinition(parser.parseServer(mockElement), "localhost", "40404");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseServers() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(" neptune[], venus[9876]");

		List<BeanDefinition> servers = parser.parseServers(mockElement, mockRegistry);

		assertNotNull(servers);
		assertFalse(servers.isEmpty());
		assertEquals(2, servers.size());
		assertBeanDefinition(servers.get(0), "neptune", String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(servers.get(1), "venus", "9876");
	}

	@Test
	public void parseServersWithPropertyPlaceholder() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.ID_ATTRIBUTE))).thenReturn(null);
		when(mockElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(
			"${gemfire.servers.hosts-and-ports}");
		when(mockRegistry.containsBeanDefinition(anyString())).thenReturn(false);

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				String generatedName = invocation.getArgumentAt(0, String.class);
				BeanDefinition addServersDefinition = invocation.getArgumentAt(1, BeanDefinition.class);

				assertThat(StringUtils.hasText(generatedName), is(true));
				assertThat(generatedName, is(equalTo(generatedBeanName(addServersDefinition.getBeanClassName()))));
				assertThat(addServersDefinition, is(notNullValue()));
				assertThat(addServersDefinition.getBeanClassName(), is(equalTo(MethodInvokingBean.class.getName())));

				PropertyValues addServersPropertyValues = addServersDefinition.getPropertyValues();

				assertThat(addServersPropertyValues, is(notNullValue()));
				assertPropertyValue(addServersPropertyValues, "targetObject", new RuntimeBeanReference("&gemfirePool"));
				assertPropertyValue(addServersPropertyValues, "targetMethod", "addServers");

				Object arguments = addServersPropertyValues.getPropertyValue("arguments").getValue();

				assertThat(arguments, is(instanceOf(BeanDefinition.class)));

				BeanDefinition argumentsDefinition = (BeanDefinition) arguments;

				assertThat(argumentsDefinition.getBeanClassName(), is(equalTo(ConnectionEndpointList.class.getName())));

				ConstructorArgumentValues constructorArguments = argumentsDefinition.getConstructorArgumentValues();

				assertThat(constructorArguments, is(notNullValue()));
				assertThat(constructorArguments.getArgumentCount(), is(equalTo(2)));
				assertThat(String.valueOf(constructorArguments.getArgumentValue(0, Object.class).getValue()),
					is(equalTo(String.valueOf(PoolParser.DEFAULT_SERVER_PORT))));
				assertThat(String.valueOf(constructorArguments.getArgumentValue(1, String.class).getValue()),
					is(equalTo("${gemfire.servers.hosts-and-ports}")));

				return null;
			}
		}).when(mockRegistry).registerBeanDefinition(eq(generatedBeanName(MethodInvokingBean.class)),
			any(BeanDefinition.class));

		List<BeanDefinition> servers = parser.parseServers(mockElement, mockRegistry);

		assertThat(servers, is(notNullValue()));
		assertThat(servers.isEmpty(), is(true));

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.ID_ATTRIBUTE));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockRegistry, times(1)).containsBeanDefinition(eq(generatedBeanName(MethodInvokingBean.class)));
		verify(mockRegistry, times(1)).registerBeanDefinition(eq(generatedBeanName(MethodInvokingBean.class)),
			isA(BeanDefinition.class));
	}

}
