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
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.Test;
import org.springframework.beans.PropertyValues;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * The PoolParserTest class is a test suite of test cases testing the contract and functionality
 * of the PoolParser class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.PoolParser
 * @since 1.7.0
 */
public class PoolParserTest {

	private PoolParser parser = new PoolParser();

	protected void assertBeanDefinition(BeanDefinition beanDefinition, String expectedHost, String expectedPort) {
		assertThat(beanDefinition, is(notNullValue()));
		assertThat(beanDefinition.getBeanClassName(), is(equalTo(ConnectionEndpoint.class.getName())));
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentCount(), is(equalTo(2)));
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentValue(0, String.class).getValue().toString(),
			is(equalTo(expectedHost)));
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentValue(1, String.class).getValue().toString(),
			is(equalTo(expectedPort)));
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
		Element mockLocatorOneElement = mock(Element.class, "testDoParse.MockLocatorOneElement");
		Element mockLocatorTwoElement = mock(Element.class, "testDoParse.MockLocatorTwoElement");
		Element mockServerElement = mock(Element.class, "testDoParse.MockServerElement");

		NodeList mockNodeList = mock(NodeList.class, "testDoParse.MockNodeList");

		when(mockPoolElement.hasAttribute(PoolParser.LOCATORS_ATTRIBUTE_NAME)).thenReturn(true);
		when(mockPoolElement.getAttribute(PoolParser.LOCATORS_ATTRIBUTE_NAME)).thenReturn("nebula[1234]");
		when(mockPoolElement.hasAttribute(PoolParser.SERVERS_ATTRIBUTE_NAME)).thenReturn(true);
		when(mockPoolElement.getAttribute(PoolParser.SERVERS_ATTRIBUTE_NAME)).thenReturn("skullbox[9876], backspace");
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(3);
		when(mockNodeList.item(eq(0))).thenReturn(mockLocatorOneElement);
		when(mockNodeList.item(eq(1))).thenReturn(mockServerElement);
		when(mockNodeList.item(eq(2))).thenReturn(mockLocatorTwoElement);
		when(mockLocatorOneElement.getLocalName()).thenReturn(PoolParser.LOCATOR_ELEMENT_NAME);
		when(mockLocatorOneElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("comet");
		when(mockLocatorOneElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn("1025");
		when(mockLocatorTwoElement.getLocalName()).thenReturn(PoolParser.LOCATOR_ELEMENT_NAME);
		when(mockLocatorTwoElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("quasar");
		when(mockLocatorTwoElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn(" ");
		when(mockServerElement.getLocalName()).thenReturn(PoolParser.SERVER_ELEMENT_NAME);
		when(mockServerElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("rightshift");
		when(mockServerElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn("4556");

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, builder);

		BeanDefinition poolDefinition = builder.getBeanDefinition();

		PropertyValues poolPropertyValues = poolDefinition.getPropertyValues();

		assertThat(poolDefinition, is(notNullValue()));
		assertThat(poolPropertyValues.contains("locatorEndpoints"), is(true));
		assertThat(poolPropertyValues.contains("locatorEndpointList"), is(false));
		assertThat(poolPropertyValues.contains("serverEndpoints"), is(true));
		assertThat(poolPropertyValues.contains("serverEndpointList"), is(false));

		ManagedList<BeanDefinition> locators = (ManagedList<BeanDefinition>)
			poolPropertyValues.getPropertyValue("locatorEndpoints").getValue();

		assertThat(locators, is(notNullValue()));
		assertThat(locators.size(), is(equalTo(3)));
		assertBeanDefinition(locators.get(0), "comet", "1025");
		assertBeanDefinition(locators.get(1), "quasar", String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(locators.get(2), "nebula", "1234");

		ManagedList<BeanDefinition> servers = (ManagedList<BeanDefinition>) poolDefinition.getPropertyValues()
			.getPropertyValue("serverEndpoints").getValue();

		assertThat(servers, is(notNullValue()));
		assertThat(servers.size(), is(equalTo(3)));
		assertBeanDefinition(servers.get(0), "rightshift", "4556");
		assertBeanDefinition(servers.get(1), "skullbox", "9876");
		assertBeanDefinition(servers.get(2), "backspace", String.valueOf(PoolParser.DEFAULT_SERVER_PORT));

		verify(mockPoolElement, times(1)).getChildNodes();
		verify(mockNodeList, times(4)).getLength();
		verify(mockNodeList, times(1)).item(eq(0));
		verify(mockNodeList, times(1)).item(eq(1));
		verify(mockNodeList, times(1)).item(eq(2));
		verify(mockPoolElement, never()).hasAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, never()).hasAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockLocatorOneElement, times(1)).getLocalName();
		verify(mockLocatorOneElement, times(1)).getAttribute(PoolParser.HOST_ATTRIBUTE_NAME);
		verify(mockLocatorOneElement, times(1)).getAttribute(PoolParser.PORT_ATTRIBUTE_NAME);
		verify(mockLocatorTwoElement, times(1)).getLocalName();
		verify(mockLocatorTwoElement, times(1)).getAttribute(PoolParser.HOST_ATTRIBUTE_NAME);
		verify(mockLocatorTwoElement, times(1)).getAttribute(PoolParser.PORT_ATTRIBUTE_NAME);
		verify(mockServerElement, times(1)).getLocalName();
		verify(mockServerElement, times(1)).getAttribute(PoolParser.HOST_ATTRIBUTE_NAME);
		verify(mockServerElement, times(1)).getAttribute(PoolParser.PORT_ATTRIBUTE_NAME);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doParseWithNoLocatorsOrServersSpecified() {
		Element mockPoolElement = mock(Element.class, "testDoParseWithNoLocatorsOrServersSpecified.MockPoolElement");

		NodeList mockNodeList = mock(NodeList.class, "testDoParseWithNoLocatorsOrServersSpecified.MockNodeList");

		when(mockNodeList.getLength()).thenReturn(0);
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockPoolElement.hasAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(false);
		when(mockPoolElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn("");
		when(mockPoolElement.hasAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(false);
		when(mockPoolElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn("");

		BeanDefinitionBuilder poolBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, poolBuilder);

		BeanDefinition poolDefinition = poolBuilder.getBeanDefinition();

		PropertyValues poolPropertyValues = poolDefinition.getPropertyValues();

		assertThat(poolDefinition, is(notNullValue()));
		assertThat(poolPropertyValues.contains("locatorEndpoints"), is(true));
		assertThat(poolPropertyValues.contains("locatorEndpointList"), is(false));
		assertThat(poolPropertyValues.contains("serverEndpoints"), is(false));
		assertThat(poolPropertyValues.contains("serverEndpointList"), is(false));

		ManagedList<BeanDefinition> locators = (ManagedList<BeanDefinition>)
			poolPropertyValues.getPropertyValue("locatorEndpoints").getValue();

		assertThat(locators, is(notNullValue()));
		assertThat(locators.size(), is(equalTo(1)));
		assertBeanDefinition(locators.get(0), PoolParser.DEFAULT_HOST, String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));

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
		Element mockPoolElement = mock(Element.class, "testDoParseWithServersAttributeValueSpecifiedAsAPropertyPlaceholder.MockPoolElement");

		NodeList mockNodeList = mock(NodeList.class, "testDoParseWithServersAttributeValueSpecifiedAsAPropertyPlaceholder.MockNodeList");

		when(mockNodeList.getLength()).thenReturn(0);
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockPoolElement.hasAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(false);
		when(mockPoolElement.hasAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(true);
		when(mockPoolElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn("");
		when(mockPoolElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn("${gemfire.server.hosts-and-ports}");

		BeanDefinitionBuilder poolBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, poolBuilder);

		BeanDefinition poolDefinition = poolBuilder.getBeanDefinition();

		PropertyValues poolPropertyValues = poolDefinition.getPropertyValues();

		assertThat(poolDefinition, is(notNullValue()));
		assertThat(poolPropertyValues.contains("locatorEndpoints"), is(false));
		assertThat(poolPropertyValues.contains("locatorEndpointList"), is(false));
		assertThat(poolPropertyValues.contains("serverEndpoints"), is(false));
		assertThat(poolPropertyValues.contains("serverEndpointList"), is(true));

		BeanDefinition servers = (BeanDefinition) poolPropertyValues.getPropertyValue("serverEndpointList").getValue();

		assertThat(servers, is(notNullValue()));
		assertThat(servers.getBeanClassName(), is(equalTo(ConnectionEndpointList.class.getName())));
		assertThat(servers.getFactoryMethodName(), is(equalTo("parse")));
		assertThat(servers.getConstructorArgumentValues().getArgumentValue(0, String.class).getValue().toString(),
			is(equalTo(String.valueOf(PoolParser.DEFAULT_SERVER_PORT))));
		assertThat(servers.getConstructorArgumentValues().getArgumentValue(1, String.class).getValue().toString(),
			is(equalTo("${gemfire.server.hosts-and-ports}")));

		verify(mockPoolElement, times(1)).getChildNodes();
		verify(mockNodeList, times(1)).getLength();
		verify(mockNodeList, never()).item(anyInt());
		verify(mockPoolElement, times(1)).hasAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).hasAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
	}

	@Test
	public void hasAttributesReturnsTrueAndShortcircuts() {
		Element mockElement = mock(Element.class, "testHasAttributesIsTrue.MockElement");

		when(mockElement.hasAttribute("attributeOne")).thenReturn(true);
		when(mockElement.hasAttribute("attributeTwo")).thenReturn(true);

		assertThat(parser.hasAttributes(mockElement, "attributeThree", "attributeTwo", "attributeOne"), is(true));

		verify(mockElement, times(1)).hasAttribute(eq("attributeThree"));
		verify(mockElement, times(1)).hasAttribute(eq("attributeTwo"));
		verify(mockElement, never()).hasAttribute(eq("attributeOne"));
	}

	@Test
	public void hasAttributesReturnsFalse() {
		Element mockElement = mock(Element.class, "testHasAttributesIsTrue.MockElement");

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
		assertThat(
			beanDefinition.getConstructorArgumentValues().getArgumentValue(0, Integer.class).getValue().toString(),
			is(equalTo(String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT))));
		assertThat(
			beanDefinition.getConstructorArgumentValues().getArgumentValue(1, String.class).getValue().toString(),
			is(equalTo("${test}")));
	}

	@Test
	public void buildConnectionsUsingServer() {
		BeanDefinition beanDefinition = parser.buildConnections("${test}", true);

		assertThat(beanDefinition, is(notNullValue()));
		assertThat(beanDefinition.getBeanClassName(), is(equalTo(ConnectionEndpointList.class.getName())));
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentValue(0, Integer.class).getValue().toString(),
			is(equalTo(String.valueOf(PoolParser.DEFAULT_SERVER_PORT))));
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentValue(1, String.class).getValue().toString(),
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
		Element mockElement = mock(Element.class, "testParseLocator.Element");

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn("skullbox");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("1234");

		assertBeanDefinition(parser.parseLocator(mockElement), "skullbox", "1234");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseLocatorWithNoHostPort() {
		Element mockElement = mock(Element.class, "testParseLocatorWithNoHostPort.Element");

		when(mockElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("");
		when(mockElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn(null);

		assertBeanDefinition(parser.parseLocator(mockElement), "localhost", "10334");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseLocators() {
		Element mockElement = mock(Element.class, "testParseLocators.Element");

		when(mockElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(
			"jupiter, saturn[1234],  [9876] ");

		List<BeanDefinition> locators = parser.parseLocators(mockElement, null);

		assertThat(locators, is(notNullValue()));
		assertThat(locators.size(), is(equalTo(3)));
		assertBeanDefinition(locators.get(0), "jupiter", String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(locators.get(1), "saturn", "1234");
		assertBeanDefinition(locators.get(2), "localhost", "9876");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
	}

	@Test
	public void parseLocatorsWithPropertyPlaceholder() {
		Element mockElement = mock(Element.class, "testParseLocatorsWithPropertyPlaceholder.Element");

		when(mockElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(
			"${gemfire.locators.hosts-and-ports}");

		BeanDefinitionBuilder locatorsBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockElement));

		List<BeanDefinition> locators = parser.parseLocators(mockElement, locatorsBuilder);

		assertThat(locators, is(notNullValue()));
		assertThat(locators.isEmpty(), is(true));

		BeanDefinition locatorDefinition = (BeanDefinition) locatorsBuilder.getBeanDefinition()
			.getPropertyValues().getPropertyValue("locatorEndpointList").getValue();

		assertThat(locatorDefinition, is(notNullValue()));
		assertThat(locatorDefinition.getBeanClassName(), is(equalTo(ConnectionEndpointList.class.getName())));
		assertThat(locatorDefinition.getFactoryMethodName(), is(equalTo("parse")));
		assertThat(locatorDefinition.getConstructorArgumentValues().getArgumentValue(0, String.class).getValue().toString(),
			is(equalTo(String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT))));
		assertThat(locatorDefinition.getConstructorArgumentValues().getArgumentValue(1, String.class).getValue().toString(),
			is(equalTo("${gemfire.locators.hosts-and-ports}")));

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
	}

	@Test
	public void parseServer() {
		Element mockElement = mock(Element.class, "testParseServer.Element");

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn("plato");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("9876");

		assertBeanDefinition(parser.parseServer(mockElement), "plato", "9876");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseServerWithNoHostPort() {
		Element mockElement = mock(Element.class, "testParseServerWithNoHostPort.Element");

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn(" ");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("");

		assertBeanDefinition(parser.parseServer(mockElement), "localhost", "40404");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseServers() {
		Element mockElement = mock(Element.class, "testParseServers.Element");

		when(mockElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(" neptune[], venus[9876]");

		List<BeanDefinition> servers = parser.parseServers(mockElement, null);

		assertNotNull(servers);
		assertFalse(servers.isEmpty());
		assertEquals(2, servers.size());
		assertBeanDefinition(servers.get(0), "neptune", String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(servers.get(1), "venus", "9876");
	}

	@Test
	public void parseServersWithPropertyPlaceholder() {
		Element mockElement = mock(Element.class, "testParseServersWithPropertyPlaceholder.Element");

		when(mockElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(
			"${gemfire.servers.hosts-and-ports}");

		BeanDefinitionBuilder serversBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockElement));

		List<BeanDefinition> servers = parser.parseServers(mockElement, serversBuilder);

		assertThat(servers, is(notNullValue()));
		assertThat(servers.isEmpty(), is(true));

		BeanDefinition serverDefinition = (BeanDefinition) serversBuilder.getBeanDefinition()
			.getPropertyValues().getPropertyValue("serverEndpointList").getValue();

		assertThat(serverDefinition, is(notNullValue()));
		assertThat(serverDefinition.getBeanClassName(), is(equalTo(ConnectionEndpointList.class.getName())));
		assertThat(serverDefinition.getFactoryMethodName(), is(equalTo("parse")));
		assertThat(serverDefinition.getConstructorArgumentValues().getArgumentValue(0, String.class).getValue().toString(),
			is(equalTo(String.valueOf(PoolParser.DEFAULT_SERVER_PORT))));
		assertThat(serverDefinition.getConstructorArgumentValues().getArgumentValue(1, String.class).getValue().toString(),
			is(equalTo("${gemfire.servers.hosts-and-ports}")));

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
	}

}
