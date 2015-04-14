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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.data.gemfire.client.PoolFactoryBean;
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
		assertNotNull(beanDefinition);
		assertEquals(2, beanDefinition.getConstructorArgumentValues().getArgumentCount());
		assertEquals(expectedHost, beanDefinition.getConstructorArgumentValues()
			.getArgumentValue(0, String.class).getValue());
		assertEquals(expectedPort, beanDefinition.getConstructorArgumentValues()
			.getArgumentValue(1, String.class).getValue());
	}

	@Test
	public void testGetBeanClass() {
		assertEquals(PoolFactoryBean.class, parser.getBeanClass(null));
	}

	@Test
	public void testBuildConnection() {
		assertBeanDefinition(parser.buildConnection("skullbox", "1234", true), "skullbox", "1234");
		assertBeanDefinition(parser.buildConnection("saturn", " ", true), "saturn",
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(parser.buildConnection("  ", "", true), PoolParser.DEFAULT_HOST,
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(parser.buildConnection("neptune", "9876", false), "neptune", "9876");
		assertBeanDefinition(parser.buildConnection("jupiter", null, false), "jupiter",
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(parser.buildConnection("", "  ", false), PoolParser.DEFAULT_HOST,
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
	}

	@Test
	public void testDefaultHost() {
		assertEquals("skullbox", parser.defaultHost("skullbox"));
		assertEquals("localhost", parser.defaultHost(null));
		assertEquals("localhost", parser.defaultHost(""));
		assertEquals("localhost", parser.defaultHost("  "));
	}

	@Test
	public void testDefaultPort() {
		assertEquals("1234", parser.defaultPort("1234", true));
		assertEquals("9876", parser.defaultPort("9876", false));
		assertEquals(String.valueOf(PoolParser.DEFAULT_SERVER_PORT), parser.defaultPort(null, true));
		assertEquals(String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT), parser.defaultPort("", false));
		assertEquals(String.valueOf(PoolParser.DEFAULT_SERVER_PORT), parser.defaultPort("  ", true));
	}

	@Test
	public void testParseConnection() {
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
	public void testParseSingleConnection() {
		ManagedList<BeanDefinition> beans = parser.parseConnections("skullbox[1234]", true);

		assertNotNull(beans);
		assertFalse(beans.isEmpty());
		assertEquals(1, beans.size());
		assertBeanDefinition(beans.get(0), "skullbox", "1234");
	}

	@Test
	public void testParseMultipleConnections() {
		ManagedList<BeanDefinition> beans = parser.parseConnections(
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
	public void testParseDigits() {
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
	public void testParseLocator() {
		Element mockElement = mock(Element.class, "testParseLocator.Element");

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn("skullbox");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("1234");

		assertBeanDefinition(parser.parseLocator(mockElement), "skullbox", "1234");
	}

	@Test
	public void testParseLocators() {
		Element mockElement = mock(Element.class, "testParseLocators.Element");

		when(mockElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn("jupiter, saturn[1234]");

		ManagedList<BeanDefinition> locators = parser.parseLocators(mockElement);

		assertNotNull(locators);
		assertFalse(locators.isEmpty());
		assertEquals(2, locators.size());
		assertBeanDefinition(locators.get(0), "jupiter", String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(locators.get(1), "saturn", "1234");
	}

	@Test
	public void testParseServer() {
		Element mockElement = mock(Element.class, "testParseServer.Element");

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn("plato");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("9876");

		assertBeanDefinition(parser.parseServer(mockElement), "plato", "9876");
	}

	@Test
	public void testParseServers() {
		Element mockElement = mock(Element.class, "testParseServers.Element");

		when(mockElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(" neptune[], venus[9876]");

		ManagedList<BeanDefinition> servers = parser.parseServers(mockElement);

		assertNotNull(servers);
		assertFalse(servers.isEmpty());
		assertEquals(2, servers.size());
		assertBeanDefinition(servers.get(0), "neptune", String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(servers.get(1), "venus", "9876");
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testPostProcess() {
		Element mockPoolElement = mock(Element.class, "testPostProcess.MockPoolElement");
		Element mockLocatorOneElement = mock(Element.class, "testPostProcess.MockLocatorOneElement");
		Element mockLocatorTwoElement = mock(Element.class, "testPostProcess.MockLocatorTwoElement");
		Element mockServerElement = mock(Element.class, "testPostProcess.MockServerElement");

		NodeList mockNodeList = mock(NodeList.class, "testPostProcess.MockNodeList");

		when(mockPoolElement.getAttribute(PoolParser.LOCATORS_ATTRIBUTE_NAME)).thenReturn("nebula[1122]");
		when(mockPoolElement.getAttribute(PoolParser.SERVERS_ATTRIBUTE_NAME)).thenReturn("skullbox[4848], backspace");
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(3);
		when(mockNodeList.item(eq(0))).thenReturn(mockLocatorOneElement);
		when(mockNodeList.item(eq(1))).thenReturn(mockServerElement);
		when(mockNodeList.item(eq(2))).thenReturn(mockLocatorTwoElement);
		when(mockLocatorOneElement.getLocalName()).thenReturn(PoolParser.LOCATOR_ELEMENT_NAME);
		when(mockLocatorOneElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("comet");
		when(mockLocatorOneElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn("1034");
		when(mockLocatorTwoElement.getLocalName()).thenReturn(PoolParser.LOCATOR_ELEMENT_NAME);
		when(mockLocatorTwoElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("quasar");
		when(mockLocatorTwoElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn(" ");
		when(mockServerElement.getLocalName()).thenReturn(PoolParser.SERVER_ELEMENT_NAME);
		when(mockServerElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("rightshift");
		when(mockServerElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn("4554");

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.postProcess(builder, mockPoolElement);

		BeanDefinition poolDefinition = builder.getRawBeanDefinition();

		assertNotNull(poolDefinition);

		ManagedList<BeanDefinition> locators = (ManagedList<BeanDefinition>) poolDefinition.getPropertyValues()
			.getPropertyValue("locators").getValue();

		assertNotNull(locators);
		assertFalse(locators.isEmpty());
		assertEquals(3, locators.size());
		assertBeanDefinition(locators.get(0), "comet", "1034");
		assertBeanDefinition(locators.get(1), "quasar", String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(locators.get(2), "nebula", "1122");

		ManagedList<BeanDefinition> servers = (ManagedList<BeanDefinition>) poolDefinition.getPropertyValues()
			.getPropertyValue("servers").getValue();

		assertNotNull(servers);
		assertFalse(servers.isEmpty());
		assertEquals(3, servers.size());
		assertBeanDefinition(servers.get(0), "rightshift", "4554");
		assertBeanDefinition(servers.get(1), "skullbox", "4848");
		assertBeanDefinition(servers.get(2), "backspace", String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
	}

}
