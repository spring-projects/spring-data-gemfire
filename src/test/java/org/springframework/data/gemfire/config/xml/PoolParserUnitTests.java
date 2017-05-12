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

package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.junit.MockitoJUnitRunner;
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
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Unit tests for {@link PoolParser}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.xml.PoolParser
 * @since 1.7.0
 */
@RunWith(MockitoJUnitRunner.class)
public class PoolParserUnitTests {

	@Mock
	private BeanDefinitionRegistry mockRegistry;

	private PoolParser parser;

	@Before
	public void setup() {
		PoolParser.INFRASTRUCTURE_COMPONENTS_REGISTERED.set(true);

		parser = new PoolParser() {
			@Override BeanDefinitionRegistry getRegistry(ParserContext parserContext) {
				return mockRegistry;
			}
		};
	}

	protected void assertBeanDefinition(BeanDefinition beanDefinition, String expectedHost, String expectedPort) {
		assertThat(beanDefinition).isNotNull();
		assertThat(beanDefinition.getBeanClassName()).isEqualTo(ConnectionEndpoint.class.getName());
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentCount()).isEqualTo(2);
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentValue(0, String.class).getValue())
			.isEqualTo(expectedHost);
		assertThat(beanDefinition.getConstructorArgumentValues().getArgumentValue(1, String.class).getValue())
			.isEqualTo(expectedPort);
	}

	protected void assertPropertyNotPresent(BeanDefinition beanDefinition, String propertyName) {
		assertThat(beanDefinition.getPropertyValues().contains(propertyName)).isFalse();
	}

	protected void assertPropertyPresent(BeanDefinition beanDefinition, String propertyName) {
		assertThat(beanDefinition.getPropertyValues().contains(propertyName)).isTrue();
	}

	protected void assertPropertyValue(BeanDefinition beanDefinition, String propertyName, Object propertyValue) {
		assertThat(beanDefinition.getPropertyValues().getPropertyValue(propertyName).getValue())
			.isEqualTo(propertyValue);
	}

	protected String generateBeanName(Class<?> type) {
		return generateBeanName(type.getName());
	}

	protected String generateBeanName(String beanClassName) {
		return String.format("%1$s%2$s%3$d", beanClassName, BeanDefinitionReaderUtils.GENERATED_BEAN_NAME_SEPARATOR, 0);
	}

	protected Answer<Void> newAnswer(final String beanReference, final String targetMethod,
			final String host, final String port) {

		return new Answer<Void>() {
			@Override
			public Void answer(InvocationOnMock invocation) throws Throwable {
				String generatedName = invocation.getArgument(0);
				BeanDefinition methodInvokingBeanDefinition = invocation.getArgument(1);

				assertThat(methodInvokingBeanDefinition).isNotNull();
				assertThat(methodInvokingBeanDefinition.getBeanClassName()).isEqualTo(MethodInvokingBean.class.getName());
				assertThat(generatedName).isEqualTo(generateBeanName(methodInvokingBeanDefinition.getBeanClassName()));
				assertPropertyValue(methodInvokingBeanDefinition, "targetObject", new RuntimeBeanReference(beanReference));
				assertPropertyValue(methodInvokingBeanDefinition, "targetMethod", targetMethod);

				BeanDefinition argumentsDefinition = getPropertyValue(methodInvokingBeanDefinition, "arguments");

				assertThat(argumentsDefinition.getBeanClassName()).isEqualTo(ConnectionEndpointList.class.getName());

				ConstructorArgumentValues constructorArguments = argumentsDefinition.getConstructorArgumentValues();

				assertThat(constructorArguments.getArgumentCount()).isEqualTo(2);
				assertThat(constructorArguments.getArgumentValue(0, Integer.class).getValue()).isEqualTo(port);
				assertThat(constructorArguments.getArgumentValue(1, String.class).getValue()).isEqualTo(host);

				return null;
			}
		};
	}

	@SuppressWarnings("unchecked")
	protected <T> T getPropertyValue(BeanDefinition beanDefinition, String propertyName) {
		return (T) beanDefinition.getPropertyValues().getPropertyValue(propertyName).getValue();
	}

	@Test
	public void getBeanClassIsEqualToPoolFactoryBeanClass() {
		assertThat(parser.getBeanClass(null)).isEqualTo(PoolFactoryBean.class);
	}

	@Test
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
		when(mockPoolElement.getAttribute(PoolParser.LOCATORS_ATTRIBUTE_NAME)).thenReturn(null);
		when(mockPoolElement.getAttribute(PoolParser.SERVERS_ATTRIBUTE_NAME)).thenReturn(null);
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(3);
		when(mockNodeList.item(eq(0))).thenReturn(mockLocatorElementOne);
		when(mockNodeList.item(eq(1))).thenReturn(mockServerElement);
		when(mockNodeList.item(eq(2))).thenReturn(mockLocatorElementTwo);
		when(mockLocatorElementOne.getLocalName()).thenReturn(PoolParser.LOCATOR_ELEMENT_NAME);
		when(mockLocatorElementOne.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("venus");
		when(mockLocatorElementOne.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn("1025");
		when(mockLocatorElementTwo.getLocalName()).thenReturn(PoolParser.LOCATOR_ELEMENT_NAME);
		when(mockLocatorElementTwo.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("mars");
		when(mockLocatorElementTwo.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn(" ");
		when(mockServerElement.getLocalName()).thenReturn(PoolParser.SERVER_ELEMENT_NAME);
		when(mockServerElement.getAttribute(PoolParser.HOST_ATTRIBUTE_NAME)).thenReturn("skullbox");
		when(mockServerElement.getAttribute(PoolParser.PORT_ATTRIBUTE_NAME)).thenReturn("65535");

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, null, builder);

		BeanDefinition poolDefinition = builder.getBeanDefinition();

		assertThat(poolDefinition).isNotNull();

		assertPropertyValue(poolDefinition, "freeConnectionTimeout", "5000");
		assertPropertyValue(poolDefinition, "idleTimeout", "120000");
		assertPropertyValue(poolDefinition, "keepAlive", "true");
		assertPropertyValue(poolDefinition, "loadConditioningInterval", "300000");
		assertPropertyValue(poolDefinition, "maxConnections", "500");
		assertPropertyValue(poolDefinition, "minConnections", "50");
		assertPropertyValue(poolDefinition, "multiUserAuthentication", "true");
		assertPropertyValue(poolDefinition, "pingInterval", "15000");
		assertPropertyValue(poolDefinition, "prSingleHopEnabled", "true");
		assertPropertyValue(poolDefinition, "readTimeout", "20000");
		assertPropertyValue(poolDefinition, "retryAttempts", "1");
		assertPropertyValue(poolDefinition, "serverGroup", "TestGroup");
		assertPropertyValue(poolDefinition, "socketBufferSize", "16384");
		assertPropertyValue(poolDefinition, "statisticInterval", "500");
		assertPropertyValue(poolDefinition, "subscriptionAckInterval", "200");
		assertPropertyValue(poolDefinition, "subscriptionEnabled", "true");
		assertPropertyValue(poolDefinition, "subscriptionMessageTrackingTimeout", "30000");
		assertPropertyValue(poolDefinition, "subscriptionRedundancy", "2");
		assertPropertyValue(poolDefinition, "threadLocalConnections", "false");
		assertPropertyPresent(poolDefinition, "locators");
		assertPropertyPresent(poolDefinition, "servers");

		ManagedList<BeanDefinition> locators = getPropertyValue(poolDefinition, "locators");

		assertThat(locators).isNotNull();
		assertThat(locators.size()).isEqualTo(2);
		assertBeanDefinition(locators.get(0), "venus", "1025");
		assertBeanDefinition(locators.get(1), "mars", String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));

		ManagedList<BeanDefinition> servers = getPropertyValue(poolDefinition, "servers");

		assertThat(servers).isNotNull();
		assertThat(servers.size()).isEqualTo(1);
		assertBeanDefinition(servers.get(0), "skullbox", "65535");

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
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
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
	public void doParseWithNoLocatorsAndNoServersConfigured() {
		Element mockPoolElement = mock(Element.class);
		NodeList mockNodeList = mock(NodeList.class);

		when(mockPoolElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn("");
		when(mockPoolElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn("  ");
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(0);

		BeanDefinitionBuilder poolBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, null, poolBuilder);

		BeanDefinition poolDefinition = poolBuilder.getBeanDefinition();

		assertThat(poolDefinition).isNotNull();

		PropertyValues poolPropertyValues = poolDefinition.getPropertyValues();

		assertThat(poolPropertyValues.contains("locators")).isFalse();
		assertThat(poolPropertyValues.contains("servers")).isTrue();

		ManagedList<BeanDefinition> servers = getPropertyValue(poolDefinition, "servers");

		assertThat(servers).isNotNull();
		assertThat(servers.size()).isEqualTo(1);

		assertBeanDefinition(servers.get(0), PoolParser.DEFAULT_HOST, String.valueOf(PoolParser.DEFAULT_SERVER_PORT));

		verify(mockPoolElement, times(1)).getChildNodes();
		verify(mockNodeList, times(1)).getLength();
		verify(mockNodeList, never()).item(anyInt());
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
	}

	@Test
	public void doParseWithLocatorsAttributeConfiguredAsSpELExpression() {
		Element mockPoolElement = mock(Element.class);
		NodeList mockNodeList = mock(NodeList.class);

		when(mockPoolElement.getAttribute(PoolParser.ID_ATTRIBUTE)).thenReturn("TestPool");
		when(mockPoolElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(
			"#{T(example.app.config.GemFireProperties).locatorHostsPorts()}");
		when(mockPoolElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn("");
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(0);
		when(mockRegistry.containsBeanDefinition(anyString())).thenReturn(false);

		Answer<Void> answer = newAnswer("&TestPool", "addLocators",
			"#{T(example.app.config.GemFireProperties).locatorHostsPorts()}",
				String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));

		doAnswer(answer).when(mockRegistry).registerBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)),
			any(BeanDefinition.class));

		BeanDefinitionBuilder poolBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, null, poolBuilder);

		BeanDefinition poolDefinition = poolBuilder.getBeanDefinition();

		assertThat(poolDefinition).isNotNull();
		assertPropertyNotPresent(poolDefinition, "locators");
		assertPropertyNotPresent(poolDefinition, "servers");

		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.ID_ATTRIBUTE));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getChildNodes();
		verify(mockNodeList, times(1)).getLength();
		verify(mockNodeList, never()).item(anyInt());
		verify(mockRegistry, times(1)).containsBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)));
		verify(mockRegistry, times(1)).registerBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)),
			isA(BeanDefinition.class));
	}

	@Test
	public void doParseWithServersAttributeConfiguredAsPropertyPlaceholder() {
		Element mockPoolElement = mock(Element.class);
		NodeList mockNodeList = mock(NodeList.class);

		when(mockPoolElement.getAttribute(PoolParser.ID_ATTRIBUTE)).thenReturn("TestPool");
		when(mockPoolElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn("");
		when(mockPoolElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn(
			"${gemfire.server.hosts-and-ports}");
		when(mockPoolElement.getChildNodes()).thenReturn(mockNodeList);
		when(mockNodeList.getLength()).thenReturn(0);
		when(mockRegistry.containsBeanDefinition(anyString())).thenReturn(false);

		Answer<Void> answer = newAnswer("&TestPool", "addServers", "${gemfire.server.hosts-and-ports}",
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));

		doAnswer(answer).when(mockRegistry).registerBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)),
			any(BeanDefinition.class));

		BeanDefinitionBuilder poolBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			parser.getBeanClass(mockPoolElement));

		parser.doParse(mockPoolElement, null, poolBuilder);

		BeanDefinition poolDefinition = poolBuilder.getBeanDefinition();

		assertThat(poolDefinition).isNotNull();
		assertPropertyNotPresent(poolDefinition, "locators");
		assertPropertyNotPresent(poolDefinition, "servers");

		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.ID_ATTRIBUTE));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
		verify(mockPoolElement, times(1)).getChildNodes();
		verify(mockNodeList, times(1)).getLength();
		verify(mockNodeList, never()).item(anyInt());
		verify(mockRegistry, times(1)).containsBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)));
		verify(mockRegistry, times(1)).registerBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)),
			isA(BeanDefinition.class));
	}

	@Test
	public void buildConnection() {
		assertBeanDefinition(parser.buildConnection("earth", "1234", true), "earth", "1234");
		assertBeanDefinition(parser.buildConnection("mars", " ", true), "mars",
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(parser.buildConnection("  ", "1234", true), PoolParser.DEFAULT_HOST, "1234");
		assertBeanDefinition(parser.buildConnection("  ", "", true), PoolParser.DEFAULT_HOST,
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertBeanDefinition(parser.buildConnection("jupiter", "9876", false), "jupiter", "9876");
		assertBeanDefinition(parser.buildConnection("saturn", null, false), "saturn",
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertBeanDefinition(parser.buildConnection(null, "9876", false), PoolParser.DEFAULT_HOST, "9876");
		assertBeanDefinition(parser.buildConnection("", "  ", false), PoolParser.DEFAULT_HOST,
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
	}

	@Test
	public void buildConnectionsUsingLocator() {
		BeanDefinition beanDefinition = parser.buildConnections("${locators}", false);

		assertThat(beanDefinition).isNotNull();
		assertThat(beanDefinition.getBeanClassName()).isEqualTo(ConnectionEndpointList.class.getName());

		ConstructorArgumentValues constructorArguments = beanDefinition.getConstructorArgumentValues();

		assertThat(constructorArguments).isNotNull();
		assertThat(constructorArguments.getArgumentCount()).isEqualTo(2);
		assertThat(constructorArguments.getArgumentValue(0, Integer.class).getValue())
			.isEqualTo(String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertThat(constructorArguments.getArgumentValue(1, String.class).getValue()).isEqualTo("${locators}");
	}

	@Test
	public void buildConnectionsUsingServer() {
		BeanDefinition beanDefinition = parser.buildConnections("#{servers}", true);

		assertThat(beanDefinition).isNotNull();
		assertThat(beanDefinition.getBeanClassName()).isEqualTo(ConnectionEndpointList.class.getName());

		ConstructorArgumentValues constructorArguments = beanDefinition.getConstructorArgumentValues();

		assertThat(constructorArguments).isNotNull();
		assertThat(constructorArguments.getArgumentCount()).isEqualTo(2);
		assertThat(constructorArguments.getArgumentValue(0, Integer.class).getValue())
			.isEqualTo(String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertThat(constructorArguments.getArgumentValue(1, String.class).getValue()).isEqualTo("#{servers}");
	}

	@Test
	public void defaultHost() {
		assertThat(parser.defaultHost("skullbox")).isEqualTo("skullbox");
		assertThat(parser.defaultHost("  ")).isEqualTo("localhost");
		assertThat(parser.defaultHost("")).isEqualTo("localhost");
		assertThat(parser.defaultHost(null)).isEqualTo("localhost");
	}

	@Test
	public void defaultPort() {
		assertThat(parser.defaultPort("1234", true)).isEqualTo("1234");
		assertThat(parser.defaultPort("9876", false)).isEqualTo("9876");
		assertThat(parser.defaultPort("  ", true)).isEqualTo(String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
		assertThat(parser.defaultPort("", false)).isEqualTo(String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));
		assertThat(parser.defaultPort(null, true)).isEqualTo(String.valueOf(PoolParser.DEFAULT_SERVER_PORT));
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

		assertBeanDefinition(parser.parseLocator(mockElement), PoolParser.DEFAULT_HOST,
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseLocators() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.ID_ATTRIBUTE))).thenReturn("TestPool");
		when(mockElement.getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME))).thenReturn(
			"jupiter, saturn[1234],  [9876] ");
		when(mockRegistry.containsBeanDefinition(anyString())).thenReturn(false);

		Answer<Void> answer = newAnswer("&TestPool", "addLocators", "jupiter, saturn[1234],  [9876] ",
			String.valueOf(PoolParser.DEFAULT_LOCATOR_PORT));

		doAnswer(answer).when(mockRegistry).registerBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)),
			any(BeanDefinition.class));

		BeanDefinitionBuilder poolBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		assertThat(parser.parseLocators(mockElement, poolBuilder, mockRegistry)).isTrue();

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.ID_ATTRIBUTE));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.LOCATORS_ATTRIBUTE_NAME));
		verify(mockRegistry, times(1)).containsBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)));
		verify(mockRegistry, times(1)).registerBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)),
			isA(BeanDefinition.class));
	}

	@Test
	public void parseServer() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn("pluto");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("9876");

		assertBeanDefinition(parser.parseServer(mockElement), "pluto", "9876");

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseServerWithNoHostPort() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME))).thenReturn(" ");
		when(mockElement.getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME))).thenReturn("");

		assertBeanDefinition(parser.parseServer(mockElement), PoolParser.DEFAULT_HOST,
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.HOST_ATTRIBUTE_NAME));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.PORT_ATTRIBUTE_NAME));
	}

	@Test
	public void parseServers() {
		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq(PoolParser.ID_ATTRIBUTE))).thenReturn("TestPool");
		when(mockElement.getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME))).thenReturn("mars[], venus[9876]");
		when(mockRegistry.containsBeanDefinition(anyString())).thenReturn(false);

		Answer<Void> answer = newAnswer("&TestPool", "addServers", "mars[], venus[9876]",
			String.valueOf(PoolParser.DEFAULT_SERVER_PORT));

		doAnswer(answer).when(mockRegistry).registerBeanDefinition(eq(generateBeanName(MethodInvokingBean.class)),
			any(BeanDefinition.class));

		BeanDefinitionBuilder poolBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		assertThat(parser.parseServers(mockElement, poolBuilder, mockRegistry)).isTrue();

		verify(mockElement, times(1)).getAttribute(eq(PoolParser.ID_ATTRIBUTE));
		verify(mockElement, times(1)).getAttribute(eq(PoolParser.SERVERS_ATTRIBUTE_NAME));
	}
}
