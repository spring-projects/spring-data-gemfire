/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.config;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.MethodInvokingBean;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Parser for GFE &lt;pool;gt; bean definitions.
 *  
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
class PoolParser extends AbstractSingleBeanDefinitionParser {

	static final AtomicBoolean INFRASTRUCTURE_REGISTRATION = new AtomicBoolean(false);

	protected static final int DEFAULT_LOCATOR_PORT = GemfireUtils.DEFAULT_LOCATOR_PORT;
	protected static final int DEFAULT_SERVER_PORT = GemfireUtils.DEFAULT_CACHE_SERVER_PORT;

	protected static final Pattern PROPERTY_PLACEHOLDER_PATTERN = Pattern.compile("\\$\\{.+\\}");

	protected static final String DEFAULT_HOST = "localhost";
	protected static final String HOST_ATTRIBUTE_NAME = "host";
	protected static final String LOCATOR_ELEMENT_NAME = "locator";
	protected static final String LOCATORS_ATTRIBUTE_NAME = "locators";
	protected static final String PORT_ATTRIBUTE_NAME = "port";
	protected static final String SERVER_ELEMENT_NAME = "server";
	protected static final String SERVERS_ATTRIBUTE_NAME = "servers";

	/* (non-Javadoc) */
	static void registerSupportingInfrastructureComponents(ParserContext parserContext) {
		if (INFRASTRUCTURE_REGISTRATION.compareAndSet(false, true)) {
			AbstractBeanDefinition beanDefinition = BeanDefinitionBuilder
				.genericBeanDefinition(ClientRegionAndPoolBeanFactoryPostProcessor.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE)
				.getBeanDefinition();

			BeanDefinitionReaderUtils.registerWithGeneratedName(beanDefinition, parserContext.getRegistry());
		}
	}

	@Override
	protected Class<?> getBeanClass(Element element) {
		return PoolFactoryBean.class;
	}

	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {

		registerSupportingInfrastructureComponents(parserContext);

		ParsingUtils.setPropertyValue(element, builder, "free-connection-timeout");
		ParsingUtils.setPropertyValue(element, builder, "idle-timeout");
		ParsingUtils.setPropertyValue(element, builder, "keep-alive");
		ParsingUtils.setPropertyValue(element, builder, "load-conditioning-interval");
		ParsingUtils.setPropertyValue(element, builder, "max-connections");
		ParsingUtils.setPropertyValue(element, builder, "min-connections");
		ParsingUtils.setPropertyValue(element, builder, "multi-user-authentication");
		ParsingUtils.setPropertyValue(element, builder, "ping-interval");
		ParsingUtils.setPropertyValue(element, builder, "pr-single-hop-enabled");
		ParsingUtils.setPropertyValue(element, builder, "read-timeout");
		ParsingUtils.setPropertyValue(element, builder, "retry-attempts");
		ParsingUtils.setPropertyValue(element, builder, "server-group");
		ParsingUtils.setPropertyValue(element, builder, "socket-buffer-size");
		ParsingUtils.setPropertyValue(element, builder, "statistic-interval");
		ParsingUtils.setPropertyValue(element, builder, "subscription-ack-interval");
		ParsingUtils.setPropertyValue(element, builder, "subscription-enabled");
		ParsingUtils.setPropertyValue(element, builder, "subscription-message-tracking-timeout");
		ParsingUtils.setPropertyValue(element, builder, "subscription-redundancy");
		ParsingUtils.setPropertyValue(element, builder, "thread-local-connections");

		List<Element> childElements = DomUtils.getChildElements(element);

		ManagedList<BeanDefinition> locators = new ManagedList<BeanDefinition>(childElements.size());
		ManagedList<BeanDefinition> servers = new ManagedList<BeanDefinition>(childElements.size());

		for (Element childElement : childElements) {
			String childElementName = childElement.getLocalName();

			if (LOCATOR_ELEMENT_NAME.equals(childElementName)) {
				locators.add(parseLocator(childElement));
			}

			if (SERVER_ELEMENT_NAME.equals(childElementName)) {
				servers.add(parseServer(childElement));
			}
		}

		locators.addAll(parseLocators(element, getRegistry(parserContext)));
		servers.addAll(parseServers(element, getRegistry(parserContext)));

		// NOTE if neither Locators nor Servers were specified, then setup a connection to a Server
		// running on localhost, listening on the default CacheServer port, 40404
		if (childElements.isEmpty() && !hasAttributes(element, LOCATORS_ATTRIBUTE_NAME, SERVERS_ATTRIBUTE_NAME)) {
			servers.add(buildConnection(DEFAULT_HOST, String.valueOf(DEFAULT_SERVER_PORT), false));
		}

		if (!locators.isEmpty()) {
			builder.addPropertyValue("locators", locators);
		}

		if (!servers.isEmpty()) {
			builder.addPropertyValue("servers", servers);
		}
	}

	/* (non-Javadoc) */
	boolean hasAttributes(Element element, String... attributeNames) {
		for (String attributeName : attributeNames) {
			if (element.hasAttribute(attributeName)) {
				return true;
			}
		}

		return false;
	}

	/* (non-Javadoc) */
	boolean isPropertyPlaceholder(String value) {
		return PROPERTY_PLACEHOLDER_PATTERN.matcher(value).matches();
	}

	/* (non-Javadoc) */
	BeanDefinitionRegistry getRegistry(ParserContext parserContext) {
		return parserContext.getRegistry();
	}

	/* (non-Javadoc) */
	BeanDefinition buildConnections(String propertyPlaceholder, boolean server) {
		BeanDefinitionBuilder connectionEndpointListBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			ConnectionEndpointList.class);

		connectionEndpointListBuilder.setFactoryMethod("parse");
		connectionEndpointListBuilder.addConstructorArgValue(defaultPort(null, server));
		connectionEndpointListBuilder.addConstructorArgValue(propertyPlaceholder);

		return connectionEndpointListBuilder.getBeanDefinition();
	}

	/* (non-Javadoc) */
	BeanDefinition buildConnection(String host, String port, boolean server) {
		BeanDefinitionBuilder connectionEndpointBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			ConnectionEndpoint.class);

		connectionEndpointBuilder.addConstructorArgValue(defaultHost(host));
		connectionEndpointBuilder.addConstructorArgValue(defaultPort(port, server));

		return connectionEndpointBuilder.getBeanDefinition();
	}

	/* (non-Javadoc) */
	String defaultHost(String host) {
		return (StringUtils.hasText(host) ? host : DEFAULT_HOST);
	}

	/* (non-Javadoc) */
	String defaultPort(String port, boolean server) {
		return (StringUtils.hasText(port) ? port : (server ? String.valueOf(DEFAULT_SERVER_PORT)
			: String.valueOf(DEFAULT_LOCATOR_PORT)));
	}

	/* (non-Javadoc) */
	List<BeanDefinition> parseConnections(String hostPortCommaDelimitedList, boolean server) {
		List<BeanDefinition> connections = Collections.emptyList();

		if (StringUtils.hasText(hostPortCommaDelimitedList)) {
			String[] hostsPorts = hostPortCommaDelimitedList.split(",");

			connections = new ArrayList<BeanDefinition>(hostsPorts.length);

			for (String hostPort : hostsPorts) {
				connections.add(parseConnection(hostPort, server));
			}
		}

		return connections;
	}

	/* (non-Javadoc) */
	BeanDefinition parseConnection(String hostPort, boolean server) {
		String host = hostPort.trim();
		String port = defaultPort(null, server);

		int portIndex = host.indexOf('[');

		if (portIndex > -1) {
			port = parseDigits(host.substring(portIndex)).trim();
			host = host.substring(0, portIndex).trim();
		}

		return buildConnection(host, port, server);
	}

	/* (non-Javadoc) */
	String parseDigits(String value) {
		StringBuilder digits = new StringBuilder();

		for (char character : value.toCharArray()) {
			if (Character.isDigit(character)) {
				digits.append(character);
			}
		}

		return digits.toString();
	}

	/* (non-Javadoc) */
	BeanDefinition parseLocator(Element element) {
		return buildConnection(element.getAttribute(HOST_ATTRIBUTE_NAME),
			element.getAttribute(PORT_ATTRIBUTE_NAME), false);
	}

	/* (non-Javadoc) */
	List<BeanDefinition> parseLocators(Element element, BeanDefinitionRegistry registry) {
		List<BeanDefinition> beanDefinitions = Collections.emptyList();

		String locatorsAttributeValue = element.getAttribute(LOCATORS_ATTRIBUTE_NAME);

		if (isPropertyPlaceholder(locatorsAttributeValue)) {
			BeanDefinitionBuilder addLocatorsMethodInvokingBeanBuilder = BeanDefinitionBuilder.genericBeanDefinition(
				MethodInvokingBean.class);

			addLocatorsMethodInvokingBeanBuilder.addPropertyReference("targetObject", resolveDereferencedId(element));
			addLocatorsMethodInvokingBeanBuilder.addPropertyValue("targetMethod", "addLocators");
			addLocatorsMethodInvokingBeanBuilder.addPropertyValue("arguments",
				buildConnections(locatorsAttributeValue, false));

			AbstractBeanDefinition addLocatorsMethodInvokingBean =
				addLocatorsMethodInvokingBeanBuilder.getBeanDefinition();

			BeanDefinitionReaderUtils.registerWithGeneratedName(addLocatorsMethodInvokingBean, registry);
		}
		else {
			beanDefinitions = parseConnections(locatorsAttributeValue, false);
		}

		return beanDefinitions;
	}

	/* (non-Javadoc) */
	BeanDefinition parseServer(Element element) {
		return buildConnection(element.getAttribute(HOST_ATTRIBUTE_NAME),
			element.getAttribute(PORT_ATTRIBUTE_NAME), true);
	}

	/* (non-Javadoc) */
	List<BeanDefinition> parseServers(Element element, BeanDefinitionRegistry registry) {
		List<BeanDefinition> beanDefinitions = Collections.emptyList();

		String serversAttributeValue = element.getAttribute(SERVERS_ATTRIBUTE_NAME);

		if (isPropertyPlaceholder(serversAttributeValue)) {
			BeanDefinitionBuilder addServersMethodInvokingBeanBuilder = BeanDefinitionBuilder.genericBeanDefinition(
				MethodInvokingBean.class);

			addServersMethodInvokingBeanBuilder.addPropertyReference("targetObject", resolveDereferencedId(element));
			addServersMethodInvokingBeanBuilder.addPropertyValue("targetMethod", "addServers");
			addServersMethodInvokingBeanBuilder.addPropertyValue("arguments",
				buildConnections(serversAttributeValue, true));

			AbstractBeanDefinition addServersMethodInvokingBean =
				addServersMethodInvokingBeanBuilder.getBeanDefinition();

			BeanDefinitionReaderUtils.registerWithGeneratedName(addServersMethodInvokingBean, registry);
		}
		else {
			beanDefinitions = parseConnections(serversAttributeValue, true);
		}

		return beanDefinitions;
	}

	/* (non-Javadoc) */
	String resolveId(Element element) {
		String id = element.getAttribute(ID_ATTRIBUTE);
		return (StringUtils.hasText(id) ? id : GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);
	}

	/* (non-Javadoc) */
	String resolveDereferencedId(Element element) {
		return SpringUtils.dereferenceBean(resolveId(element));
	}

	/* (non-Javadoc) */
	@Override
	protected String resolveId(Element element, AbstractBeanDefinition definition, ParserContext parserContext)
			throws BeanDefinitionStoreException {

		String id = super.resolveId(element, definition, parserContext);

		if (!StringUtils.hasText(id)) {
			id = GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME;
			parserContext.getRegistry().registerAlias(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME, "gemfire-pool");
		}

		return id;
	}

}
