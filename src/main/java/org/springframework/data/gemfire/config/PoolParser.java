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

import java.net.InetSocketAddress;
import java.util.List;

import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

import com.gemstone.gemfire.cache.server.CacheServer;
import com.gemstone.gemfire.internal.DistributionLocator;

/**
 * Parser for GFE &lt;pool;gt; bean definitions.
 *  
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
class PoolParser extends AbstractSimpleBeanDefinitionParser {

	protected static final int DEFAULT_LOCATOR_PORT = DistributionLocator.DEFAULT_LOCATOR_PORT;
	protected static final int DEFAULT_SERVER_PORT = CacheServer.DEFAULT_PORT;

	protected static final String DEFAULT_HOST = "localhost";
	protected static final String HOST_ATTRIBUTE_NAME = "host";
	protected static final String LOCATOR_ELEMENT_NAME = "locator";
	protected static final String LOCATORS_ATTRIBUTE_NAME = "locators";
	protected static final String PORT_ATTRIBUTE_NAME = "port";
	protected static final String SERVER_ELEMENT_NAME = "server";
	protected static final String SERVERS_ATTRIBUTE_NAME = "servers";

	@Override
	protected Class<?> getBeanClass(Element element) {
		return PoolFactoryBean.class;
	}
	
	@Override
	protected void postProcess(BeanDefinitionBuilder builder, Element element) {
		List<Element> subElements = DomUtils.getChildElements(element);

		ManagedList<BeanDefinition> locators = new ManagedList<BeanDefinition>(subElements.size());
		ManagedList<BeanDefinition> servers = new ManagedList<BeanDefinition>(subElements.size());

		// parse nested locator/server elements
		for (Element subElement : subElements) {
			String name = subElement.getLocalName();

			if (LOCATOR_ELEMENT_NAME.equals(name)) {
				locators.add(parseLocator(subElement));
			}
			if (SERVER_ELEMENT_NAME.equals(name)) {
				servers.add(parseServer(subElement));
			}
		}

		locators.addAll(parseLocators(element));
		servers.addAll(parseServers(element));

		if (!locators.isEmpty()) {
			builder.addPropertyValue("locators", locators);
		}

		if (!servers.isEmpty()) {
			builder.addPropertyValue("servers", servers);
		}
	}

	/* (non-Javadoc) */
	BeanDefinition buildConnection(String host, String port, boolean server) {
		BeanDefinitionBuilder inetSocketAddressBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			InetSocketAddress.class);

		inetSocketAddressBuilder.addConstructorArgValue(defaultHost(host));
		inetSocketAddressBuilder.addConstructorArgValue(defaultPort(port, server));

		return inetSocketAddressBuilder.getBeanDefinition();
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
	ManagedList<BeanDefinition> parseConnections(String hostPortCommaDelimitedList, boolean server) {
		ManagedList<BeanDefinition> connections = new ManagedList<BeanDefinition>();

		if (StringUtils.hasText(hostPortCommaDelimitedList)) {
			String[] hostPorts = hostPortCommaDelimitedList.split(",");

			for (String hostPort : hostPorts) {
				connections.add(parseConnection(hostPort, server));
			}
		}

		return connections;
	}

	/* (non-Javadoc) */
	BeanDefinition parseConnection(String hostPort, boolean server) {
		String port = defaultPort(null, server);
		String host;

		int portIndex = hostPort.indexOf('[');

		if (portIndex > -1) {
			host = hostPort.substring(0, portIndex).trim();
			port = parseDigits(hostPort.substring(portIndex)).trim();
		}
		else {
			host = hostPort.trim();
		}

		return buildConnection(host, port, server);
	}

	/* (non-Javadoc) */
	String parseDigits(String value) {
		StringBuilder digits = new StringBuilder();

		for (char chr : value.toCharArray()) {
			if (Character.isDigit(chr)) {
				digits.append(chr);
			}
		}

		return digits.toString();
	}

	/* (non-Javadoc) */
	BeanDefinition parseLocator(Element element) {
		return buildConnection(element.getAttribute(HOST_ATTRIBUTE_NAME), element.getAttribute(PORT_ATTRIBUTE_NAME), false);
	}

	ManagedList<BeanDefinition> parseLocators(Element element) {
		return parseConnections(element.getAttribute(LOCATORS_ATTRIBUTE_NAME), false);
	}

	/* (non-Javadoc) */
	BeanDefinition parseServer(Element element) {
		return buildConnection(element.getAttribute(HOST_ATTRIBUTE_NAME), element.getAttribute(PORT_ATTRIBUTE_NAME), false);
	}

	ManagedList<BeanDefinition> parseServers(Element element) {
		return parseConnections(element.getAttribute(SERVERS_ATTRIBUTE_NAME), true);
	}

	/* (non-Javadoc) */
	@Override
	protected String resolveId(Element element, AbstractBeanDefinition definition, ParserContext parserContext)
			throws BeanDefinitionStoreException {

		String id = super.resolveId(element, definition, parserContext);

		if (!StringUtils.hasText(id)) {
			id = GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME;
			// for backward compatibility
			parserContext.getRegistry().registerAlias(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME, "gemfirePool");
			parserContext.getRegistry().registerAlias(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME, "gemfire-pool");
		}

		return id;
	}

}
