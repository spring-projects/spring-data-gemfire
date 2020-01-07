/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeList;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

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
import org.springframework.data.gemfire.config.support.ClientRegionPoolBeanFactoryPostProcessor;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Bean definition parser for &lt;gfe:pool&gt; SDG XML namespace (XSD) elements.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 */
class PoolParser extends AbstractSingleBeanDefinitionParser {

	static final AtomicBoolean INFRASTRUCTURE_COMPONENTS_REGISTERED = new AtomicBoolean(false);

	static final int DEFAULT_LOCATOR_PORT = GemfireUtils.DEFAULT_LOCATOR_PORT;
	static final int DEFAULT_SERVER_PORT = GemfireUtils.DEFAULT_CACHE_SERVER_PORT;

	static final String DEFAULT_HOST = "localhost";
	static final String HOST_ATTRIBUTE_NAME = "host";
	static final String LOCATOR_ELEMENT_NAME = "locator";
	static final String LOCATORS_ATTRIBUTE_NAME = "locators";
	static final String PORT_ATTRIBUTE_NAME = "port";
	static final String SERVER_ELEMENT_NAME = "server";
	static final String SERVERS_ATTRIBUTE_NAME = "servers";

	private static void registerInfrastructureComponents(ParserContext parserContext) {

		if (INFRASTRUCTURE_COMPONENTS_REGISTERED.compareAndSet(false, true)) {

			// Be careful to not to register this infrastructure component (just yet; requires more thought)
			/*
			BeanDefinitionReaderUtils.registerWithGeneratedName(
				BeanDefinitionBuilder.rootBeanDefinition(ClientCachePoolBeanFactoryPostProcessor.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE)
				.getBeanDefinition(), parserContext.getRegistry());
			*/

			BeanDefinitionReaderUtils.registerWithGeneratedName(
				BeanDefinitionBuilder.rootBeanDefinition(ClientRegionPoolBeanFactoryPostProcessor.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE)
				.getBeanDefinition(), parserContext.getRegistry());
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return PoolFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder poolBuilder) {

		registerInfrastructureComponents(parserContext);

		// Be careful not to enable this dependency
		//poolBuilder.addDependsOn(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

		ParsingUtils.setPropertyValue(element, poolBuilder, "free-connection-timeout");
		ParsingUtils.setPropertyValue(element, poolBuilder, "idle-timeout");
		ParsingUtils.setPropertyValue(element, poolBuilder, "keep-alive");
		ParsingUtils.setPropertyValue(element, poolBuilder, "load-conditioning-interval");
		ParsingUtils.setPropertyValue(element, poolBuilder, "max-connections");
		ParsingUtils.setPropertyValue(element, poolBuilder, "min-connections");
		ParsingUtils.setPropertyValue(element, poolBuilder, "multi-user-authentication");
		ParsingUtils.setPropertyValue(element, poolBuilder, "ping-interval");
		ParsingUtils.setPropertyValue(element, poolBuilder, "pr-single-hop-enabled");
		ParsingUtils.setPropertyValue(element, poolBuilder, "read-timeout");
		ParsingUtils.setPropertyValue(element, poolBuilder, "retry-attempts");
		ParsingUtils.setPropertyValue(element, poolBuilder, "server-group");
		ParsingUtils.setPropertyValue(element, poolBuilder, "socket-buffer-size");
		ParsingUtils.setPropertyValue(element, poolBuilder, "socket-connect-timeout");
		ParsingUtils.setPropertyValue(element, poolBuilder, "statistic-interval");
		ParsingUtils.setPropertyValue(element, poolBuilder, "subscription-ack-interval");
		ParsingUtils.setPropertyValue(element, poolBuilder, "subscription-enabled");
		ParsingUtils.setPropertyValue(element, poolBuilder, "subscription-message-tracking-timeout");
		ParsingUtils.setPropertyValue(element, poolBuilder, "subscription-redundancy");
		ParsingUtils.setPropertyValue(element, poolBuilder, "subscription-timeout-multiplier");
		ParsingUtils.setPropertyValue(element, poolBuilder, "thread-local-connections");

		List<Element> childElements = DomUtils.getChildElements(element);

		ManagedList<BeanDefinition> locators = new ManagedList<>(childElements.size());
		ManagedList<BeanDefinition> servers = new ManagedList<>(childElements.size());

		nullSafeList(childElements).forEach(childElement -> {

			String childElementName = childElement.getLocalName();

			if (LOCATOR_ELEMENT_NAME.equals(childElementName)) {
				locators.add(parseLocator(childElement));
			}

			if (SERVER_ELEMENT_NAME.equals(childElementName)) {
				servers.add(parseServer(childElement));
			}
		});

		BeanDefinitionRegistry registry = resolveRegistry(parserContext);

		boolean locatorsSet = parseLocators(element, poolBuilder, registry);
		boolean serversSet = parseServers(element, poolBuilder, registry);

		// If neither Locators nor Servers were explicitly configured, then setup a connection to a CacheServer
		// running on localhost, listening on the default CacheServer port, 40404
		if (childElements.isEmpty() && !(locatorsSet || serversSet)) {
			servers.add(buildConnection(DEFAULT_HOST, String.valueOf(DEFAULT_SERVER_PORT), true));
		}

		if (!locators.isEmpty()) {
			poolBuilder.addPropertyValue("locators", locators);
		}

		if (!servers.isEmpty()) {
			poolBuilder.addPropertyValue("servers", servers);
		}
	}

	BeanDefinitionRegistry resolveRegistry(ParserContext parserContext) {
		return parserContext.getRegistry();
	}

	BeanDefinition buildConnection(String host, String port, boolean server) {

		BeanDefinitionBuilder connectionEndpointBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(ConnectionEndpoint.class);

		connectionEndpointBuilder.addConstructorArgValue(defaultHost(host));
		connectionEndpointBuilder.addConstructorArgValue(defaultPort(port, server));

		return connectionEndpointBuilder.getBeanDefinition();
	}

	BeanDefinition buildConnections(String expression, boolean server) {

		BeanDefinitionBuilder connectionEndpointListBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(ConnectionEndpointList.class);

		connectionEndpointListBuilder.setFactoryMethod("parse");
		connectionEndpointListBuilder.addConstructorArgValue(defaultPort(null, server));
		connectionEndpointListBuilder.addConstructorArgValue(expression);

		return connectionEndpointListBuilder.getBeanDefinition();
	}

	String defaultHost(String host) {

		return (StringUtils.hasText(host) ? host : DEFAULT_HOST);
	}

	String defaultPort(String port, boolean server) {

		return (StringUtils.hasText(port) ? port
			: (server ? String.valueOf(DEFAULT_SERVER_PORT) : String.valueOf(DEFAULT_LOCATOR_PORT)));
	}

	BeanDefinition parseLocator(Element element) {

		return buildConnection(element.getAttribute(HOST_ATTRIBUTE_NAME),
			element.getAttribute(PORT_ATTRIBUTE_NAME), false);
	}

	boolean parseLocators(Element element, BeanDefinitionBuilder poolBuilder, BeanDefinitionRegistry registry) {

		String locatorsAttributeValue = element.getAttribute(LOCATORS_ATTRIBUTE_NAME);

		if (StringUtils.hasText(locatorsAttributeValue)) {

			BeanDefinitionBuilder addLocatorsMethodInvokingBeanBuilder =
				BeanDefinitionBuilder.genericBeanDefinition(MethodInvokingBean.class);

			addLocatorsMethodInvokingBeanBuilder.addPropertyReference("targetObject", resolveDereferencedId(element));
			addLocatorsMethodInvokingBeanBuilder.addPropertyValue("targetMethod", "addLocators");
			addLocatorsMethodInvokingBeanBuilder.addPropertyValue("arguments",
				buildConnections(locatorsAttributeValue, false));

			AbstractBeanDefinition addLocatorsMethodInvokingBean =
				addLocatorsMethodInvokingBeanBuilder.getBeanDefinition();

			poolBuilder.addPropertyReference("locatorsConfiguration",
				BeanDefinitionReaderUtils.registerWithGeneratedName(addLocatorsMethodInvokingBean, registry));

			return true;
		}

		return false;
	}

	BeanDefinition parseServer(Element element) {

		return buildConnection(element.getAttribute(HOST_ATTRIBUTE_NAME),
			element.getAttribute(PORT_ATTRIBUTE_NAME), true);
	}

	boolean parseServers(Element element, BeanDefinitionBuilder poolBuilder, BeanDefinitionRegistry registry) {

		String serversAttributeValue = element.getAttribute(SERVERS_ATTRIBUTE_NAME);

		if (StringUtils.hasText(serversAttributeValue)) {

			BeanDefinitionBuilder addServersMethodInvokingBeanBuilder =
				BeanDefinitionBuilder.genericBeanDefinition(MethodInvokingBean.class);

			addServersMethodInvokingBeanBuilder.addPropertyReference("targetObject", resolveDereferencedId(element));
			addServersMethodInvokingBeanBuilder.addPropertyValue("targetMethod", "addServers");
			addServersMethodInvokingBeanBuilder.addPropertyValue("arguments",
				buildConnections(serversAttributeValue, true));

			AbstractBeanDefinition addServersMethodInvokingBean =
				addServersMethodInvokingBeanBuilder.getBeanDefinition();

			poolBuilder.addPropertyReference("serversConfiguration",
				BeanDefinitionReaderUtils.registerWithGeneratedName(addServersMethodInvokingBean, registry));

			return true;
		}

		return false;
	}

	String resolveId(Element element) {

		return Optional.ofNullable(element.getAttribute(ID_ATTRIBUTE)).filter(StringUtils::hasText)
			.orElse(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);
	}

	String resolveDereferencedId(Element element) {
		return SpringUtils.dereferenceBean(resolveId(element));
	}

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
