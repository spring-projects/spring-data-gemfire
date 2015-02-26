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

import java.util.List;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.wan.GatewayHubFactoryBean;
import org.springframework.data.gemfire.wan.GatewayProxy;
import org.springframework.util.CollectionUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Parser for the &lt;gateway-hub&gt; SDG XML namespace element used to create GemFire GatewayHubs.
 *
 * @author David Turanski
 * @author John J. Blum
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.ManagedList
 * @see org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser
 * @see org.springframework.beans.factory.xml.ParserContext
 * @see org.springframework.data.gemfire.wan.GatewayHubFactoryBean
 * @see org.springframework.data.gemfire.wan.GatewayProxy
 */
class GatewayHubParser extends AbstractSimpleBeanDefinitionParser {

	@Override
	protected Class<?> getBeanClass(Element element) {
		return GatewayHubFactoryBean.class;
	}

	@Override
	@SuppressWarnings({ "rawtypes", "unchecked" })
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		builder.addConstructorArgReference(ParsingUtils.resolveCacheReference(element.getAttribute("cache-ref")));
		builder.setLazyInit(false);

		ParsingUtils.setPropertyValue(element, builder, "bind-address");
		ParsingUtils.setPropertyValue(element, builder, "manual-start");
		ParsingUtils.setPropertyValue(element, builder, "max-time-between-pings", "maximumTimeBetweenPings");
		ParsingUtils.setPropertyValue(element, builder, "socket-buffer-size");
		ParsingUtils.setPropertyValue(element, builder, "startup-policy");
		ParsingUtils.setPropertyValue(element, builder, "port");

		parseGateways(element, parserContext, builder);
	}

	private void parseGateways(Element element, ParserContext parserContext, BeanDefinitionBuilder gatewayHubBuilder) {
		List<Element> gatewayElements = DomUtils.getChildElementsByTagName(element, "gateway");

		if (!CollectionUtils.isEmpty(gatewayElements)) {
			ManagedList<BeanDefinition> gateways = new ManagedList<BeanDefinition>();

			for (Element gatewayElement : gatewayElements) {
				BeanDefinitionBuilder gatewayBuilder = BeanDefinitionBuilder.genericBeanDefinition(GatewayProxy.class);

				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "gateway-id", "id");
				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "concurrency-level");
				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "order-policy");
				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "socket-buffer-size");
				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "socket-read-timeout");

				parseGatewayEndpoints(gatewayElement, gatewayBuilder);
				parseGatewayListener(gatewayElement, parserContext, gatewayBuilder);

				parseGatewayQueue(gatewayElement, gatewayBuilder);

				gateways.add(gatewayBuilder.getBeanDefinition());
			}

			gatewayHubBuilder.addPropertyValue("gateways", gateways);
		}
	}

	private void parseGatewayEndpoints(Element gatewayElement, BeanDefinitionBuilder gatewayBuilder) {
		List<Element> endpointElements = DomUtils.getChildElementsByTagName(gatewayElement, "gateway-endpoint");

		if (!CollectionUtils.isEmpty(endpointElements)) {
			ManagedList<BeanDefinition> endpoints = new ManagedList<BeanDefinition>();

			for (Element endpointElement : endpointElements) {
				BeanDefinitionBuilder endpointBuilder = BeanDefinitionBuilder.genericBeanDefinition(
					GatewayProxy.GatewayEndpoint.class);

				ParsingUtils.setPropertyValue(endpointElement, endpointBuilder, "endpoint-id", "id");
				ParsingUtils.setPropertyValue(endpointElement, endpointBuilder, "host");
				ParsingUtils.setPropertyValue(endpointElement, endpointBuilder, "port");

				endpoints.add(endpointBuilder.getBeanDefinition());
			}

			gatewayBuilder.addPropertyValue("endpoints", endpoints);
		}
	}

	private void parseGatewayListener(Element gatewayElement, ParserContext parserContext,
			BeanDefinitionBuilder gatewayBuilder) {

		Element gatewayListenerElement = DomUtils.getChildElementByTagName(gatewayElement, "gateway-listener");

		if (gatewayListenerElement != null) {
			gatewayBuilder.addPropertyValue("listeners", ParsingUtils.parseRefOrNestedBeanDeclaration(
				parserContext, gatewayListenerElement, gatewayBuilder));
		}
	}

	private void parseGatewayQueue(Element gatewayElement, BeanDefinitionBuilder gatewayBuilder) {
		Element queueElement = DomUtils.getChildElementByTagName(gatewayElement, "gateway-queue");

		if (queueElement != null) {
			BeanDefinitionBuilder queueBuilder = BeanDefinitionBuilder.genericBeanDefinition(
				GatewayProxy.GatewayQueue.class);

			ParsingUtils.setPropertyValue(queueElement, queueBuilder, "alert-threshold");
			ParsingUtils.setPropertyValue(queueElement, queueBuilder, "batch-size");
			ParsingUtils.setPropertyValue(queueElement, queueBuilder, "batch-time-interval");
			ParsingUtils.setPropertyValue(queueElement, queueBuilder, "disk-store-ref");
			ParsingUtils.setPropertyValue(queueElement, queueBuilder, "enable-batch-conflation");
			ParsingUtils.setPropertyValue(queueElement, queueBuilder, "maximum-queue-memory");
			ParsingUtils.setPropertyValue(queueElement, queueBuilder, "persistent");

			if (queueElement.hasAttribute("disk-store-ref")) {
				gatewayBuilder.getBeanDefinition().setDependsOn(new String[] {
					queueElement.getAttribute("disk-store-ref") });
			}

			gatewayBuilder.addPropertyValue("queue", queueBuilder.getBeanDefinition());
		}
	}

}
