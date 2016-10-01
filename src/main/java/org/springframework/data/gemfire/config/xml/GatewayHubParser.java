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

package org.springframework.data.gemfire.config.xml;

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
 * Bean definition parser for the &lt;gfe:gateway-hub&gt; SDG XML namespace (XSD) element.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser
 * @see org.springframework.data.gemfire.wan.GatewayHubFactoryBean
 * @see org.springframework.data.gemfire.wan.GatewayProxy
 */
class GatewayHubParser extends AbstractSimpleBeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return GatewayHubFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings({ "rawtypes", "unchecked" })
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		builder.addConstructorArgReference(ParsingUtils.resolveCacheReference(element.getAttribute("cache-ref")));
		builder.setLazyInit(false);

		ParsingUtils.setPropertyValue(element, builder, "bind-address");
		ParsingUtils.setPropertyValue(element, builder, "manual-start");
		ParsingUtils.setPropertyValue(element, builder, "max-connections");
		ParsingUtils.setPropertyValue(element, builder, "max-time-between-pings", "maximumTimeBetweenPings");
		ParsingUtils.setPropertyValue(element, builder, "port");
		ParsingUtils.setPropertyValue(element, builder, "socket-buffer-size");
		ParsingUtils.setPropertyValue(element, builder, "startup-policy");

		parseGateways(element, parserContext, builder);
	}

	/* (non-Javadoc) */
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
				parseGatewayListeners(gatewayElement, parserContext, gatewayBuilder);
				parseGatewayQueue(gatewayElement, gatewayBuilder);

				gateways.add(gatewayBuilder.getBeanDefinition());
			}

			gatewayHubBuilder.addPropertyValue("gateways", gateways);
		}
	}

	/* (non-Javadoc) */
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

	/* (non-Javadoc) */
	private void parseGatewayListeners(Element gatewayElement, ParserContext parserContext,
			BeanDefinitionBuilder gatewayBuilder) {

		Element gatewayListenerElement = DomUtils.getChildElementByTagName(gatewayElement, "gateway-listener");

		if (gatewayListenerElement != null) {
			gatewayBuilder.addPropertyValue("listeners", ParsingUtils.parseRefOrNestedBeanDeclaration(
				parserContext, gatewayListenerElement, gatewayBuilder));
		}
	}

	/* (non-Javadoc) */
	private void parseGatewayQueue(Element gatewayElement, BeanDefinitionBuilder gatewayBuilder) {
		Element gatewayQueueElement = DomUtils.getChildElementByTagName(gatewayElement, "gateway-queue");

		if (gatewayQueueElement != null) {
			BeanDefinitionBuilder queueBuilder = BeanDefinitionBuilder.genericBeanDefinition(
				GatewayProxy.GatewayQueue.class);

			ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "alert-threshold");
			ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "batch-size");
			ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "batch-time-interval");
			ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "disk-store-ref");
			ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "enable-batch-conflation");
			ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "maximum-queue-memory");
			ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "persistent");

			/* Make sure any disk store is created first */
			if (gatewayQueueElement.hasAttribute("disk-store-ref")) {
				gatewayBuilder.addDependsOn(gatewayQueueElement.getAttribute("disk-store-ref"));
			}

			gatewayBuilder.addPropertyValue("queue", queueBuilder.getBeanDefinition());
		}
	}
}
