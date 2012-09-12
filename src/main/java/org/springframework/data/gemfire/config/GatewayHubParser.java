/*
 * Copyright 2010-2012 the original author or authors.
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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.wan.GatewayHubFactoryBean;
import org.springframework.data.gemfire.wan.GatewayProxy;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * @author David Turanski
 * 
 */
class GatewayHubParser extends AbstractSimpleBeanDefinitionParser {
	@Override
	protected Class<?> getBeanClass(Element element) {
		return GatewayHubFactoryBean.class;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		builder.setLazyInit(false);
		String cacheRef = element.getAttribute("cache-ref");
		// add cache reference (fallback to default if nothing is specified)
		builder.addConstructorArgReference((StringUtils.hasText(cacheRef) ? cacheRef : "gemfireCache"));
		ParsingUtils.setPropertyValue(element, builder, "bind-address");
		ParsingUtils.setPropertyValue(element, builder, "manual-start");
		ParsingUtils.setPropertyValue(element, builder, "socket-buffer-size");
		ParsingUtils.setPropertyValue(element, builder, "startup-policy");
		ParsingUtils.setPropertyValue(element, builder, "port");
		
		List<Element> gatewayElements = DomUtils.getChildElementsByTagName(element, "gateway");
		if (!CollectionUtils.isEmpty(gatewayElements)) {
			ManagedList gateways = new ManagedList();
			for (Element gatewayElement : gatewayElements) {
				BeanDefinitionBuilder gatewayBuilder = BeanDefinitionBuilder.genericBeanDefinition(GatewayProxy.class);
				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "gateway-id", "id");
				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "concurrency-level");
				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "socket-read-timeout");
				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "socket-buffer-size");
				ParsingUtils.setPropertyValue(gatewayElement, gatewayBuilder, "order-policy");
				List<Element> endpointElements = DomUtils.getChildElementsByTagName(gatewayElement, "gateway-endpoint");
				if (!CollectionUtils.isEmpty(endpointElements)) {
					ManagedList endpoints = new ManagedList();
					for (Element endpointElement : endpointElements) {
						BeanDefinitionBuilder endpointBuilder = BeanDefinitionBuilder
								.genericBeanDefinition(GatewayProxy.GatewayEndpoint.class);
						ParsingUtils.setPropertyValue(endpointElement, endpointBuilder, "host");
						ParsingUtils.setPropertyValue(endpointElement, endpointBuilder, "port");
						ParsingUtils.setPropertyValue(endpointElement, endpointBuilder, "endpoint-id", "id");
						endpoints.add(endpointBuilder.getBeanDefinition());
					}
					gatewayBuilder.addPropertyValue("endpoints", endpoints);
				}

				Element gatewayListenerElement = DomUtils.getChildElementByTagName(gatewayElement, "gateway-listener");
				if (gatewayListenerElement != null) {
					Object obj = ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, gatewayListenerElement,
							gatewayBuilder);
					gatewayBuilder.addPropertyValue("listeners", obj);
				}

				Element gatewayQueueElement = DomUtils.getChildElementByTagName(gatewayElement, "gateway-queue");
				if (gatewayQueueElement != null) {
					BeanDefinitionBuilder queueBuilder = BeanDefinitionBuilder
							.genericBeanDefinition(GatewayProxy.GatewayQueue.class);
					ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "alert-threshold");
					ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "batch-size");
					ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "batch-time-interval");
					ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "maximum-queue-memory");
					ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "persistent");
					ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "disk-store-ref");
					/*
					 * Make sure any disk store is created first
					 */
					if (gatewayQueueElement.hasAttribute("disk-store-ref")) {
						gatewayBuilder.getBeanDefinition().setDependsOn(
								new String[] {gatewayQueueElement.getAttribute("disk-store-ref")});
					}
					ParsingUtils.setPropertyValue(gatewayQueueElement, queueBuilder, "enable-batch-conflation");
					gatewayBuilder.addPropertyValue("queue", queueBuilder.getBeanDefinition());
					
					
				}
				gateways.add(gatewayBuilder.getBeanDefinition());
			}
			builder.addPropertyValue("gateways", gateways);
		}
	}
}
