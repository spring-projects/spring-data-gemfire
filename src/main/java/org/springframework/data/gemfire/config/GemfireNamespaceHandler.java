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

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.xml.NamespaceHandlerSupport;
import org.springframework.beans.factory.xml.ParserContext;
import org.w3c.dom.Element;

/**
 * Namespace handler for GemFire definitions.
 * 
 * @author Costin Leau
 * @author David Turanski
 */
class GemfireNamespaceHandler extends NamespaceHandlerSupport {
	static final List<String> GEMFIRE7_ELEMENTS = Arrays.asList("async-event-queue", "gateway-sender",
			"gateway-receiver");

	@Override
	public BeanDefinition parse(Element element, ParserContext parserContext) {

		boolean v7ElementsPresent = GEMFIRE7_ELEMENTS.contains(element.getLocalName());

		if (v7ElementsPresent) {
			ParsingUtils.throwExceptionIfNotGemfireV7(element.getLocalName(), null, parserContext);
		}

		return super.parse(element, parserContext);
	}

	@Override
	public void init() {
		registerBeanDefinitionParser("cache", new CacheParser());
		registerBeanDefinitionParser("client-cache", new ClientCacheParser());
		registerBeanDefinitionParser("lookup-region", new LookupRegionParser());
		registerBeanDefinitionParser("replicated-region", new ReplicatedRegionParser());
		registerBeanDefinitionParser("partitioned-region", new PartitionedRegionParser());
		registerBeanDefinitionParser("local-region", new LocalRegionParser());
		registerBeanDefinitionParser("client-region", new ClientRegionParser());
		registerBeanDefinitionParser("pool", new PoolParser());
		registerBeanDefinitionParser("index", new IndexParser());
		registerBeanDefinitionParser("disk-store", new DiskStoreParser());
		registerBeanDefinitionParser("cache-server", new CacheServerParser());
		registerBeanDefinitionParser("transaction-manager", new TransactionManagerParser());
		registerBeanDefinitionParser("cq-listener-container", new GemfireListenerContainerParser());
		registerBeanDefinitionParser("async-event-queue", new AsyncEventQueueParser());
		registerBeanDefinitionParser("gateway-sender", new GatewaySenderParser());
		registerBeanDefinitionParser("gateway-receiver", new GatewayReceiverParser());
		registerBeanDefinitionParser("function-service", new FunctionServiceParser());
		// V6 WAN parsers
		registerBeanDefinitionParser("gateway-hub", new GatewayHubParser());
	}
}
