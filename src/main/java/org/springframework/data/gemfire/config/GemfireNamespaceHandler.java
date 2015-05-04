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

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.xml.NamespaceHandlerSupport;
import org.springframework.beans.factory.xml.ParserContext;
import org.w3c.dom.Element;

/**
 * Spring NamespaceHandler for GemFire XML namespace (XSD) bean definitions.
 * 
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.xml.NamespaceHandlerSupport
 */
@SuppressWarnings("unused")
class GemfireNamespaceHandler extends NamespaceHandlerSupport {

	@Override
	public BeanDefinition parse(Element element, ParserContext parserContext) {
		ParsingUtils.assertGemFireFeatureAvailable(element, parserContext);
		return super.parse(element, parserContext);
	}

	@Override
	public void init() {
		registerBeanDefinitionParser("annotation-driven", new AnnotationDrivenBeanDefinitionParser());
		registerBeanDefinitionParser("cache", new CacheParser());
		registerBeanDefinitionParser("cache-server", new CacheServerParser());
		registerBeanDefinitionParser("auto-region-lookup", new AutoRegionLookupBeanDefinitionParser());
		registerBeanDefinitionParser("lookup-region", new LookupRegionParser());
		registerBeanDefinitionParser("region-template", new TemplateRegionParser());
		registerBeanDefinitionParser("local-region", new LocalRegionParser());
		registerBeanDefinitionParser("local-region-template", new LocalRegionParser());
		registerBeanDefinitionParser("partitioned-region", new PartitionedRegionParser());
		registerBeanDefinitionParser("partitioned-region-template", new PartitionedRegionParser());
		registerBeanDefinitionParser("replicated-region", new ReplicatedRegionParser());
		registerBeanDefinitionParser("replicated-region-template", new ReplicatedRegionParser());
		registerBeanDefinitionParser("async-event-queue", new AsyncEventQueueParser());
		registerBeanDefinitionParser("disk-store", new DiskStoreParser());
		registerBeanDefinitionParser("function-service", new FunctionServiceParser());
		registerBeanDefinitionParser("gateway-receiver", new GatewayReceiverParser());
		registerBeanDefinitionParser("gateway-sender", new GatewaySenderParser());
		registerBeanDefinitionParser("index", new IndexParser());
		registerBeanDefinitionParser("transaction-manager", new TransactionManagerParser());
		registerBeanDefinitionParser("client-cache", new ClientCacheParser());
		registerBeanDefinitionParser("client-region", new ClientRegionParser());
		registerBeanDefinitionParser("client-region-template", new ClientRegionParser());
		registerBeanDefinitionParser("cq-listener-container", new GemfireListenerContainerParser());
		registerBeanDefinitionParser("pool", new PoolParser());
	}

}
