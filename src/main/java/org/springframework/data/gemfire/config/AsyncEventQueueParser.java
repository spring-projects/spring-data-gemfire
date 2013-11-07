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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.wan.AsyncEventQueueFactoryBean;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * @author David Turanski
 * @author John Blum
 */
public class AsyncEventQueueParser extends AbstractSingleBeanDefinitionParser {

	@Override
	protected Class<?> getBeanClass(Element element) {
		return AsyncEventQueueFactoryBean.class;
	}

	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		builder.setLazyInit(false);

		Element asyncEventListenerElement = DomUtils.getChildElementByTagName(element, "async-event-listener");

		Object asyncEventListener = ParsingUtils.parseRefOrSingleNestedBeanDeclaration(parserContext,
			asyncEventListenerElement, builder);

		builder.addPropertyValue("asyncEventListener", asyncEventListener);

		String cacheRefAttribute = element.getAttribute("cache-ref");

		String cacheName = (StringUtils.hasText(cacheRefAttribute) ? cacheRefAttribute
			: GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

		builder.addConstructorArgReference(cacheName);

		ParsingUtils.setPropertyValue(element, builder, "batch-size");
		ParsingUtils.setPropertyValue(element, builder, "maximum-queue-memory");
		ParsingUtils.setPropertyValue(element, builder, "persistent");
		ParsingUtils.setPropertyValue(element, builder, "parallel");

		parseDiskStore(element, builder);

		if (GemfireUtils.GEMFIRE_VERSION.compareTo("7.0.1") >= 0) {
			ParsingUtils.setPropertyValue(element, builder, "batch-conflation-enabled");
			ParsingUtils.setPropertyValue(element, builder, "disk-synchronous");
			ParsingUtils.setPropertyValue(element, builder, "batch-time-interval");
			ParsingUtils.setPropertyValue(element, builder, "disk-synchronous");
			ParsingUtils.setPropertyValue(element, builder, "dispatcher-threads");
			ParsingUtils.setPropertyValue(element, builder, "order-policy");
		}

		ParsingUtils.setPropertyValue(element, builder, NAME_ATTRIBUTE);

		if (!StringUtils.hasText(element.getAttribute(NAME_ATTRIBUTE))) {
			if (element.getParentNode().getNodeName().endsWith("region")) {
				Element region = (Element) element.getParentNode();
				String regionName = StringUtils.hasText(region.getAttribute("name")) ? region.getAttribute("name")
						: region.getAttribute("id");

				int i = 0;
				String name = regionName + ".asyncEventQueue#" + i;
				while (parserContext.getRegistry().isBeanNameInUse(name)) {
					i++;
					name = regionName + ".asyncEventQueue#" + i;
				}

				builder.addPropertyValue("name", name);
			}
		}
	}

	private void parseDiskStore(final Element element, final BeanDefinitionBuilder builder) {
		ParsingUtils.setPropertyValue(element, builder, "disk-store-ref");

		String diskStoreRef = element.getAttribute("disk-store-ref");

		if (StringUtils.hasText(diskStoreRef)) {
			builder.addDependsOn(diskStoreRef);
		}
	}

}
