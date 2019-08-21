/*
 * Copyright 2010-2019 the original author or authors.
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

import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewaySender;

import org.w3c.dom.Element;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.LookupRegionFactoryBean;
import org.springframework.util.xml.DomUtils;

/**
 * Bean definition parser for the &lt;gfe:lookup-region&gt; SDG XML namespace (XSD) element.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.xml.ParserContext
 * @see org.springframework.data.gemfire.LookupRegionFactoryBean
 * @see org.springframework.data.gemfire.config.xml.AbstractRegionParser
 * @see org.w3c.dom.Element
 */
class LookupRegionParser extends AbstractRegionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getRegionFactoryClass() {
		return LookupRegionFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion) {

		super.doParse(element, builder);

		String resolvedCacheRef =
			ParsingUtils.resolveCacheReference(element.getAttribute(ParsingUtils.CACHE_REF_ATTRIBUTE_NAME));

		builder.addPropertyReference("cache", resolvedCacheRef);

		ParsingUtils.setPropertyValue(element, builder, "async-event-queue-ids");
		ParsingUtils.setPropertyValue(element, builder, "cloning-enabled");
		ParsingUtils.setPropertyValue(element, builder, "eviction-maximum");
		ParsingUtils.setPropertyValue(element, builder, "gateway-sender-ids");
		ParsingUtils.setPropertyValue(element, builder, "name");

		ParsingUtils.parseExpiration(element, parserContext, builder);

		parseCollectionOfCustomSubElements(element, parserContext, builder, AsyncEventQueue.class.getName(),
			"async-event-queue", "asyncEventQueues");

		parseCollectionOfCustomSubElements(element, parserContext, builder, GatewaySender.class.getName(),
			"gateway-sender", "gatewaySenders");

		Element cacheListenerElement = DomUtils.getChildElementByTagName(element, "cache-listener");

		if (cacheListenerElement != null) {
			builder.addPropertyValue("cacheListeners",
				ParsingUtils.parseRefOrNestedBeanDeclaration(cacheListenerElement, parserContext, builder));
		}

		Element cacheLoaderElement = DomUtils.getChildElementByTagName(element, "cache-loader");

		if (cacheLoaderElement != null) {
			builder.addPropertyValue("cacheLoader",
				ParsingUtils.parseRefOrSingleNestedBeanDeclaration(cacheLoaderElement, parserContext, builder));
		}

		Element cacheWriterElement = DomUtils.getChildElementByTagName(element, "cache-writer");

		if (cacheWriterElement != null) {
			builder.addPropertyValue("cacheWriter",
				ParsingUtils.parseRefOrSingleNestedBeanDeclaration(cacheWriterElement, parserContext, builder));
		}

		if (!subRegion) {
			parseSubRegions(element, parserContext, resolvedCacheRef);
		}
	}
}
