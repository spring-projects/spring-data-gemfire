/*
 * Copyright 2018 the original author or authors.
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

import org.apache.geode.cache.Region;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewaySender;

import org.w3c.dom.Element;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;

/**
 * Abstract Spring XML Parser for peer {@link Region} bean definitions.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.xml.ParserContext
 * @see org.w3c.dom.Element
 * @since 2.2.0
 */
public abstract class AbstractPeerRegionParser extends AbstractRegionParser {

	@Override
	protected void doParseRegionConfiguration(Element element, ParserContext parserContext,
			BeanDefinitionBuilder regionBuilder, BeanDefinitionBuilder regionAttributesBuilder, boolean subRegion) {

		super.doParseRegionConfiguration(element, parserContext, regionBuilder, regionAttributesBuilder, subRegion);

		ParsingUtils.setPropertyValue(element, regionBuilder, "async-event-queue-ids");
		ParsingUtils.setPropertyValue(element, regionBuilder, "gateway-sender-ids");

		parseCollectionOfCustomSubElements(element, parserContext, regionBuilder, AsyncEventQueue.class.getName(),
			"async-event-queue", "asyncEventQueues");

		parseCollectionOfCustomSubElements(element, parserContext, regionBuilder, GatewaySender.class.getName(),
			"gateway-sender", "gatewaySenders");
	}
}
