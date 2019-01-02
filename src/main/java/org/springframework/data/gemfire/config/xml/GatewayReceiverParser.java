/*
 * Copyright 2010-2019 the original author or authors.
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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean;
import org.w3c.dom.Element;

/**
 * Bean definition parser for the &lt;gfe:gateway-receiver&gt; SDG XML namespace (XSD) element.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser
 * @see org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean
 */
class GatewayReceiverParser extends AbstractSimpleBeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return GatewayReceiverFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		String cacheRef = element.getAttribute(ParsingUtils.CACHE_REF_ATTRIBUTE_NAME);

		builder.addConstructorArgReference(SpringUtils.defaultIfEmpty(
			cacheRef, GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));

		builder.setLazyInit(false);

		ParsingUtils.setPropertyValue(element, builder, "bind-address");
		ParsingUtils.setPropertyValue(element, builder, "hostname-for-senders");
		ParsingUtils.setPropertyValue(element, builder, "start-port");
		ParsingUtils.setPropertyValue(element, builder, "end-port");
		ParsingUtils.setPropertyValue(element, builder, "manual-start");
		ParsingUtils.setPropertyValue(element, builder, "maximum-time-between-pings");
		ParsingUtils.setPropertyValue(element, builder, "socket-buffer-size");
		ParsingUtils.parseTransportFilters(element, parserContext, builder);
	}
}
