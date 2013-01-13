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
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

/**
 * @author David Turanski
 * 
 */
class GatewayReceiverParser extends AbstractSimpleBeanDefinitionParser {
	@Override
	protected Class<?> getBeanClass(Element element) {
		return GatewayReceiverFactoryBean.class;
	}

	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		builder.setLazyInit(false);
		String cacheRef = element.getAttribute("cache-ref");
		// add cache reference (fallback to default if nothing is specified)
		builder.addConstructorArgReference((StringUtils.hasText(cacheRef) ? cacheRef : "gemfireCache"));
		ParsingUtils.setPropertyValue(element, builder, "start-port");
		ParsingUtils.setPropertyValue(element, builder, "end-port");
		ParsingUtils.setPropertyValue(element, builder, "socket-buffer-size");
		ParsingUtils.setPropertyValue(element, builder, "maximum-time-between-pings");
		ParsingUtils.setPropertyValue(element, builder, "bind-address");
		ParsingUtils.parseTransportFilters(element, parserContext, builder);
	}
}
