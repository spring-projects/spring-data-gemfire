/*
 * Copyright 2010-2020 the original author or authors.
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

import org.w3c.dom.Element;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;

/**
 * Bean definition parser for the &lt;gfe:client-cache&gt; SDG XML namespace (XSD) element.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author Lyndon Adams
 * @author John Blum
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see CacheParser
 */
class ClientCacheParser extends CacheParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return ClientCacheFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder clientCacheBuilder) {

		super.doParse(element, parserContext, clientCacheBuilder);

		ParsingUtils.setPropertyValue(element, clientCacheBuilder, "durable-client-id");
		ParsingUtils.setPropertyValue(element, clientCacheBuilder, "durable-client-timeout");
		ParsingUtils.setPropertyValue(element, clientCacheBuilder, "keep-alive");
		ParsingUtils.setPropertyValue(element, clientCacheBuilder, "pool-name");
		ParsingUtils.setPropertyValue(element, clientCacheBuilder, "ready-for-events");
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void postProcessDynamicRegionSupport(Element element, BeanDefinitionBuilder dynamicRegionSupport) {
		ParsingUtils.setPropertyValue(element, dynamicRegionSupport, "pool-name");
	}
}
