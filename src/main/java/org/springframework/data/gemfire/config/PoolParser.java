/*
 * Copyright 2010 the original author or authors.
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

import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.client.PoolConnection;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Parser for &lt;pool;gt; definitions.
 *  
 * @author Costin Leau
 */
class PoolParser extends AbstractSimpleBeanDefinitionParser {

	protected Class<?> getBeanClass(Element element) {
		return PoolFactoryBean.class;
	}

	protected void postProcess(BeanDefinitionBuilder builder, Element element) {
		List<Element> subElements = DomUtils.getChildElements(element);
		ManagedList<Object> locators = new ManagedList<Object>(subElements.size());
		ManagedList<Object> servers = new ManagedList<Object>(subElements.size());

		// parse nested locator/server elements
		for (Element subElement : subElements) {
			String name = subElement.getLocalName();

			if ("locator".equals(name)) {
				locators.add(parseLocator(subElement));
			}
			if ("server".equals(name)) {
				servers.add(parseServer(subElement));
			}
		}

		if (!locators.isEmpty()) {
			builder.addPropertyValue("locators", locators);
		}

		if (!servers.isEmpty()) {
			builder.addPropertyValue("servers", servers);
		}
	}

	private Object parseServer(Element subElement) {
		return parseConnection(subElement);
	}

	private Object parseLocator(Element subElement) {
		return parseConnection(subElement);
	}

	private BeanDefinition parseConnection(Element element) {
		BeanDefinitionBuilder defBuilder = BeanDefinitionBuilder.genericBeanDefinition(PoolConnection.class);
		ParsingUtils.setPropertyValue(element, defBuilder, "host", "host");
		ParsingUtils.setPropertyValue(element, defBuilder, "port", "port");
		return defBuilder.getBeanDefinition();
	}

	@Override
	protected String resolveId(Element element, AbstractBeanDefinition definition, ParserContext parserContext)
			throws BeanDefinitionStoreException {
		String name = super.resolveId(element, definition, parserContext);
		if (!StringUtils.hasText(name)) {
			name = "gemfire-pool";
		}
		return name;
	}
}