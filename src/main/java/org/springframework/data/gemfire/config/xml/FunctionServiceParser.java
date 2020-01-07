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

import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.function.FunctionServiceFactoryBean;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Bean definition parser for the &lt;gfe:function-service&gt; SDG XML namespace (XSD) element.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser
 * @see FunctionServiceFactoryBean
 */
class FunctionServiceParser extends AbstractSimpleBeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return FunctionServiceFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		super.doParse(element, builder);

		builder.setLazyInit(false);

		Element functionElement = DomUtils.getChildElementByTagName(element, "function");

		if (functionElement != null) {
			builder.addPropertyValue("functions", ParsingUtils.parseRefOrNestedBeanDeclaration(
				functionElement, parserContext, builder));
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected String resolveId(Element element, AbstractBeanDefinition beanDefinition, ParserContext parserContext)
			throws BeanDefinitionStoreException {

		String resolvedId = super.resolveId(element, beanDefinition, parserContext);

		return SpringUtils.defaultIfEmpty(resolvedId, GemfireConstants.DEFAULT_GEMFIRE_FUNCTION_SERVICE_NAME);
	}
}
