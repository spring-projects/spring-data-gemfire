/*
 * Copyright 2010-2012the original author or authors.
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
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.transaction.GemfireTransactionManager;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

/**
 * Bean definition parser for the &lt;gfe:transaction-manager&gt; SDG XML namespace (XSD) element.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser
 * @see GemfireTransactionManager
 */
class TransactionManagerParser extends AbstractSingleBeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return GemfireTransactionManager.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void doParse(Element element, BeanDefinitionBuilder builder) {
		super.doParse(element, builder);

		ParsingUtils.setPropertyValue(element, builder, "copy-on-read", "copyOnRead");
		builder.addPropertyReference("cache", ParsingUtils.resolveCacheReference(element));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected String resolveId(Element element, AbstractBeanDefinition definition, ParserContext parserContext)
			throws BeanDefinitionStoreException {

		String name = super.resolveId(element, definition, parserContext);

		if (!StringUtils.hasText(name)) {
			name = GemfireConstants.DEFAULT_GEMFIRE_TRANSACTION_MANAGER_NAME;
			//For backward compatibility
			parserContext.getRegistry().registerAlias(GemfireConstants.DEFAULT_GEMFIRE_TRANSACTION_MANAGER_NAME,
				"gemfire-transaction-manager");
		}

		return name;
	}
}
