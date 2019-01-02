/*
 * Copyright 2002-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.MutablePropertyValues;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.xml.AbstractBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor;
import org.w3c.dom.Element;

/**
 * Bean definition parser for the &lt;gfe-data:datasource&gt; SDG XML namespace (XSD) element.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.xml.AbstractBeanDefinitionParser
 * @see org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor
 * @see ClientCacheParser
 * @see PoolParser
 */
class GemfireDataSourceParser extends AbstractBeanDefinitionParser {

	static final String SUBSCRIPTION_ENABLED_ATTRIBUTE_NAME = "subscription-enabled";
	static final String SUBSCRIPTION_ENABLED_PROPERTY_NAME = "subscriptionEnabled";

	protected final Log log = LogFactory.getLog(getClass());

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected AbstractBeanDefinition parseInternal(Element element, ParserContext parserContext) {
		BeanDefinition clientCacheDefinition = new ClientCacheParser().parse(element, parserContext);

		parserContext.getRegistry().registerBeanDefinition(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME,
			clientCacheDefinition);

		BeanDefinition poolDefinition = new PoolParser().parse(element, parserContext);

		MutablePropertyValues poolProperties = poolDefinition.getPropertyValues();

		if (!element.hasAttribute(SUBSCRIPTION_ENABLED_ATTRIBUTE_NAME)) {
			poolProperties.add(SUBSCRIPTION_ENABLED_PROPERTY_NAME, true);
		}

		parserContext.getRegistry().registerBeanDefinition(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME, poolDefinition);

		if (log.isDebugEnabled()) {
			log.debug(String.format("Registered GemFire ClientCache bean [%1$s] of type [%2$s]%n",
				GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME, clientCacheDefinition.getBeanClassName()));
		}

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(
			GemfireDataSourcePostProcessor.class);

		builder.addConstructorArgReference(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

		BeanDefinitionReaderUtils.registerWithGeneratedName(builder.getBeanDefinition(), parserContext.getRegistry());

		return null;
	}
}
