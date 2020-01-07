/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.config.xml;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Element;

import org.springframework.beans.MutablePropertyValues;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.xml.AbstractBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;

/**
 * Bean definition parser for the &lt;gfe-data:datasource&gt; SDG XML namespace (XSD) element.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.w3c.dom.Element
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.xml.AbstractBeanDefinitionParser
 * @see org.springframework.beans.factory.xml.ParserContext
 * @see org.springframework.data.gemfire.client.GemfireDataSourcePostProcessor
 * @see org.springframework.data.gemfire.config.xml.ClientCacheParser
 * @see org.springframework.data.gemfire.config.xml.PoolParser
 */
class GemfireDataSourceParser extends AbstractBeanDefinitionParser {

	static final String SUBSCRIPTION_ENABLED_ATTRIBUTE_NAME = "subscription-enabled";
	static final String SUBSCRIPTION_ENABLED_PROPERTY_NAME = "subscriptionEnabled";

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected AbstractBeanDefinition parseInternal(Element element, ParserContext parserContext) {

		parseAndRegisterClientCache(element, parserContext);
		parseAndRegisterPool(element, parserContext);
		registerGemFireDataSourceBeanPostProcessor(parserContext);

		return null;
	}

	@SuppressWarnings("all")
	private void parseAndRegisterClientCache(Element element, ParserContext parserContext) {

		BeanDefinition clientCacheDefinition = new ClientCacheParser().parse(element, parserContext);

		parserContext.getRegistry()
			.registerBeanDefinition(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME, clientCacheDefinition);

		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Registered Pivotal GemFire ClientCache bean [%1$s] of type [%2$s]%n",
				GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME, clientCacheDefinition.getBeanClassName()));
		}
	}

	@SuppressWarnings("all")
	private void parseAndRegisterPool(Element element, ParserContext parserContext) {

		BeanDefinition poolDefinition = new PoolParser().parse(element, parserContext);

		MutablePropertyValues poolProperties = poolDefinition.getPropertyValues();

		if (!element.hasAttribute(SUBSCRIPTION_ENABLED_ATTRIBUTE_NAME)) {
			poolProperties.add(SUBSCRIPTION_ENABLED_PROPERTY_NAME, true);
		}

		parserContext.getRegistry().registerBeanDefinition(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME, poolDefinition);
	}

	private void registerGemFireDataSourceBeanPostProcessor(@NonNull ParserContext parserContext) {

		BeanFactory beanFactory = resolveBeanFactory(parserContext);

		if (beanFactory != null) {

			BeanDefinitionBuilder builder =
				BeanDefinitionBuilder.genericBeanDefinition(GemfireDataSourcePostProcessor.class);

			builder.addPropertyValue("beanFactory", beanFactory);

			BeanDefinitionReaderUtils.registerWithGeneratedName(builder.getBeanDefinition(), parserContext.getRegistry());
		}
	}

	private @Nullable BeanFactory resolveBeanFactory(@NonNull ParserContext parserContext) {

		BeanDefinitionRegistry registry = parserContext.getRegistry();

		return registry instanceof ConfigurableApplicationContext
			? ((ConfigurableApplicationContext) registry).getBeanFactory()
			: registry instanceof BeanFactory
			? (BeanFactory) registry
			: null;
	}
}
