/*
 * Copyright 2011-2020 the original author or authors.
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

import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.server.CacheServerFactoryBean;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;

/**
 * Bean definition parser for the &lt;gfe:cache-server&lt; SDG XML namespace (XSD) element.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser
 * @see org.springframework.data.gemfire.server.CacheServerFactoryBean
 * @since 1.1.0
 */
class CacheServerParser extends AbstractSimpleBeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return CacheServerFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected boolean isEligibleAttribute(Attr attribute, ParserContext parserContext) {
		return (super.isEligibleAttribute(attribute, parserContext) && !"groups".equals(attribute.getName())
			&& !"cache-ref".equals(attribute.getName()));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void postProcess(BeanDefinitionBuilder builder, Element element) {
		String cacheRefAttribute = element.getAttribute(ParsingUtils.CACHE_REF_ATTRIBUTE_NAME);

		builder.addPropertyReference("cache", SpringUtils.defaultIfEmpty(
			cacheRefAttribute, GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));

		String groupsAttribute = element.getAttribute("groups");

		if (StringUtils.hasText(groupsAttribute)) {
			builder.addPropertyValue("serverGroups", StringUtils.commaDelimitedListToStringArray(groupsAttribute));
		}

		parseSubscription(element, builder);
	}

	/* (non-Javadoc) */
	private void parseSubscription(Element element, BeanDefinitionBuilder builder) {
		Element subscriptionConfigElement = DomUtils.getChildElementByTagName(element, "subscription-config");

		if (subscriptionConfigElement != null) {
			ParsingUtils.setPropertyValue(subscriptionConfigElement, builder, "capacity", "subscriptionCapacity");
			ParsingUtils.setPropertyValue(subscriptionConfigElement, builder, "disk-store", "subscriptionDiskStore");

			String evictionTypeAttribute = subscriptionConfigElement.getAttribute("eviction-type");

			if (StringUtils.hasText(evictionTypeAttribute)) {
				builder.addPropertyValue("subscriptionEvictionPolicy", evictionTypeAttribute);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected String resolveId(Element element, AbstractBeanDefinition definition, ParserContext parserContext)
		throws BeanDefinitionStoreException {

		String name = super.resolveId(element, definition, parserContext);
		return (StringUtils.hasText(name) ? name : "gemfireServer");
	}
}
