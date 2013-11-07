/*
 * Copyright 2011-2013 the original author or authors.
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

import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.server.CacheServerFactoryBean;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

/**
 * Namespace parser for "cache-server" element.
 * <p/>
 * @author Costin Leau
 * @author John Blum
 */
class CacheServerParser extends AbstractSimpleBeanDefinitionParser {

	@Override
	protected Class<?> getBeanClass(Element element) {
		return CacheServerFactoryBean.class;
	}

	@Override
	protected String resolveId(Element element, AbstractBeanDefinition definition, ParserContext parserContext)
		throws BeanDefinitionStoreException {
		String name = super.resolveId(element, definition, parserContext);
		if (!StringUtils.hasText(name)) {
			name = "gemfireServer";
		}
		return name;
	}

	@Override
	protected boolean isEligibleAttribute(Attr attribute, ParserContext parserContext) {
		return super.isEligibleAttribute(attribute, parserContext) && !"groups".equals(attribute.getName())
			&& !"cache-ref".equals(attribute.getName());
	}

	@Override
	protected void postProcess(BeanDefinitionBuilder builder, Element element) {
		String cacheRefAttribute = element.getAttribute("cache-ref");

		builder.addPropertyReference("cache", (StringUtils.hasText(cacheRefAttribute) ? cacheRefAttribute
			: GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));

		String groupsAttribute = element.getAttribute("groups");

		if (StringUtils.hasText(groupsAttribute)) {
			builder.addPropertyValue("serverGroups", StringUtils.commaDelimitedListToStringArray(groupsAttribute));
		}

		parseSubscription(builder, element);
	}

	private void parseSubscription(BeanDefinitionBuilder builder, Element element) {
		Element subscriptionConfigElement = DomUtils.getChildElementByTagName(element, "subscription-config");

		if (subscriptionConfigElement != null) {
			ParsingUtils.setPropertyValue(subscriptionConfigElement, builder, "capacity", "subscriptionCapacity");
			ParsingUtils.setPropertyValue(subscriptionConfigElement, builder, "disk-store", "subscriptionDiskStore");

			String evictionTypeAttribute = subscriptionConfigElement.getAttribute("eviction-type");

			if (StringUtils.hasText(evictionTypeAttribute)) {
				builder.addPropertyValue("subscriptionEvictionPolicy", evictionTypeAttribute.toUpperCase());
			}
		}
	}

}
