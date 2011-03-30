/*
 * Copyright 2010-2011 the original author or authors.
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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Scope;

/**
 * Parser for &lt;replicated-region;gt; definitions.
 * 
 * @author Costin Leau
 */
class ReplicatedRegionParser extends AliasReplacingBeanDefinitionParser {

	protected Class<?> getBeanClass(Element element) {
		return RegionFactoryBean.class;
	}

	@Override
	protected void doParseInternal(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		super.doParse(element, builder);

		// set the data policy
		String attr = element.getAttribute("persistent");
		if (Boolean.parseBoolean(attr)) {
			builder.addPropertyValue("dataPolicy", DataPolicy.PERSISTENT_REPLICATE);
		}
		else {
			builder.addPropertyValue("dataPolicy", DataPolicy.REPLICATE);
		}

		builder.addPropertyValue("scope", Scope.DISTRIBUTED_ACK);

		ParsingUtils.setPropertyValue(element, builder, "name", "name");

		attr = element.getAttribute("cache-ref");
		// add cache reference (fallback to default if nothing is specified)
		builder.addPropertyReference("cache", (StringUtils.hasText(attr) ? attr : "gemfire-cache"));

		// add attributes
		BeanDefinitionBuilder attrBuilder = BeanDefinitionBuilder.genericBeanDefinition(RegionAttributesFactoryBean.class);

		attr = element.getAttribute("publisher");
		if (StringUtils.hasText(attr)) {
			attrBuilder.addPropertyValue("publisher", Boolean.valueOf(attr));
		}

		ParsingUtils.parseEviction(parserContext, element, attrBuilder);
		ParsingUtils.parseDiskStorage(element, attrBuilder);

		builder.addPropertyValue("attributes", attrBuilder.getBeanDefinition());

		List<Element> subElements = DomUtils.getChildElements(element);

		// parse nested cache-listener elements
		for (Element subElement : subElements) {
			String name = subElement.getLocalName();

			if ("cache-listener".equals(name)) {
				builder.addPropertyValue("cacheListeners", parseCacheListener(parserContext, subElement, builder));
			}

			else if ("cache-loader".equals(name)) {
				builder.addPropertyValue("cacheLoader", parseCacheLoader(parserContext, subElement, builder));
			}

			else if ("cache-writer".equals(name)) {
				builder.addPropertyValue("cacheWriter", parseCacheWriter(parserContext, subElement, builder));
			}
		}
	}

	private Object parseCacheListener(ParserContext parserContext, Element subElement, BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder);
	}

	private Object parseCacheLoader(ParserContext parserContext, Element subElement, BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder);
	}

	private Object parseCacheWriter(ParserContext parserContext, Element subElement, BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder);
	}
}