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

import java.util.List;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.Interest;
import org.springframework.data.gemfire.client.RegexInterest;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Parser for &lt;client-region;gt; bean definitions.
 * <p/>
 * To avoid eager evaluations, the region interests are declared as nested definition.
 * <p/>
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
class ClientRegionParser extends AbstractRegionParser {

	@Override
	protected Class<?> getRegionFactoryClass() {
		return ClientRegionFactoryBean.class;
	}

	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		super.doParse(element, builder);

		validateDataPolicyShortcutMutualExclusion(element, parserContext);

		String cacheRefAttributeValue = element.getAttribute("cache-ref");

		builder.addPropertyReference("cache", (StringUtils.hasText(cacheRefAttributeValue) ? cacheRefAttributeValue
			: GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));

		ParsingUtils.setPropertyValue(element, builder, "close");
		ParsingUtils.setPropertyValue(element, builder, "destroy");
		ParsingUtils.setPropertyValue(element, builder, "data-policy", "dataPolicyName");
		ParsingUtils.setPropertyValue(element, builder, "name");
		ParsingUtils.setPropertyValue(element, builder, "persistent");
		ParsingUtils.setPropertyValue(element, builder, "pool-name");
		ParsingUtils.setPropertyValue(element, builder, "shortcut");

		parseDiskStoreAttribute(element, builder);

		// Client RegionAttributes for overflow/eviction, expiration and statistics
		BeanDefinitionBuilder regionAttributesBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			RegionAttributesFactoryBean.class);

		ParsingUtils.parseOptionalRegionAttributes(parserContext, element, regionAttributesBuilder);
		ParsingUtils.parseStatistics(element, regionAttributesBuilder);
		ParsingUtils.parseEviction(parserContext, element, regionAttributesBuilder);
		ParsingUtils.parseExpiration(parserContext, element, regionAttributesBuilder);

		builder.addPropertyValue("attributes", regionAttributesBuilder.getBeanDefinition());

		List<Element> subElements = DomUtils.getChildElements(element);

		ManagedList<Object> interests = new ManagedList<Object>();

		for (Element subElement : subElements) {
			String subElementLocalName = subElement.getLocalName();

			if ("cache-listener".equals(subElementLocalName)) {
				builder.addPropertyValue("cacheListeners", ParsingUtils.parseRefOrNestedBeanDeclaration(
					parserContext, subElement, builder));
			}
			else if ("cache-loader".equals(subElementLocalName)) {
				builder.addPropertyValue("cacheLoader", ParsingUtils.parseRefOrNestedBeanDeclaration(
					parserContext, subElement, builder));
			}
			else if ("cache-writer".equals(subElementLocalName)) {
				builder.addPropertyValue("cacheWriter", ParsingUtils.parseRefOrNestedBeanDeclaration(
					parserContext, subElement, builder));
			}
			else if ("key-interest".equals(subElementLocalName)) {
				interests.add(parseKeyInterest(subElement, parserContext));
			}
			else if ("regex-interest".equals(subElementLocalName)) {
				interests.add(parseRegexInterest(subElement));
			}
		}

		if (!interests.isEmpty()) {
			builder.addPropertyValue("interests", interests);
		}
	}

	@Override
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion) {
		throw new UnsupportedOperationException(String.format(
			"doParseRegion(:Element, :ParserContext, :BeanDefinitionBuilder, :boolean) is not supported on %1$s",
			getClass().getName()));
	}

	private void validateDataPolicyShortcutMutualExclusion(final Element element, final ParserContext parserContext) {
		if (element.hasAttribute("data-policy") && element.hasAttribute("shortcut")) {
			parserContext.getReaderContext().error(String.format(
				"Only one of [data-policy, shortcut] may be specified with element '%1$s'.", element.getTagName()),
				element);
		}
	}

	private void parseDiskStoreAttribute(final Element element, final BeanDefinitionBuilder builder) {
		String diskStoreRefAttribute = element.getAttribute("disk-store-ref");

		if (StringUtils.hasText(diskStoreRefAttribute)) {
			builder.addPropertyValue("diskStoreName", diskStoreRefAttribute);
			builder.addDependsOn(diskStoreRefAttribute);
		}
	}

	private Object parseKeyInterest(Element keyInterestElement, ParserContext parserContext) {
		BeanDefinitionBuilder keyInterestBuilder = BeanDefinitionBuilder.genericBeanDefinition(Interest.class);

		parseCommonInterestAttributes(keyInterestElement, keyInterestBuilder);
		keyInterestBuilder.addConstructorArgValue(ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext,
			keyInterestElement, keyInterestBuilder, "key-ref"));

		return keyInterestBuilder.getBeanDefinition();
	}

	private Object parseRegexInterest(Element regexInterestElement) {
		BeanDefinitionBuilder regexInterestBuilder = BeanDefinitionBuilder.genericBeanDefinition(RegexInterest.class);

		parseCommonInterestAttributes(regexInterestElement, regexInterestBuilder);
		ParsingUtils.setPropertyValue(regexInterestElement, regexInterestBuilder, "pattern", "key");

		return regexInterestBuilder.getBeanDefinition();
	}

	private void parseCommonInterestAttributes(Element element, BeanDefinitionBuilder builder) {
		ParsingUtils.setPropertyValue(element, builder, "durable", "durable");
		ParsingUtils.setPropertyValue(element, builder, "result-policy", "policy");
		ParsingUtils.setPropertyValue(element, builder, "receive-values", "receiveValues");
	}

}
