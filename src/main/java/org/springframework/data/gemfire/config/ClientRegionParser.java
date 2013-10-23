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

import com.gemstone.gemfire.cache.DataPolicy;

/**
 * Parser for &lt;client-region;gt; definitions.
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

		String cacheRefAttribute = element.getAttribute("cache-ref");

		builder.addPropertyReference("cache", (StringUtils.hasText(cacheRefAttribute) ? cacheRefAttribute
			: GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));

		ParsingUtils.setPropertyValue(element, builder, "close");
		ParsingUtils.setPropertyValue(element, builder, "destroy");
		ParsingUtils.setPropertyValue(element, builder, "data-policy", "dataPolicyName");
		ParsingUtils.setPropertyValue(element, builder, "name");
		ParsingUtils.setPropertyValue(element, builder, "pool-name");
		ParsingUtils.setPropertyValue(element, builder, "shortcut");

		parseDiskStoreAttribute(element, builder);

		boolean dataPolicyFrozen = false;

		// TODO why is the DataPolicy determined in the ClientRegionParser and not in the ClientRegionFactoryBean when evaluating the configuration settings?
		// set the persistent policy
		if (Boolean.parseBoolean(element.getAttribute("persistent"))) {
			builder.addPropertyValue("dataPolicy", DataPolicy.PERSISTENT_REPLICATE);
			dataPolicyFrozen = true;
		}

		// eviction + expiration + overflow + optional client region attributes
		BeanDefinitionBuilder regionAttributesBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			RegionAttributesFactoryBean.class);

		boolean overwriteDataPolicy = ParsingUtils.parseEviction(parserContext, element, regionAttributesBuilder);

		ParsingUtils.parseOptionalRegionAttributes(parserContext, element, regionAttributesBuilder);
		ParsingUtils.parseStatistics(element, regionAttributesBuilder);
		// NOTE parsing 'expiration' attributes must happen after parsing the user-defined setting for 'statistics'
		// in GemFire this attribute corresponds to the RegionAttributes 'statistics-enabled' setting).  If the user
		// configured expiration settings (any?), then statistics must be enabled, regardless if the user explicitly
		// disabled them.
		ParsingUtils.parseExpiration(parserContext, element, regionAttributesBuilder);

		// TODO understand why this determination is not made in the FactoryBean?
		if (!dataPolicyFrozen && overwriteDataPolicy) {
			builder.addPropertyValue("dataPolicy", DataPolicy.NORMAL);
		}

		builder.addPropertyValue("attributes", regionAttributesBuilder.getBeanDefinition());

		List<Element> subElements = DomUtils.getChildElements(element);
		ManagedList<Object> interests = new ManagedList<Object>();

		// parse nested elements
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
				interests.add(parseKeyInterest(parserContext, subElement));
			}
			else if ("regex-interest".equals(subElementLocalName)) {
				interests.add(parseRegexInterest(parserContext, subElement));
			}
		}

		// TODO is adding an 'interests' property really based on whether there are "sub-elements", or should it be based on whether there are "interests" (as in 'key-interest' and 'regex-interest')?
		if (!subElements.isEmpty()) {
			builder.addPropertyValue("interests", interests);
		}
	}

	private void parseDiskStoreAttribute(Element element, BeanDefinitionBuilder builder) {
		String diskStoreRefAttribute = element.getAttribute("disk-store-ref");

		if (StringUtils.hasText(diskStoreRefAttribute)) {
			builder.addPropertyValue("diskStoreName", diskStoreRefAttribute);
			builder.addDependsOn(diskStoreRefAttribute);
		}
	}

	@Override
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion) {
		throw new UnsupportedOperationException(String.format(
			"doParseRegion(:Element, :ParserContext, :BeanDefinitionBuilder, subRegion:boolean) is not supported on %1$s",
			getClass().getName()));
	}

	private Object parseKeyInterest(ParserContext parserContext, Element subElement) {
		BeanDefinitionBuilder keyInterestBuilder = BeanDefinitionBuilder.genericBeanDefinition(Interest.class);

		parseCommonInterestAttributes(subElement, keyInterestBuilder);

		Object key = ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, keyInterestBuilder,
			"key-ref");

		keyInterestBuilder.addConstructorArgValue(key);

		return keyInterestBuilder.getBeanDefinition();
	}

	private Object parseRegexInterest(ParserContext parserContext, Element subElement) {
		BeanDefinitionBuilder regexInterestBuilder = BeanDefinitionBuilder.genericBeanDefinition(RegexInterest.class);

		parseCommonInterestAttributes(subElement, regexInterestBuilder);
		ParsingUtils.setPropertyValue(subElement, regexInterestBuilder, "pattern", "key");

		return regexInterestBuilder.getBeanDefinition();
	}

	private void parseCommonInterestAttributes(Element element, BeanDefinitionBuilder builder) {
		ParsingUtils.setPropertyValue(element, builder, "durable", "durable");
		ParsingUtils.setPropertyValue(element, builder, "result-policy", "policy");
		ParsingUtils.setPropertyValue(element, builder, "receive-values", "receiveValues");
	}

}
