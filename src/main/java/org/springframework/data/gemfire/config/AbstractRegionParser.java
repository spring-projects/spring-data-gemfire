/*
 * Copyright 2012 the original author or authors.
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedArray;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.SubRegionFactoryBean;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Base class for all Region Parsers
 *
 * @author David Turanski
 */
abstract class AbstractRegionParser extends AbstractSingleBeanDefinitionParser {

	protected final Log log = LogFactory.getLog(getClass());

	@Override
	protected Class<?> getBeanClass(Element element) {
		return (isSubRegion(element) ? SubRegionFactoryBean.class : getRegionFactoryClass());
	}

	protected abstract Class<?> getRegionFactoryClass();

	protected boolean isSubRegion(Element element) {
		String localName = element.getParentNode().getLocalName();
		return (localName != null && localName.endsWith("region"));
	}

	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		super.doParse(element, builder);
		boolean subRegion = isSubRegion(element);
        doParseRegion(element, parserContext, builder, subRegion);
	}

	protected abstract void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion);

	protected void doParseCommonRegionConfiguration(Element element, ParserContext parserContext,
			BeanDefinitionBuilder builder, BeanDefinitionBuilder attrBuilder, boolean subRegion) {

		if (!subRegion) {
			String cacheRef = element.getAttribute("cache-ref");
			// add cache reference (fallback to default if nothing is specified)
			builder.addPropertyReference("cache", (StringUtils.hasText(cacheRef) ? cacheRef : GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));
			ParsingUtils.setPropertyValue(element, builder, "close");
			ParsingUtils.setPropertyValue(element, builder, "destroy");
		}
		// add attributes
		ParsingUtils.setPropertyValue(element, builder, "name");

		ParsingUtils.parseOptionalRegionAttributes(parserContext, element, attrBuilder);
		ParsingUtils.parseStatistics(element, attrBuilder);
		ParsingUtils.setPropertyValue(element, attrBuilder, "publisher");
		if (!isSubRegion(element)) {
			ParsingUtils.setPropertyValue(element, builder, "persistent");
		}
		// set the data policy
		ParsingUtils.setPropertyValue(element, builder, "data-policy");

		if (StringUtils.hasText(element.getAttribute("disk-store-ref"))) {
			ParsingUtils.setPropertyValue(element, builder, "disk-store-ref", "diskStoreName");
			builder.addDependsOn(element.getAttribute("disk-store-ref"));
		}

		ParsingUtils.parseExpiration(parserContext, element, attrBuilder);
		ParsingUtils.parseSubscription(parserContext, element, attrBuilder);
		ParsingUtils.parseEviction(parserContext, element, attrBuilder);
		ParsingUtils.parseMembershipAttributes(parserContext, element, attrBuilder);

		String enableGateway = element.getAttribute("enable-gateway");
		String hubId = element.getAttribute("hub-id");
		// Factory will enable gateway if it is not set and hub-id is set.
		if (StringUtils.hasText(enableGateway)) {
			if (GemfireUtils.isGemfireVersion7OrAbove()) {
				log.warn("'enable-gateway' is deprecated since Gemfire 7.0");
			}
		}
		ParsingUtils.setPropertyValue(element, builder, "enable-gateway");

		if (StringUtils.hasText(hubId)) {
			if (GemfireUtils.isGemfireVersion7OrAbove()) {
				log.warn("'hub-id' is deprecated since Gemfire 7.0");
			}
			if (!CollectionUtils.isEmpty(DomUtils.getChildElementsByTagName(element, "gateway-sender"))) {
				parserContext.getReaderContext().error("It is invalid to specify both 'hub-id' and 'gateway-sender'",
						element);
			}
		}
		ParsingUtils.setPropertyValue(element, builder, "hub-id");

		// Parse child elements

	 	parseCollectionOfCustomSubElements(parserContext, element, builder,
				"com.gemstone.gemfire.cache.wan.GatewaySender", "gateway-sender","gatewaySenders");
		parseCollectionOfCustomSubElements(parserContext, element, builder,
				"com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue", "async-event-queue", "asyncEventQueues");

		List<Element> subElements = DomUtils.getChildElements(element);
		for (Element subElement : subElements) {
			if (subElement.getLocalName().equals("cache-listener")) {
				builder.addPropertyValue("cacheListeners",
						ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder));
			}
			else if (subElement.getLocalName().equals("cache-loader")) {
				builder.addPropertyValue("cacheLoader",
						ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder));
			}
			else if (subElement.getLocalName().equals("cache-writer")) {
				builder.addPropertyValue("cacheWriter",
						ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder));
			}
		}

		if (!subRegion) {
			Map<String, Element> allSubRegionElements = new HashMap<String, Element>();
			findSubregionElements(element, getRegionNameFromElement(element), allSubRegionElements);
			if (!CollectionUtils.isEmpty(allSubRegionElements)) {
				for (Map.Entry<String, Element> entry : allSubRegionElements.entrySet()) {
					parseSubRegion(entry.getValue(), parserContext, entry.getKey());
				}
			}
		}

	}

	private void parseCollectionOfCustomSubElements(ParserContext parserContext, Element element,
			BeanDefinitionBuilder builder, String className, String subElementName, String propertyName) {
		List<Element> subElements = DomUtils.getChildElementsByTagName(element, new String[] { subElementName,
				subElementName + "-ref" });
		if (!CollectionUtils.isEmpty(subElements)) {

			ManagedArray array = new ManagedArray(className, subElements.size());
			for (Element subElement : subElements) {
				array.add(ParsingUtils.parseRefOrNestedCustomElement(parserContext, subElement, builder));
			}
			builder.addPropertyValue(propertyName, array);
		}
	}

	private BeanDefinition parseSubRegion(Element element, ParserContext parserContext, String regionPath) {
		String parentBeanName = getParentPathForSubRegion(regionPath);
		String regionName = getRegionNameFromElement(element); // do before 'renaming' the element, below

		element.setAttribute("id", regionPath);
		element.setAttribute("name", regionPath);

		BeanDefinition beanDefinition = parserContext.getDelegate().parseCustomElement(element);
		beanDefinition.getPropertyValues().add("parent", new RuntimeBeanReference(parentBeanName));
		beanDefinition.getPropertyValues().add("regionName", regionName);

		return beanDefinition;
	}

	private String getRegionNameFromElement(Element element) {
		String name = element.getAttribute(NAME_ATTRIBUTE);
		return (StringUtils.hasText(name) ? name : element.getAttribute(ID_ATTRIBUTE));
	}

    private String buildPathForSubRegion(String parentName, String regionName) {
        String regionPath = StringUtils.arrayToDelimitedString(new String[] { parentName, regionName }, "/");
        if (!regionPath.startsWith("/")) {
            regionPath = "/" + regionPath;
        }
        return regionPath;
    }

	private String getParentPathForSubRegion(String regionPath) {
		int index = regionPath.lastIndexOf("/");
		String parentPath = regionPath.substring(0, index);
		if (parentPath.lastIndexOf("/") == 0) {
			parentPath = parentPath.substring(1);
		}
		return parentPath;
	}

	private void findSubregionElements(Element parent, String parentPath, Map<String, Element> allSubregionElements) {
		for (Element element : DomUtils.getChildElements(parent)) {
			if (element.getLocalName().endsWith("region")) {
				String regionPath = buildPathForSubRegion(parentPath, getRegionNameFromElement(element));
				allSubregionElements.put(regionPath, element);
				findSubregionElements(element, regionPath, allSubregionElements);
			}
		}
	}

}
