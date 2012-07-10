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

import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Base class for all Region Parsers
 * 
 * @author David Turanski
 */
abstract class AbstractRegionParser extends AliasReplacingBeanDefinitionParser {
	protected final Log log = LogFactory.getLog(getClass());

	@Override
	protected void doParseInternal(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		super.doParse(element, builder);
		boolean subRegion = isSubRegion(element);

		doParseRegion(element, parserContext, builder, subRegion);

		if (subRegion) {
			builder.addPropertyValue("parent", parserContext.getContainingBeanDefinition().getAttribute("parent"));
			builder.addPropertyValue("regionName", element.getAttribute(NAME_ATTRIBUTE));
		}
	}

	protected abstract void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion);

	protected void doParseSubRegion(Element element, Element subElement, ParserContext parserContext,
			BeanDefinitionBuilder builder, boolean subRegion) {

		String regionPath = null;
		String parentBeanName = null;
		if (subRegion) {
			parentBeanName = parserContext.getContainingBeanDefinition().getAttribute("regionPath").toString();
		}
		else {
			parentBeanName = getRegionNameFromElement(element);
		}
		regionPath = StringUtils.arrayToDelimitedString(new String[] { parentBeanName,
				getRegionNameFromElement(subElement) }, "/");
		if (!regionPath.startsWith("/")) {
			regionPath = "/" + regionPath;
		}
		/*
		 * The Region parser needs some context to handle recursion correctly
		 */
		builder.getBeanDefinition().setAttribute("parent",
				new BeanDefinitionHolder(builder.getBeanDefinition(), parentBeanName));
		builder.getBeanDefinition().setAttribute("regionPath", regionPath);

		// Make recursive call
		BeanDefinition subRegionDef = this.parseSubRegion(subElement, parserContext, builder);
		// TODO: Is there a better work-around?
		/*
		 * This setting prevents the BF from generating a name for this been
		 */
		subRegionDef.setScope(BeanDefinition.SCOPE_PROTOTYPE);

		if (log.isDebugEnabled()) {
			log.debug("registering subregion as " + regionPath);
		}
		this.registerBeanDefinition(new BeanDefinitionHolder(subRegionDef, regionPath), parserContext.getRegistry());
	}

	protected void doParseCommonRegionConfiguration(Element element, ParserContext parserContext,
			BeanDefinitionBuilder builder, BeanDefinitionBuilder attrBuilder, boolean subRegion) {

		if (!subRegion) {
			String cacheRef = element.getAttribute("cache-ref");
			// add cache reference (fallback to default if nothing is specified)
			builder.addPropertyReference("cache", (StringUtils.hasText(cacheRef) ? cacheRef : "gemfire-cache"));
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
		ParsingUtils.setPropertyValue(element, builder, "data-policy", "dataPolicyName");

		if (StringUtils.hasText(element.getAttribute("disk-store-ref"))) {
			ParsingUtils.setPropertyValue(element, builder, "disk-store-ref", "diskStoreName");
			builder.addDependsOn(element.getAttribute("disk-store-ref"));
		}

		ParsingUtils.parseExpiration(parserContext, element, attrBuilder);
		ParsingUtils.parseEviction(parserContext, element, attrBuilder);
		ParsingUtils.parseMembershipAttributes(parserContext, element, attrBuilder);

		List<Element> subElements = DomUtils.getChildElements(element);

		// parse nested elements
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
			// subregion
			else if (name.endsWith("region")) {
				doParseSubRegion(element, subElement, parserContext, builder, subRegion);
			}
		}
	}

	private BeanDefinition parseSubRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		BeanDefinition beanDefinition = parserContext.getDelegate().parseCustomElement(element,
				builder.getBeanDefinition());
		return beanDefinition;
	}

	private String getRegionNameFromElement(Element element) {
		String name = element.getAttribute(NAME_ATTRIBUTE);
		return StringUtils.hasText(name) ? name : element.getAttribute(ID_ATTRIBUTE);
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