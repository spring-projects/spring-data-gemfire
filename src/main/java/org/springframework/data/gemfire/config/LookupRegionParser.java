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
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Parser for &lt;lookup-region;gt; definitions.
 * 
 * @author Costin Leau
 * @author David Turanski
 */
class LookupRegionParser extends AbstractRegionParser {

	@Override
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion) {
		super.doParse(element, builder);

		ParsingUtils.setPropertyValue(element, builder, "name", "name");

		if (!subRegion) {
			String attr = element.getAttribute("cache-ref");
			// add cache reference (fallback to default if nothing is specified)
			builder.addPropertyReference("cache", (StringUtils.hasText(attr) ? attr : GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));
		}
		else {
			builder.addPropertyValue("lookupOnly", true);
		}

		// parse nested elements
		List<Element> subElements = DomUtils.getChildElements(element);
		for (Element subElement : subElements) {
			String name = subElement.getLocalName();
			if (name.endsWith("region")) {
				doParseSubRegion(element, subElement, parserContext, builder, subRegion);
			}
		}
	}

	@Override
	protected Class<?> getRegionFactoryClass() {
		return RegionLookupFactoryBean.class;
	}
}
