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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.LookupRegionFactoryBean;
import org.w3c.dom.Element;

/**
 * Parser for &lt;lookup-region;gt; definitions.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
class LookupRegionParser extends AbstractRegionParser {

	@Override
	protected Class<?> getRegionFactoryClass() {
		return LookupRegionFactoryBean.class;
	}

	@Override
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion) {
		super.doParse(element, builder);

		String resolvedCacheRef = ParsingUtils.resolveCacheReference(element.getAttribute("cache-ref"));

		builder.addPropertyReference("cache", resolvedCacheRef);
		ParsingUtils.setPropertyValue(element, builder, "name", "name");

		if (!subRegion) {
			parseSubRegions(element, parserContext, resolvedCacheRef);
		}
	}

}
