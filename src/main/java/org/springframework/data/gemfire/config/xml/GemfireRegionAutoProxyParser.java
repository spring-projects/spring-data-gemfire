/*
 * Copyright 2002-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import java.util.Collections;

import org.springframework.aop.config.AopNamespaceUtils;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.serialization.json.JSONRegionAdvice;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

/**
 * Bean definition parser for the &lt;gfe-data:json-region-auto-proxy&lt; SDG XML namespace (XSD) element.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.xml.BeanDefinitionParser
 * @see JSONRegionAdvice
 */
class GemfireRegionAutoProxyParser implements BeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public BeanDefinition parse(Element element, ParserContext parserContext) {
		AopNamespaceUtils.registerAspectJAnnotationAutoProxyCreatorIfNecessary(parserContext, element);

		BeanDefinitionBuilder jsonRegionAdviceBuilder = BeanDefinitionBuilder.rootBeanDefinition(
			JSONRegionAdvice.class).setRole(BeanDefinition.ROLE_INFRASTRUCTURE);

		ParsingUtils.setPropertyValue(element, jsonRegionAdviceBuilder, "pretty-print");
		ParsingUtils.setPropertyValue(element, jsonRegionAdviceBuilder, "convert-returned-collections");

		String regionNames = element.getAttribute("included-regions");

		if (StringUtils.hasText(regionNames)) {
			String[] regions = StringUtils.commaDelimitedListToStringArray(regionNames);
			ManagedList<String> regionList = new ManagedList<String>(regions.length);
			Collections.addAll(regionList, regions);
			jsonRegionAdviceBuilder.addPropertyValue("includedRegions", regionList);
		}

		BeanDefinitionReaderUtils.registerWithGeneratedName(jsonRegionAdviceBuilder.getBeanDefinition(),
			parserContext.getRegistry());

		return jsonRegionAdviceBuilder.getBeanDefinition();
	}
}
