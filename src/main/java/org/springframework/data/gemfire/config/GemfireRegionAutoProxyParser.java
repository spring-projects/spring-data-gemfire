/*
 * Copyright 2002-2013 the original author or authors.
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
package org.springframework.data.gemfire.config;

import org.springframework.aop.config.AopNamespaceUtils;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.support.JSONRegionAdvice;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

/**
 * @author David Turanski
 *
 */
public class GemfireRegionAutoProxyParser implements BeanDefinitionParser {

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.xml.BeanDefinitionParser#parse(org.w3c.dom.Element, org.springframework.beans.factory.xml.ParserContext)
	 */
	@Override
	public BeanDefinition parse(Element element, ParserContext parserContext) {
		AopNamespaceUtils.registerAspectJAnnotationAutoProxyCreatorIfNecessary(parserContext, element);
		BeanDefinitionBuilder builder = BeanDefinitionBuilder.rootBeanDefinition(JSONRegionAdvice.class);
		ParsingUtils.setPropertyValue(element, builder, "pretty-print");
		ParsingUtils.setPropertyValue(element, builder, "convert-returned-collections");
		String regionNames = element.getAttribute("included-regions");
		if (StringUtils.hasText(regionNames)) {
			String[] regions = StringUtils.commaDelimitedListToStringArray(regionNames);
			ManagedList<String> regionList = new ManagedList<String>(regions.length);
			for (String regionRef : regions) {
				regionList.add(regionRef);
			}
			builder.addPropertyValue("includedRegions", regionList);
		}
		BeanDefinitionReaderUtils.registerWithGeneratedName(builder.getBeanDefinition(), parserContext.getRegistry());
		return builder.getBeanDefinition();
	}
}
