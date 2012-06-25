/*
 * Copyright 2011 the original author or authors.
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

import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.SubRegionFactoryBean;
import org.w3c.dom.Element;

/**
 * Extension class dealing with the attribute clash (name) that triggers the
 * region name to be considered a bean alias. Overrides the automatic alias
 * detection and replaces it with its own using meta attributes (since the
 * parsing method is final).
 * 
 * @author Costin Leau
 */
abstract class AliasReplacingBeanDefinitionParser extends AbstractSingleBeanDefinitionParser {

	@Override
	protected Class<?> getBeanClass(Element element) {

		if (isSubRegion(element)) {
			return SubRegionFactoryBean.class;
		}
		else {
			return RegionFactoryBean.class;
		}
	}

	@Override
	protected final void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {

		ParsingUtils.addBeanAliasAsMetadata(element, builder);

		doParseInternal(element, parserContext, builder);
	}

	@Override
	protected void registerBeanDefinition(BeanDefinitionHolder definition, BeanDefinitionRegistry registry) {
		// add the aliases from the metadata
		super.registerBeanDefinition(ParsingUtils.replaceBeanAliasAsMetadata(definition), registry);
	}

	protected boolean isSubRegion(Element element) {
		return element.getParentNode().getLocalName().endsWith("region");
	}

	protected abstract void doParseInternal(Element element, ParserContext parserContext, BeanDefinitionBuilder builder);
}