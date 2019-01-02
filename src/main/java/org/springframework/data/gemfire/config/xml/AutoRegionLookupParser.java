/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.config.support.AutoRegionLookupBeanPostProcessor;
import org.w3c.dom.Element;

/**
 * Bean definition parser for the &lt;gfe:auto-region-lookup&gt; SDG XML namespace (XSD) element.
 *
 * This parser will register a Spring {@link org.springframework.beans.factory.config.BeanPostProcessor)
 * that discovers all Regions defined in GemFire's native {@literal cache.xml} file, or when using
 * GemFire 8's cluster-based configuration service to define Regions, to create corresponding beans
 * in the Spring application context.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.xml.BeanDefinitionParser
 * @see org.springframework.data.gemfire.config.support.AutoRegionLookupBeanPostProcessor
 * @since 1.5.0
 */
class AutoRegionLookupParser implements BeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public BeanDefinition parse(Element element, ParserContext parserContext) {
		registerAutoRegionLookupBeanPostProcessor(element, parserContext);
		return null;
	}

	/* (non-Javadoc) */
	private void registerAutoRegionLookupBeanPostProcessor(Element element, ParserContext parserContext) {
		AbstractBeanDefinition autoRegionLookupBeanPostProcessor = BeanDefinitionBuilder
			.rootBeanDefinition(AutoRegionLookupBeanPostProcessor.class)
			.setRole(BeanDefinition.ROLE_INFRASTRUCTURE)
			.getBeanDefinition();

		autoRegionLookupBeanPostProcessor.setSource(parserContext.extractSource(element));

		BeanDefinitionReaderUtils.registerWithGeneratedName(autoRegionLookupBeanPostProcessor,
			parserContext.getRegistry());
	}
}
