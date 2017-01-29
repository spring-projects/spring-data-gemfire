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

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.function.config.GemfireFunctionBeanPostProcessor;
import org.w3c.dom.Element;

/**
 * A Spring {@link BeanDefinitionParser} to enable Spring Data GemFire Function annotation support.
 *
 * Bean definition parser for the &lt;gfe:annotation-driven&gt; SDG XML namespace (XSD) element.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.xml.BeanDefinitionParser
 * @see org.springframework.data.gemfire.function.config.GemfireFunctionBeanPostProcessor
 */
class AnnotationDrivenParser implements BeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public BeanDefinition parse(Element element, ParserContext parserContext) {
		registerGemfireFunctionBeanPostProcessor(element, parserContext);
		return null;
	}

	/**
	 * Registers the {@link GemfireFunctionBeanPostProcessor} as a bean with the Spring application context.
	 *
	 * @param element {@link Element} being parsed.
	 * @param parserContext {@link ParserContext} used capture contextual information while parsing.
	 */
	private void registerGemfireFunctionBeanPostProcessor(Element element, ParserContext parserContext) {
		AbstractBeanDefinition gemfireFunctionBeanPostProcessor = BeanDefinitionBuilder
			.rootBeanDefinition(GemfireFunctionBeanPostProcessor.class)
			.setRole(BeanDefinition.ROLE_INFRASTRUCTURE)
			.getBeanDefinition();

		gemfireFunctionBeanPostProcessor.setSource(parserContext.extractSource(element));

		BeanDefinitionReaderUtils.registerWithGeneratedName(gemfireFunctionBeanPostProcessor,
			parserContext.getRegistry());
	}
}
