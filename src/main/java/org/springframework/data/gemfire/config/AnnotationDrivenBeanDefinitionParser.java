/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * https://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.config;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.function.config.GemfireFunctionBeanPostProcessor;
import org.w3c.dom.Element;

/**
 * @author David Turanski
 *
 */
public class AnnotationDrivenBeanDefinitionParser implements BeanDefinitionParser {

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.xml.BeanDefinitionParser#parse(org.w3c.dom.Element, org.springframework.beans.factory.xml.ParserContext)
	 */
	@Override
	public BeanDefinition parse(Element element, ParserContext parserContext) {
		registerGemfireFunctionBeanPostProcessor(element, parserContext);
		return null;
	}

	/**
	 * 
	 */
	private void registerGemfireFunctionBeanPostProcessor(Element element, ParserContext parserContext) {
		Object source = parserContext.extractSource(element);
		RootBeanDefinition beanDefinition = new RootBeanDefinition(GemfireFunctionBeanPostProcessor.class);
		beanDefinition.setRole(BeanDefinition.ROLE_INFRASTRUCTURE);
		beanDefinition.setSource(source);
		BeanDefinitionReaderUtils.registerWithGeneratedName(beanDefinition, parserContext.getRegistry());
	}

}
