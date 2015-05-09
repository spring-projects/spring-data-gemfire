/*
 * Copyright 2011-2013 the original author or authors.
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

import java.util.concurrent.atomic.AtomicBoolean;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.IndexFactoryBean;
import org.w3c.dom.Element;

/**
 * Namespace parser for &lt;index;gt; bean definitions.
 * 
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @since 1.1.0
 */
class IndexParser extends AbstractSimpleBeanDefinitionParser {

	private static final AtomicBoolean registered = new AtomicBoolean(false);

	protected static void registerCreateDefinedIndexesApplicationListener(ParserContext parserContext) {
		if (registered.compareAndSet(false, true)) {
			AbstractBeanDefinition createDefinedIndexesApplicationListener = BeanDefinitionBuilder
				.genericBeanDefinition(CreateDefinedIndexesApplicationListener.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE)
				.getBeanDefinition();

			BeanDefinitionReaderUtils.registerWithGeneratedName(createDefinedIndexesApplicationListener,
				parserContext.getRegistry());
		}

	}
	protected Class<?> getBeanClass(Element element) {
		return IndexFactoryBean.class;
	}

	@Override
	protected boolean isEligibleAttribute(String attributeName) {
		return (!"cache-ref".equals(attributeName) && super.isEligibleAttribute(attributeName));
	}

	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		registerCreateDefinedIndexesApplicationListener(parserContext);
		ParsingUtils.setPropertyReference(element, builder, "cache-ref", "cache");
		super.doParse(element, parserContext, builder);
	}

}
