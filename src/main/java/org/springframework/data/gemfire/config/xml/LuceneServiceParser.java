/*
 * Copyright 2016-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.config.xml;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.data.gemfire.search.lucene.LuceneServiceFactoryBean;
import org.w3c.dom.Element;

/**
 * Spring XML {@link AbstractSingleBeanDefinitionParser parser} for a {@link LuceneServiceFactoryBean} bean definition.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser
 * @see org.springframework.data.gemfire.search.lucene.LuceneServiceFactoryBean
 * @since 1.1.0
 */
class LuceneServiceParser extends AbstractSingleBeanDefinitionParser {

	/**
	 * @inheritDoc
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return LuceneServiceFactoryBean.class;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doParse(Element element, BeanDefinitionBuilder builder) {
		super.doParse(element, builder);
		ParsingUtils.setCacheReference(element, builder);
	}
}
