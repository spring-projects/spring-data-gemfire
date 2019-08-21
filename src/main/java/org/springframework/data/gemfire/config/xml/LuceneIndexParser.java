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

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;

import org.w3c.dom.Element;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;

/**
 * Spring XML {@link AbstractSingleBeanDefinitionParser parser} for the {@link LuceneIndexFactoryBean} bean definition.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser
 * @see org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean
 * @since 1.1.0
 */
class LuceneIndexParser extends AbstractSingleBeanDefinitionParser {

	/**
	 * @inheritDoc
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return LuceneIndexFactoryBean.class;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {

		super.doParse(element, parserContext, builder);

		ParsingUtils.setCacheReference(element, builder);
		ParsingUtils.setPropertyValue(element, builder, "destroy");
		ParsingUtils.setPropertyReference(element, builder, "lucene-service-ref", "luceneService");
		ParsingUtils.setPropertyValue(element, builder, "name", "indexName");
		ParsingUtils.setPropertyReference(element, builder, "region-ref", "region");
		ParsingUtils.setPropertyValue(element, builder, "region-path");

		Optional.ofNullable(element.getAttribute("fields"))
			.filter(StringUtils::hasText)
			.ifPresent(fields -> builder.addPropertyValue("fields",
				Arrays.stream(fields.split(",")).map(String::trim).collect(Collectors.toList())));

		Optional.ofNullable(DomUtils.getChildElementByTagName(element, "field-analyzers"))
			.ifPresent(fieldAnalyzersElement -> builder.addPropertyValue("fieldAnalyzers",
				ParsingUtils.parseRefOrSingleNestedBeanDeclaration(fieldAnalyzersElement, parserContext, builder)));

		Optional.ofNullable(DomUtils.getChildElementByTagName(element, "lucene-serializer"))
			.ifPresent(luceneSerializerElement -> builder.addPropertyValue("luceneSerializer",
				ParsingUtils.parseRefOrSingleNestedBeanDeclaration(luceneSerializerElement, parserContext, builder)));
	}
}
