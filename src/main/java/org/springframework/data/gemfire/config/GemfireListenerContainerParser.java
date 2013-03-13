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

import java.util.List;

import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.ManagedSet;
import org.springframework.beans.factory.xml.AbstractSimpleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.listener.ContinuousQueryDefinition;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.data.gemfire.listener.adapter.ContinuousQueryListenerAdapter;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

/**
 * Parser for SGF <code>&lt;cq-listener-container&gt;</code> element.
 * 
 * @author Costin Leau
 */
class GemfireListenerContainerParser extends AbstractSimpleBeanDefinitionParser {

	@Override
	protected Class<ContinuousQueryListenerContainer> getBeanClass(Element element) {
		return ContinuousQueryListenerContainer.class;
	}

	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		// parse attributes (but replace the value assignment with references)
		NamedNodeMap attributes = element.getAttributes();

		for (int x = 0; x < attributes.getLength(); x++) {
			Attr attribute = (Attr) attributes.item(x);
			if (isEligibleAttribute(attribute, parserContext)) {
				String propertyName = extractPropertyName(attribute.getLocalName());
				Assert.state(StringUtils.hasText(propertyName),
						"Illegal property name returned from 'extractPropertyName(String)': cannot be null or empty.");
				builder.addPropertyReference(propertyName, attribute.getValue());
			}
		}


		ParsingUtils.setPropertyValue(element, builder, "phase");
		ParsingUtils.setPropertyValue(element, builder, "pool-name");

		// Register the Id if one has been provided.
		String id = element.getAttribute("id");
		if( StringUtils.hasText(id)) {
			//BeanDefinitionHolder holder = new BeanDefinitionHolder(builder.getBeanDefinition(), id, null);
			//BeanDefinitionReaderUtils.registerBeanDefinition(holder,  parserContext.getRegistry());
		}
		
		postProcess(builder, element);

		// parse nested listeners
		List<Element> listDefs = DomUtils.getChildElementsByTagName(element, "listener");

		if (!listDefs.isEmpty()) {
			ManagedSet<BeanDefinition> listeners = new ManagedSet<BeanDefinition>(listDefs.size());
			for (Element listElement : listDefs) {
				listeners.add(parseListener(listElement, parserContext, builder));
			}

			builder.addPropertyValue("queryListeners", listeners);
		}
	}

	@Override
	protected boolean isEligibleAttribute(String attributeName) {
		return (!"phase".equals(attributeName) | !"id".equals(attributeName));
	}

	/**
	 * Parses a listener definition. Returns the listener bean reference definition (of a {@link ContinuousQueryDefinition}).
	 * 
	 * @param element
	 * @return
	 */
	private BeanDefinition parseListener(Element element, ParserContext parserContext, BeanDefinitionBuilder definition) {
		
		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(ContinuousQueryListenerAdapter.class);
		builder.addConstructorArgReference(element.getAttribute("ref"));

		String attr = element.getAttribute("method");
		if (StringUtils.hasText(attr)) {
			builder.addPropertyValue("defaultListenerMethod", attr);
		}

		BeanDefinitionBuilder defBuilder = BeanDefinitionBuilder.genericBeanDefinition(ContinuousQueryDefinition.class);

		attr = element.getAttribute("name");
		if (StringUtils.hasText(attr)) {
			defBuilder.addConstructorArgValue(attr);
		}
		
		defBuilder.addConstructorArgValue(element.getAttribute("query"));
		defBuilder.addConstructorArgValue(builder.getBeanDefinition());

		attr = element.getAttribute("durable");
		if (StringUtils.hasText(attr)) {
			defBuilder.addConstructorArgValue(attr);
		}

		return defBuilder.getBeanDefinition();
	}
	
	@Override
	protected boolean shouldGenerateId() {
		return false;
	}
}