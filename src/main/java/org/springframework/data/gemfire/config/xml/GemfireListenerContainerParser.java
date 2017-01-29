/*
 * Copyright 2011-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.xml;

import java.util.List;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedSet;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.listener.ContinuousQueryDefinition;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.data.gemfire.listener.adapter.ContinuousQueryListenerAdapter;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Bean definition parser for the &lt;gfe:cq-listener-container&gt; element.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser
 * @see org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer
 */
class GemfireListenerContainerParser extends AbstractSingleBeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<ContinuousQueryListenerContainer> getBeanClass(Element element) {
		return ContinuousQueryListenerContainer.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		ParsingUtils.setPropertyReference(element, builder, "cache", "cache");
		ParsingUtils.setPropertyValue(element, builder, "auto-startup");
		ParsingUtils.setPropertyValue(element, builder, "phase");
		ParsingUtils.setPropertyValue(element, builder, "pool-name");
		ParsingUtils.setPropertyReference(element, builder, "error-handler", "errorHandler");
		ParsingUtils.setPropertyReference(element, builder, "task-executor", "taskExecutor");

		// parse nested Continuous Query Listeners
		List<Element> listenerElements = DomUtils.getChildElementsByTagName(element, "listener");

		if (!listenerElements.isEmpty()) {
			ManagedSet<BeanDefinition> listeners = new ManagedSet<BeanDefinition>(listenerElements.size());

			for (Element listenerElement : listenerElements) {
				listeners.add(parseListener(listenerElement));
			}

			builder.addPropertyValue("queryListeners", listeners);
		}
	}

	/**
	 * Parses a listener bean definition. Returns the listener bean definition
	 * (of type {@link ContinuousQueryDefinition)).
	 *
	 * @param element XML DOM {@link Element} to parse.
	 * @return a Spring {@link BeanDefinition} for the GemFire Continuous Query (CQ) listener configuration.
	 * @see org.springframework.beans.factory.xml.BeanDefinition
	 * @see org.w3c.dom.Element
	 */
	private BeanDefinition parseListener(Element element) {
		BeanDefinitionBuilder continuousQueryListenerBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			ContinuousQueryListenerAdapter.class);

		continuousQueryListenerBuilder.addConstructorArgReference(element.getAttribute("ref"));

		String attribute = element.getAttribute("method");

		if (StringUtils.hasText(attribute)) {
			continuousQueryListenerBuilder.addPropertyValue("defaultListenerMethod", attribute);
		}

		BeanDefinitionBuilder continuousQueryBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			ContinuousQueryDefinition.class);

		attribute = element.getAttribute("name");

		if (StringUtils.hasText(attribute)) {
			continuousQueryBuilder.addConstructorArgValue(attribute);
		}

		continuousQueryBuilder.addConstructorArgValue(element.getAttribute("query"));
		continuousQueryBuilder.addConstructorArgValue(continuousQueryListenerBuilder.getBeanDefinition());

		attribute = element.getAttribute("durable");

		if (StringUtils.hasText(attribute)) {
			continuousQueryBuilder.addConstructorArgValue(attribute);
		}

		return continuousQueryBuilder.getBeanDefinition();
	}
}
