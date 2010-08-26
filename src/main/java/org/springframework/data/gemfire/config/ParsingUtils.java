/*
 * Copyright 2010 the original author or authors.
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

import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Various minor utility used by the parser.
 * 
 * @author Costin Leau
 */
abstract class ParsingUtils {

	static void setPropertyValue(Element element, BeanDefinitionBuilder builder, String attrName, String propertyName) {
		String attr = element.getAttribute(attrName);
		if (StringUtils.hasText(attr)) {
			builder.addPropertyValue(propertyName, attr);
		}
	}

	static void setPropertyReference(Element element, BeanDefinitionBuilder builder, String attrName, String propertyName) {
		String attr = element.getAttribute(attrName);
		if (StringUtils.hasText(attr)) {
			builder.addPropertyReference(propertyName, attr);
		}
	}

	/**
	 * Utility method handling parsing of nested definition of the type:
	 * <pre>
	 *   <tag ref="someBean"/>
	 * </pre>
	 * or 
	 * <pre>
	 *   <tag>
	 *     <bean .... />
	 *   </tag>
	 * </pre>
	 * 
	 * @param element
	 * @return
	 */
	static Object parseRefOrNestedBeanDeclaration(ParserContext parserContext, Element element, BeanDefinitionBuilder builder) {
		String attr = element.getAttribute("ref");
		if (StringUtils.hasText(attr)) {
			// check if there's a nested declaration as well
			List<Element> childElements = DomUtils.getChildElements(element);
			if (!childElements.isEmpty()) {
				parserContext.getReaderContext().error(
						"either use the 'ref' attribute or a nested bean declaration for '" + element.getLocalName()
								+ "' element, but not both", element);

			}
			return new RuntimeBeanReference(attr);
		}
		// nested bean definition
		else {
			return parserContext.getDelegate().parsePropertySubElement(element, builder.getRawBeanDefinition());
		}
	}
}
