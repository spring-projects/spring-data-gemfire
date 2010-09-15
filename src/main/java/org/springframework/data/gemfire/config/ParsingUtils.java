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
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

import com.gemstone.gemfire.cache.DiskWriteAttributesFactory;

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
		return parseRefOrNestedBeanDeclaration(parserContext, element, builder, "ref");
	}

	static Object parseRefOrNestedBeanDeclaration(ParserContext parserContext, Element element, BeanDefinitionBuilder builder, String refAttrName) {
		String attr = element.getAttribute(refAttrName);
		boolean hasRef = StringUtils.hasText(attr);

		// check nested declarations
		List<Element> childElements = DomUtils.getChildElements(element);

		if (hasRef) {
			if (!childElements.isEmpty()) {
				parserContext.getReaderContext().error(
						"either use the '" + refAttrName + "' attribute or a nested bean declaration for '"
								+ element.getLocalName() + "' element, but not both", element);
			}
			return new RuntimeBeanReference(attr);
		}

		if (childElements.isEmpty()) {
			parserContext.getReaderContext().error(
					"specify either '" + refAttrName + "' attribute or a nested bean declaration for '"
							+ element.getLocalName() + "' element", element);
		}

		// nested parse nested bean definition
		if (childElements.size() == 1) {
			return parserContext.getDelegate().parsePropertySubElement(childElements.get(0),
					builder.getRawBeanDefinition());
		}

		ManagedList<Object> list = new ManagedList<Object>();

		for (Element el : childElements) {
			list.add(parserContext.getDelegate().parsePropertySubElement(el, builder.getRawBeanDefinition()));
		}

		return list;
	}

	/**
	 * Parses disk store sub-element. Populates the given attribute factory with the proper attributes.
	 * 
	 * @param element - element enclosing the disk-store definition
	 * @param beanBuilder - beanbuilder for a RegionAttributesFactory instance
	 */
	static void parseDiskStorage(Element element, BeanDefinitionBuilder beanBuilder) {
		Element diskStoreElement = DomUtils.getChildElementByTagName(element, "disk-store");

		if (diskStoreElement == null)
			return;

		BeanDefinitionBuilder diskDefBuilder = BeanDefinitionBuilder.genericBeanDefinition(DiskWriteAttributesFactory.class);
		setPropertyValue(diskStoreElement, diskDefBuilder, "synchronous-write", "synchronous");
		setPropertyValue(diskStoreElement, diskDefBuilder, "auto-compact", "rollOplogs");
		setPropertyValue(diskStoreElement, diskDefBuilder, "max-oplog-size", "maxOplogSize");
		setPropertyValue(diskStoreElement, diskDefBuilder, "time-interval", "timeInterval");
		setPropertyValue(diskStoreElement, diskDefBuilder, "queue-size", "bytesThreshold");


		// parse nested disk-dir
		List<Element> list = DomUtils.getChildElementsByTagName(diskStoreElement, "disk-dir");
		ManagedList<Object> locations = new ManagedList<Object>(list.size());
		ManagedList<Object> sizes = new ManagedList<Object>(list.size());

		for (Element diskDirElement : list) {
			locations.add(diskDirElement.getAttribute("location"));

			String attr = diskDirElement.getAttribute("max-size");
			sizes.add(StringUtils.hasText(attr) ? attr : "10240");
		}

		// wrap up the disk attributes factory to call 'create'

		BeanDefinitionBuilder factoryWrapper = BeanDefinitionBuilder.genericBeanDefinition(DiskWriteAttributesFactoryBean.class);
		factoryWrapper.addPropertyValue("diskAttributesFactory", diskDefBuilder.getBeanDefinition());
		beanBuilder.addPropertyValue("diskWriteAttributes", factoryWrapper.getBeanDefinition());
		beanBuilder.addPropertyValue("diskDirs", locations);
		beanBuilder.addPropertyValue("diskSizes", sizes);
	}

	/**
	 * Parses the eviction sub-element. Populates the given attribute factory with the proper attributes.
	 * 
	 * @param element
	 * @param attrBuilder
	 */
	static void parseEviction(Element element, BeanDefinitionBuilder attrBuilder) {
		Element evictionElement = DomUtils.getChildElementByTagName(element, "eviction");

		if (evictionElement == null)
			return;

		BeanDefinitionBuilder evictionDefBuilder = BeanDefinitionBuilder.genericBeanDefinition(EvictionAttributesFactoryBean.class);

		// do manual conversion since the enum is not public
		String attr = evictionElement.getAttribute("type");
		if (StringUtils.hasText(attr)) {
			evictionDefBuilder.addPropertyValue("type", EvictionType.valueOf(attr.toUpperCase()));
		}

		setPropertyValue(evictionElement, evictionDefBuilder, "threshold", "threshold");
		setPropertyValue(evictionElement, evictionDefBuilder, "action", "action");

		attrBuilder.addPropertyValue("evictionAttributes", evictionDefBuilder.getBeanDefinition());
	}
}