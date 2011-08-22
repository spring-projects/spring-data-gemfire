/*
 * Copyright 2010-2011 the original author or authors.
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

import org.springframework.beans.BeanMetadataAttribute;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.AbstractBeanDefinitionParser;
import org.springframework.beans.factory.xml.BeanDefinitionParserDelegate;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

import com.gemstone.gemfire.cache.DiskWriteAttributesFactory;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.ExpirationAttributes;

/**
 * Various minor utility used by the parser.
 * 
 * @author Costin Leau
 */
abstract class ParsingUtils {

	private static final String ALIASES_KEY = ParsingUtils.class.getName() + ":aliases";

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
	 * Utility for parsing bean aliases. Normally parsed by AbstractBeanDefinitionParser however due to the attribute clash
	 * (bean uses 'name' for aliases while region use it to indicate their name), the parser needs to handle this differently by
	 * storing them as metadata which gets deleted just before registration.
	 * 
	 * @param element
	 * @param builder
	 */
	static void addBeanAliasAsMetadata(Element element, BeanDefinitionBuilder builder) {
		String[] aliases = new String[0];
		String name = element.getAttributeNS(BeanDefinitionParserDelegate.BEANS_NAMESPACE_URI,
				AbstractBeanDefinitionParser.NAME_ATTRIBUTE);

		if (StringUtils.hasLength(name)) {
			aliases = StringUtils.trimArrayElements(StringUtils.commaDelimitedListToStringArray(name));
		}
		BeanMetadataAttribute attr = new BeanMetadataAttribute(ALIASES_KEY, aliases);
		attr.setSource(element);
		builder.getRawBeanDefinition().addMetadataAttribute(attr);
	}

	static BeanDefinitionHolder replaceBeanAliasAsMetadata(BeanDefinitionHolder holder) {
		BeanDefinition beanDefinition = holder.getBeanDefinition();
		return new BeanDefinitionHolder(beanDefinition, holder.getBeanName(),
				(String[]) beanDefinition.removeAttribute(ALIASES_KEY));
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
	 * @param beanBuilder - beanbuilder for a RegionAttributesFactoryBean instance
	 * @return true if parsing actually occured, false otherwise
	 */
	static boolean parseDiskStorage(Element element, BeanDefinitionBuilder beanBuilder) {
		Element diskStoreElement = DomUtils.getChildElementByTagName(element, "disk-store");

		if (diskStoreElement == null)
			return false;

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

		return true;
	}

	/**
	 * Parses the eviction sub-element. Populates the given attribute factory with the proper attributes.
	 * 
	 * @param parserContext
	 * @param element
	 * @param attrBuilder
	 * @return true if parsing actually occured, false otherwise
	 */
	static boolean parseEviction(ParserContext parserContext, Element element, BeanDefinitionBuilder attrBuilder) {
		Element evictionElement = DomUtils.getChildElementByTagName(element, "eviction");

		if (evictionElement == null)
			return false;

		BeanDefinitionBuilder evictionDefBuilder = BeanDefinitionBuilder.genericBeanDefinition(EvictionAttributesFactoryBean.class);

		// do manual conversion since the enum is not public
		String attr = evictionElement.getAttribute("type");
		if (StringUtils.hasText(attr)) {
			evictionDefBuilder.addPropertyValue("type", EvictionType.valueOf(attr.toUpperCase()));
		}

		setPropertyValue(evictionElement, evictionDefBuilder, "threshold", "threshold");
		setPropertyValue(evictionElement, evictionDefBuilder, "action", "action");


		// get object sizer (if declared)
		Element objectSizerElement = DomUtils.getChildElementByTagName(evictionElement, "object-sizer");

		if (objectSizerElement != null) {
			Object sizer = parseRefOrNestedBeanDeclaration(parserContext, objectSizerElement, evictionDefBuilder);
			evictionDefBuilder.addPropertyValue("ObjectSizer", sizer);
		}

		attrBuilder.addPropertyValue("evictionAttributes", evictionDefBuilder.getBeanDefinition());
		return true;
	}


	static void parseStatistics(Element element, BeanDefinitionBuilder attrBuilder) {
		setPropertyValue(element, attrBuilder, "statistics", "statisticsEnabled");
	}

	/**
	 * Parses the expiration sub-elements. Populates the given attribute factory with proper attributes. 
	 * 
	 * @param parserContext
	 * @param element
	 * @param attrBuilder
	 * @return
	 */
	static boolean parseExpiration(ParserContext parserContext, Element element, BeanDefinitionBuilder attrBuilder) {
		boolean result = false;
		result |= parseExpiration(element, "region-ttl", "regionTimeToLive", attrBuilder);
		result |= parseExpiration(element, "region-tti", "regionIdleTimeout", attrBuilder);
		result |= parseExpiration(element, "entry-ttl", "entryTimeToLive", attrBuilder);
		result |= parseExpiration(element, "entry-tti", "entryIdleTimeout", attrBuilder);

		if (result) {
			// turn on statistics
			attrBuilder.addPropertyValue("statisticsEnabled", Boolean.TRUE);
		}
		return result;
	}

	private static boolean parseExpiration(Element rootElement, String elementName, String propertyName, BeanDefinitionBuilder attrBuilder) {
		Element expirationElement = DomUtils.getChildElementByTagName(rootElement, elementName);

		if (expirationElement == null)
			return false;

		String expirationTime = null;
		ExpirationAction action = ExpirationAction.INVALIDATE;


		// do manual conversion since the enum is not public
		String attr = expirationElement.getAttribute("timeout");
		if (StringUtils.hasText(attr)) {
			expirationTime = attr;
		}

		attr = expirationElement.getAttribute("action");
		if (StringUtils.hasText(attr)) {
			// figure out action based on string length
			attr = attr.trim();

			if (attr.length() == 10) {
				action = ExpirationAction.INVALIDATE;
			}
			else if (attr.length() == 7) {
				action = ExpirationAction.DESTROY;
			}
			else if (attr.length() == 13) {
				action = ExpirationAction.LOCAL_DESTROY;
			}
			else if (attr.length() == 16) {
				action = ExpirationAction.LOCAL_INVALIDATE;
			}
		}
		BeanDefinitionBuilder expirationAttributes = BeanDefinitionBuilder.genericBeanDefinition(ExpirationAttributes.class);
		expirationAttributes.addConstructorArgValue(expirationTime);
		expirationAttributes.addConstructorArgValue(action);
		attrBuilder.addPropertyValue(propertyName, expirationAttributes.getBeanDefinition());

		return true;
	}
}