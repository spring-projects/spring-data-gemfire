/*
 * Copyright 2010-2013 the original author or authors.
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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeanMetadataAttribute;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedArray;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.AbstractBeanDefinitionParser;
import org.springframework.beans.factory.xml.BeanDefinitionParserDelegate;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.core.Conventions;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.ExpirationAttributes;
import com.gemstone.gemfire.cache.LossAction;
import com.gemstone.gemfire.cache.MembershipAttributes;
import com.gemstone.gemfire.cache.ResumptionAction;
import com.gemstone.gemfire.cache.Scope;

/**
 * Various minor utility used by the parser.
 * 
 * @author Costin Leau
 * @author David Turanski
 * @author Lyndon Adams
 */
abstract class ParsingUtils {
	
	private static Log log = LogFactory.getLog(ParsingUtils.class);

	final static String GEMFIRE_VERSION = CacheFactory.getVersion();

	private static final String ALIASES_KEY = ParsingUtils.class.getName() + ":aliases";



	static void setPropertyValue(Element element, BeanDefinitionBuilder builder, String attributeName,
			String propertyName, Object defaultValue) {
		String attr = element.getAttribute(attributeName);
		if (StringUtils.hasText(attr)) {
			builder.addPropertyValue(propertyName, attr);
		} else {
			if (defaultValue != null) {
				builder.addPropertyValue(propertyName, defaultValue);
			}
		}
	}
	
	static void setPropertyValue(Element element, BeanDefinitionBuilder builder, String attributeName,
			String propertyName) {
		setPropertyValue(element, builder, attributeName, propertyName,null);
	}
	

	static void setPropertyValue(Element element, BeanDefinitionBuilder builder, String attributeName) {
		setPropertyValue(element, builder, attributeName, Conventions.attributeNameToPropertyName(attributeName));
	}

	static void setPropertyReference(Element element, BeanDefinitionBuilder builder, String attrName,
			String propertyName) {
		String attr = element.getAttribute(attrName);
		if (StringUtils.hasText(attr)) {
			builder.addPropertyReference(propertyName, attr);
		}
	}

	/**
	 * Utility for parsing bean aliases. Normally parsed by
	 * AbstractBeanDefinitionParser however due to the attribute clash (bean
	 * uses 'name' for aliases while region use it to indicate their name), the
	 * parser needs to handle this differently by storing them as metadata which
	 * gets deleted just before registration.
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
	 * 
	 * <pre>
	 *   <tag ref="someBean"/>
	 * </pre>
	 * 
	 * or
	 * 
	 * <pre>
	 *   <tag>
	 *     <bean .... />
	 *   </tag>
	 * </pre>
	 * 
	 * @param element
	 * @return
	 */
	static Object parseRefOrNestedBeanDeclaration(ParserContext parserContext, Element element,
			BeanDefinitionBuilder builder) {
		return parseRefOrNestedBeanDeclaration(parserContext, element, builder, "ref", false);
	}

	static Object getBeanReference(ParserContext parserContext, Element element) {
		return getBeanReference(parserContext, element, "ref");
	}

	static Object getBeanReference(ParserContext parserContext, Element element, String refAttrName) {
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
		else {
			return null;
		}
	}

	static Object parseRefOrNestedCustomElement(ParserContext parserContext, Element element,
			BeanDefinitionBuilder builder) {
		Object beanRef = ParsingUtils.getBeanReference(parserContext, element, "bean");
		if (beanRef != null) {
			return beanRef;
		}
		else {
			return parserContext.getDelegate().parseCustomElement(element, builder.getBeanDefinition());
		}
	}

	static Object parseRefOrSingleNestedBeanDeclaration(ParserContext parserContext, Element element,
			BeanDefinitionBuilder builder) {
		return parseRefOrNestedBeanDeclaration(parserContext, element, builder, "ref", true);
	}

	static Object parseRefOrNestedBeanDeclaration(ParserContext parserContext, Element element,
			BeanDefinitionBuilder builder, String refAttrName) {
		return parseRefOrNestedBeanDeclaration(parserContext, element, builder, refAttrName, false);
	}

	static Object parseRefOrNestedBeanDeclaration(ParserContext parserContext, Element element,
			BeanDefinitionBuilder builder, String refAttrName, boolean single) {
		Object beanRef = getBeanReference(parserContext, element, refAttrName);
		if (beanRef != null) {
			return beanRef;
		}

		// check nested declarations
		List<Element> childElements = DomUtils.getChildElements(element);

		// nested parse nested bean definition
		if (childElements.size() == 1) {
			return parserContext.getDelegate().parsePropertySubElement(childElements.get(0),
					builder.getRawBeanDefinition());
		}
		else {
			if (single) {
				parserContext.getReaderContext().error(
						"the element '" + element.getLocalName()
								+ "' does not support multiple nested bean definitions", element);
			}
		}

		ManagedList<Object> list = new ManagedList<Object>();

		for (Element el : childElements) {
			list.add(parserContext.getDelegate().parsePropertySubElement(el, builder.getRawBeanDefinition()));
		}

		return list;
	}

	/**
	 * Parses the eviction sub-element. Populates the given attribute factory
	 * with the proper attributes.
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

		BeanDefinitionBuilder evictionDefBuilder = BeanDefinitionBuilder
				.genericBeanDefinition(EvictionAttributesFactoryBean.class);

		// do manual conversion since the enum is not public
		String attr = evictionElement.getAttribute("type");
		if (StringUtils.hasText(attr)) {
			evictionDefBuilder.addPropertyValue("type", EvictionType.valueOf(attr.toUpperCase()));
		}

		setPropertyValue(evictionElement, evictionDefBuilder, "threshold");
		setPropertyValue(evictionElement, evictionDefBuilder, "action");

		// get object sizer (if declared)
		Element objectSizerElement = DomUtils.getChildElementByTagName(evictionElement, "object-sizer");

		if (objectSizerElement != null) {
			Object sizer = parseRefOrNestedBeanDeclaration(parserContext, objectSizerElement, evictionDefBuilder);
			evictionDefBuilder.addPropertyValue("ObjectSizer", sizer);
		}

		attrBuilder.addPropertyValue("evictionAttributes", evictionDefBuilder.getBeanDefinition());
		return true;
	}
	
	/**
	 * Parses the subscription sub-element. Populates the given attribute factory
	 * with the proper attributes.
	 * 
	 * @author Lyndon Adams
	 * @param parserContext
	 * @param element
	 * @param attrBuilder
	 * @return true if parsing actually occured, false otherwise
	 */
	static boolean parseSubscription(ParserContext parserContext, Element element, BeanDefinitionBuilder attrBuilder) {
		Element subscriptionElement = DomUtils.getChildElementByTagName(element, "subscription");

		if (subscriptionElement == null)
			return false;

		BeanDefinitionBuilder subscriptionDefBuilder = BeanDefinitionBuilder
				.genericBeanDefinition(SubscriptionAttributesFactoryBean.class);

		// do manual conversion since the enum is not public
		String attr = subscriptionElement.getAttribute("type");
		if (StringUtils.hasText(attr)) {
			subscriptionDefBuilder.addPropertyValue("type", SubscriptionType.valueOf(attr.toUpperCase()));
		}

		attrBuilder.addPropertyValue("subscriptionAttributes", subscriptionDefBuilder.getBeanDefinition());
		return true;
	}
	

	static void parseTransportFilters(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		Element transportFilterElement = DomUtils.getChildElementByTagName(element, "transport-filter");
		if (transportFilterElement != null) {
			builder.addPropertyValue("transportFilters",
					parseRefOrNestedBeanDeclaration(parserContext, transportFilterElement, builder));
		}
	}

	static void parseStatistics(Element element, BeanDefinitionBuilder attrBuilder) {
		setPropertyValue(element, attrBuilder, "statistics", "statisticsEnabled");
	}

	/**
	 * Parses the expiration sub-elements. Populates the given attribute factory
	 * with proper attributes.
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
		result |= parseCustomExpiration(parserContext, element,"custom-entry-ttl","customEntryTimeToLive",attrBuilder);
		result |= parseCustomExpiration(parserContext, element,"custom-entry-tti","customEntryIdleTimeout",attrBuilder);

		if (result) {
			// turn on statistics
			attrBuilder.addPropertyValue("statisticsEnabled", Boolean.TRUE);
		}
		return result;
	}

	static void parseOptionalRegionAttributes(ParserContext parserContext, Element element,
			BeanDefinitionBuilder attrBuilder) {
		if (!("partitioned-region".equals(element.getLocalName()))) {
			setPropertyValue(element, attrBuilder, "persistent", "persistBackup");
		}
		setPropertyValue(element, attrBuilder, "ignore-jta", "ignoreJTA");
		setPropertyValue(element, attrBuilder, "key-constraint");
		setPropertyValue(element, attrBuilder, "value-constraint");
		setPropertyValue(element, attrBuilder, "is-lock-grantor", "lockGrantor");
		setPropertyValue(element, attrBuilder, "enable-subscription-conflation");
		setPropertyValue(element, attrBuilder, "enable-async-conflation");
		setPropertyValue(element, attrBuilder, "initial-capacity");
		setPropertyValue(element, attrBuilder, "load-factor");
		setPropertyValue(element, attrBuilder, "cloning-enabled");
		setPropertyValue(element, attrBuilder, "concurrency-level");
		setPropertyValue(element, attrBuilder, "multicast-enabled");

		String indexUpdateType = element.getAttribute("index-update-type");
		if (StringUtils.hasText(indexUpdateType)) {
			attrBuilder.addPropertyValue("indexMaintenanceSynchronous", "synchronous".equals(indexUpdateType));
		}
		
		String concurrencyChecksEnabled = element.getAttribute("concurrency-checks-enabled");
		if (StringUtils.hasText(concurrencyChecksEnabled)) {
			if (!ParsingUtils.isGemfireV7OrAbove()) {
				log.warn("'concurrency-checks-enabled' is only available in Gemfire 7.0 or above");
			} else {
				ParsingUtils.setPropertyValue(element, attrBuilder, "concurrency-checks-enabled");
			}
		}
		
		
	}

	static void parseMembershipAttributes(ParserContext parserContext, Element element,
			BeanDefinitionBuilder attrBuilder) {
		Element membershipAttributes = DomUtils.getChildElementByTagName(element, "membership-attributes");
		if (membershipAttributes != null) {
			String requiredRoles[] = StringUtils.commaDelimitedListToStringArray(membershipAttributes
					.getAttribute("required-roles"));
			String lossActionAttr = membershipAttributes.getAttribute("loss-action");
			LossAction lossAction = StringUtils.hasText(lossActionAttr) ? LossAction.fromName(lossActionAttr
					.toUpperCase().replace("-", "_")) : null;
			String resumptionActionAttr = membershipAttributes.getAttribute("resumption-action");
			ResumptionAction resumptionAction = StringUtils.hasText(resumptionActionAttr) ? ResumptionAction
					.fromName(resumptionActionAttr.toUpperCase().replace("-", "_")) : null;

			ManagedArray requiredRolesArray = new ManagedArray("java.lang.String", requiredRoles.length);
			for (int i = 0; i < requiredRoles.length; i++) {
				requiredRolesArray.add(requiredRoles[i]);
			}
			BeanDefinitionBuilder membershipAttributesBuilder = BeanDefinitionBuilder
					.genericBeanDefinition(MembershipAttributes.class);
			membershipAttributesBuilder.addConstructorArgValue(requiredRolesArray);
			membershipAttributesBuilder.addConstructorArgValue(lossAction);
			membershipAttributesBuilder.addConstructorArgValue(resumptionAction);

			attrBuilder.addPropertyValue("membershipAttributes", membershipAttributesBuilder.getRawBeanDefinition());
		}
	}

	static void throwExceptionIfNotGemfireV7(String elementName, String attributeName, ParserContext parserContext) {
		if (!isGemfireV7OrAbove()) {
			String messagePrefix = (attributeName == null) ? "element '" + elementName + "'" : "attribute '"
					+ attributeName + " of element '" + elementName + "'";
			parserContext.getReaderContext().error(
					messagePrefix + " requires Gemfire version 7 or later. The current version is " + GEMFIRE_VERSION,
					null);
		}
	}
	
	static boolean isGemfireV7OrAbove() {
		
		int version = Integer.parseInt(GEMFIRE_VERSION.substring(0,1));
		return version >= 7;
		
	}

	static void parseScope(Element element, BeanDefinitionBuilder builder) {
		String scope = element.getAttribute("scope");
		if (StringUtils.hasText(scope)) {
			builder.addPropertyValue("scope", Scope.fromString(scope.toUpperCase().replace("-", "_")));
		}
		else {
			builder.addPropertyValue("scope", Scope.DISTRIBUTED_ACK);
		}
	}

	private static boolean parseExpiration(Element rootElement, String elementName, String propertyName,
			BeanDefinitionBuilder attrBuilder) {
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
		BeanDefinitionBuilder expirationAttributes = BeanDefinitionBuilder
				.genericBeanDefinition(ExpirationAttributes.class);
		expirationAttributes.addConstructorArgValue(expirationTime);
		expirationAttributes.addConstructorArgValue(action);
		attrBuilder.addPropertyValue(propertyName, expirationAttributes.getBeanDefinition());

		return true;
	}
	
	private static boolean parseCustomExpiration(ParserContext parserContext, Element rootElement, String elementName, String propertyName,
			BeanDefinitionBuilder attrBuilder) {
		Element expirationElement = DomUtils.getChildElementByTagName(rootElement, elementName);

		if (expirationElement == null)
			return false;
		
		Object customExpiry = parseRefOrSingleNestedBeanDeclaration(parserContext, expirationElement, attrBuilder);

		attrBuilder.addPropertyValue(propertyName, customExpiry);

		return true;
	}
}