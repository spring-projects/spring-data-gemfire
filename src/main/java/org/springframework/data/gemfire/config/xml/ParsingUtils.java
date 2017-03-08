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

package org.springframework.data.gemfire.config.xml;

import java.util.List;

import org.apache.geode.cache.LossAction;
import org.apache.geode.cache.MembershipAttributes;
import org.apache.geode.cache.ResumptionAction;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.core.Conventions;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.SubscriptionAttributesFactoryBean;
import org.springframework.data.gemfire.config.support.GemfireFeature;
import org.springframework.data.gemfire.eviction.EvictionAttributesFactoryBean;
import org.springframework.data.gemfire.expiration.ExpirationAttributesFactoryBean;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Utilities used by the Spring Data GemFire XML Namespace Parsers.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author Lyndon Adams
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.config.RuntimeBeanReference
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.ManagedList
 * @see org.springframework.beans.factory.xml.ParserContext
 * @see org.springframework.core.Conventions
 * @see org.springframework.util.xml.DomUtils
 * @see org.w3c.dom.Element
 * @since 1.0.0
 */
abstract class ParsingUtils {

	protected static final String CACHE_PROPERTY_NAME = "cache";
	protected static final String REGION_PROPERTY_NAME = "region";
	protected static final String CACHE_REF_ATTRIBUTE_NAME = "cache-ref";
	protected static final String REGION_REF_ATTRIBUTE_NAME = "region-ref";

	static void setPropertyReference(Element element, BeanDefinitionBuilder builder, String attributeName,
			String propertyName) {

		String attributeValue = element.getAttribute(attributeName);

		if (StringUtils.hasText(attributeValue)) {
			builder.addPropertyReference(propertyName, attributeValue);
		}
	}

	@SuppressWarnings("unused")
	static void setPropertyReference(Element element, BeanDefinitionBuilder builder, String attributeName) {
		setPropertyReference(element, builder, attributeName, Conventions.attributeNameToPropertyName(attributeName));
	}

	static void setPropertyValue(Element element, BeanDefinitionBuilder builder, String attributeName,
			String propertyName, Object defaultValue) {

		String attributeValue = element.getAttribute(attributeName);

		if (StringUtils.hasText(attributeValue)) {
			builder.addPropertyValue(propertyName, attributeValue);
		}
		else if (defaultValue != null) {
			builder.addPropertyValue(propertyName, defaultValue);
		}
	}

	static void setPropertyValue(Element element, BeanDefinitionBuilder builder, String attributeName,
			String propertyName) {
		setPropertyValue(element, builder, attributeName, propertyName, null);
	}

	static void setPropertyValue(Element element, BeanDefinitionBuilder builder, String attributeName) {
		setPropertyValue(element, builder, attributeName, Conventions.attributeNameToPropertyName(attributeName));
	}

	static void setPropertyValue(BeanDefinitionBuilder builder, BeanDefinition source, String propertyName,
			boolean withDependsOn) {

		PropertyValue propertyValue = source.getPropertyValues().getPropertyValue(propertyName);

		if (propertyValue != null) {
			builder.addPropertyValue(propertyValue.getName(), propertyValue.getValue());

			if (withDependsOn && propertyValue.getValue() instanceof RuntimeBeanReference) {
				builder.addDependsOn(((RuntimeBeanReference) propertyValue.getValue()).getBeanName());
			}
		}
	}

	static void setPropertyValue(BeanDefinitionBuilder builder, BeanDefinition source, String propertyName) {
		setPropertyValue(builder, source, propertyName, false);
	}

	static Object getBeanReference(Element element, ParserContext parserContext, String refAttributeName) {

		String refAttributeValue = element.getAttribute(refAttributeName);
		Object returnValue = null;

		if (StringUtils.hasText(refAttributeValue)) {
			if (!DomUtils.getChildElements(element).isEmpty()) {
				parserContext.getReaderContext().error(String.format(
					"Use either the '%1$s' attribute or a nested bean declaration for '%2$s' element, but not both.",
						refAttributeName, element.getLocalName()), element);
			}

			returnValue = new RuntimeBeanReference(refAttributeValue);
		}

		return returnValue;
	}

	static Object parseRefOrNestedCustomElement(Element element, ParserContext parserContext,
			BeanDefinitionBuilder builder) {

		Object beanRef = ParsingUtils.getBeanReference(element, parserContext, "bean");

		return (beanRef != null ? beanRef : parserContext.getDelegate().parseCustomElement(
			element, builder.getBeanDefinition()));
	}

	/**
	 * Utility method handling parsing of nested bean definition of the type:
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
	 * @param element the XML element.
	 * @return Bean reference or nested Bean definition.
	 */
	static Object parseRefOrNestedBeanDeclaration(Element element, ParserContext parserContext,
			BeanDefinitionBuilder builder) {

		return parseRefOrNestedBeanDeclaration(element, parserContext, builder, "ref", false);
	}

	static Object parseRefOrNestedBeanDeclaration(Element element, ParserContext parserContext,
			BeanDefinitionBuilder builder, String refAttributeName) {

		return parseRefOrNestedBeanDeclaration(element, parserContext, builder, refAttributeName, false);
	}

	static Object parseRefOrSingleNestedBeanDeclaration(Element element, ParserContext parserContext,
			BeanDefinitionBuilder builder) {

		return parseRefOrNestedBeanDeclaration(element, parserContext, builder, "ref", true);
	}

	static Object parseRefOrSingleNestedBeanDeclaration(Element element, ParserContext parserContext,
			BeanDefinitionBuilder builder, String refAttributeName) {

		return parseRefOrNestedBeanDeclaration(element, parserContext, builder, refAttributeName, true);
	}

	static Object parseRefOrNestedBeanDeclaration(Element element, ParserContext parserContext,
			BeanDefinitionBuilder builder, String refAttributeName, boolean single) {

		Object beanReference = getBeanReference(element, parserContext, refAttributeName);

		if (beanReference != null) {
			return beanReference;
		}

		// check nested declarations
		List<Element> childElements = DomUtils.getChildElements(element);

		// parse nested bean definition
		if (childElements.size() == 1) {
			return parserContext.getDelegate().parsePropertySubElement(
				childElements.get(0), builder.getRawBeanDefinition());
		}
		else {
			// TODO also triggered when there are no child elements; need to change the message...
			if (single) {
				parserContext.getReaderContext().error(String.format(
					"The element '%1$s' does not support multiple nested bean definitions.",
						element.getLocalName()), element);
			}
		}

		ManagedList<Object> list = new ManagedList<>();

		for (Element childElement : childElements) {
			list.add(parserContext.getDelegate().parsePropertySubElement(childElement, builder.getRawBeanDefinition()));
		}

		return list;
	}

	/**
	 * Parses the eviction sub-element. Populates the given attribute factory with the proper attributes.
	 *
	 * @param element the XML elements being parsed.
	 * @param parserContext the context used for parsing the XML document.
	 * @param regionAttributesBuilder the Region Attributes builder.
	 * @return true if parsing actually occurred, false otherwise.
	 */
	static boolean parseEviction(Element element, ParserContext parserContext,
			BeanDefinitionBuilder regionAttributesBuilder) {

		Element evictionElement = DomUtils.getChildElementByTagName(element, "eviction");

		if (evictionElement != null) {
			BeanDefinitionBuilder evictionAttributesBuilder = BeanDefinitionBuilder.genericBeanDefinition(
				EvictionAttributesFactoryBean.class);

			setPropertyValue(evictionElement, evictionAttributesBuilder, "action");
			setPropertyValue(evictionElement, evictionAttributesBuilder, "threshold");
			setPropertyValue(evictionElement, evictionAttributesBuilder, "type");

			Element objectSizerElement = DomUtils.getChildElementByTagName(evictionElement, "object-sizer");

			if (objectSizerElement != null) {
				Object sizer = parseRefOrNestedBeanDeclaration(objectSizerElement, parserContext,
					evictionAttributesBuilder);
				evictionAttributesBuilder.addPropertyValue("objectSizer", sizer);
			}

			regionAttributesBuilder.addPropertyValue("evictionAttributes",
				evictionAttributesBuilder.getBeanDefinition());

			return true;
		}

	    return false;
	}

	/**
	 * Parses the subscription sub-element. Populates the given attribute factory with the proper attributes.
	 *
	 * @param element the XML element being parsed.
	 * @param parserContext the context used while parsing the XML document.
	 * @param regionAttributesBuilder the Region Attributes builder.
	 * @return true if parsing actually occurred, false otherwise.
	 */
	@SuppressWarnings("unused")
	static boolean parseSubscription(Element element, ParserContext parserContext,
		BeanDefinitionBuilder regionAttributesBuilder) {

		Element subscriptionElement = DomUtils.getChildElementByTagName(element, "subscription");

		if (subscriptionElement != null) {
			BeanDefinitionBuilder subscriptionAttributesBuilder = BeanDefinitionBuilder.genericBeanDefinition(
				SubscriptionAttributesFactoryBean.class);

			setPropertyValue(subscriptionElement, subscriptionAttributesBuilder, "type", "interestPolicy");

			regionAttributesBuilder.addPropertyValue("subscriptionAttributes",
				subscriptionAttributesBuilder.getBeanDefinition());

			return true;
		}

		return false;
	}

	static void parseTransportFilters(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		Element transportFilterElement = DomUtils.getChildElementByTagName(element, "transport-filter");

		if (transportFilterElement != null) {
			builder.addPropertyValue("transportFilters", parseRefOrNestedBeanDeclaration(transportFilterElement,
				parserContext,
				builder));
		}
	}

	static void parseStatistics(Element element, BeanDefinitionBuilder regionAttributesBuilder) {
		setPropertyValue(element, regionAttributesBuilder, "statistics", "statisticsEnabled");
	}

	/**
	 * Parses the expiration sub-elements. Populates the given attribute factory with proper attributes.
	 *
	 * @param element the XML element being parsed.
	 * @param parserContext the context used while parsing the XML document.
	 * @param regionAttributesBuilder the Region Attributes builder.
	 * @return a boolean indicating whether Region expiration attributes were specified.
	 */
	static boolean parseExpiration(Element element, ParserContext parserContext,
		BeanDefinitionBuilder regionAttributesBuilder) {

		boolean result = parseExpiration(element, "region-ttl", "regionTimeToLive", regionAttributesBuilder);

		result |= parseExpiration(element, "region-tti", "regionIdleTimeout", regionAttributesBuilder);
		result |= parseExpiration(element, "entry-ttl", "entryTimeToLive", regionAttributesBuilder);
		result |= parseExpiration(element, "entry-tti", "entryIdleTimeout", regionAttributesBuilder);
		result |= parseCustomExpiration(element, parserContext, "custom-entry-ttl", "customEntryTimeToLive",
			regionAttributesBuilder);
		result |= parseCustomExpiration(element, parserContext, "custom-entry-tti", "customEntryIdleTimeout",
			regionAttributesBuilder);

		if (result) {
			// turn on statistics
			regionAttributesBuilder.addPropertyValue("statisticsEnabled", Boolean.TRUE);
		}

		return result;
	}

	@SuppressWarnings("unused")
	static void parseOptionalRegionAttributes(Element element, ParserContext parserContext,
		BeanDefinitionBuilder regionAttributesBuilder) {

		setPropertyValue(element, regionAttributesBuilder, "cloning-enabled");
		setPropertyValue(element, regionAttributesBuilder, "concurrency-level");
		setPropertyValue(element, regionAttributesBuilder, "disk-synchronous");
		setPropertyValue(element, regionAttributesBuilder, "enable-async-conflation");
		setPropertyValue(element, regionAttributesBuilder, "enable-subscription-conflation");
		setPropertyValue(element, regionAttributesBuilder, "ignore-jta", "ignoreJTA");
		setPropertyValue(element, regionAttributesBuilder, "index-update-type");
		setPropertyValue(element, regionAttributesBuilder, "initial-capacity");
		setPropertyValue(element, regionAttributesBuilder, "is-lock-grantor", "lockGrantor");
		setPropertyValue(element, regionAttributesBuilder, "key-constraint");
		setPropertyValue(element, regionAttributesBuilder, "load-factor");
		setPropertyValue(element, regionAttributesBuilder, "multicast-enabled");
		setPropertyValue(element, regionAttributesBuilder, "off-heap");
		setPropertyValue(element, regionAttributesBuilder, "publisher");
		setPropertyValue(element, regionAttributesBuilder, "value-constraint");

		String concurrencyChecksEnabled = element.getAttribute("concurrency-checks-enabled");

		if (StringUtils.hasText(concurrencyChecksEnabled)) {
			ParsingUtils.setPropertyValue(element, regionAttributesBuilder, "concurrency-checks-enabled");
		}
	}

	@SuppressWarnings({ "deprecation", "unused" })
	static void parseMembershipAttributes(Element element, ParserContext parserContext,
		BeanDefinitionBuilder regionAttributesBuilder) {

		Element membershipAttributes = DomUtils.getChildElementByTagName(element, "membership-attributes");

		if (membershipAttributes != null) {
			String[] requiredRoles = StringUtils.commaDelimitedListToStringArray(
				membershipAttributes.getAttribute("required-roles"));

			String lossActionValue = membershipAttributes.getAttribute("loss-action");

			LossAction lossAction = (StringUtils.hasText(lossActionValue)
				? LossAction.fromName(lossActionValue.toUpperCase().replace("-", "_"))
				: LossAction.NO_ACCESS);

			String resumptionActionValue = membershipAttributes.getAttribute("resumption-action");

			ResumptionAction resumptionAction = (StringUtils.hasText(resumptionActionValue)
				? ResumptionAction.fromName(resumptionActionValue.toUpperCase().replace("-", "_"))
				: ResumptionAction.REINITIALIZE);

			regionAttributesBuilder.addPropertyValue("membershipAttributes",
				new MembershipAttributes(requiredRoles, lossAction, resumptionAction));
		}
	}

	static void parseScope(Element element, BeanDefinitionBuilder builder) {
		String scopeAttributeValue = element.getAttribute("scope");

		if (StringUtils.hasText(scopeAttributeValue)) {
			builder.addPropertyValue("scope", scopeAttributeValue);
		}
	}

	private static boolean parseExpiration(Element rootElement, String elementName, String propertyName,
			BeanDefinitionBuilder regionAttributesBuilder) {

		Element expirationElement = DomUtils.getChildElementByTagName(rootElement, elementName);

		if (expirationElement != null) {
			BeanDefinitionBuilder expirationAttributesBuilder = BeanDefinitionBuilder.genericBeanDefinition(
				ExpirationAttributesFactoryBean.class);

			setPropertyValue(expirationElement, expirationAttributesBuilder, "action");
			setPropertyValue(expirationElement, expirationAttributesBuilder, "timeout");
			regionAttributesBuilder.addPropertyValue(propertyName, expirationAttributesBuilder.getBeanDefinition());

			return true;
		}

		return false;
	}

	private static boolean parseCustomExpiration(Element rootElement, ParserContext parserContext, String elementName,
			String propertyName, BeanDefinitionBuilder regionAttributesBuilder) {

		Element expirationElement = DomUtils.getChildElementByTagName(rootElement, elementName);

		if (expirationElement != null) {
			Object customExpiry =
				parseRefOrSingleNestedBeanDeclaration(expirationElement, parserContext, regionAttributesBuilder);

			regionAttributesBuilder.addPropertyValue(propertyName, customExpiry);

			return true;
		}

		return false;
	}

	static void parseCompressor(Element element, ParserContext parserContext,
			BeanDefinitionBuilder regionAttributesBuilder) {

		Element compressorElement = DomUtils.getChildElementByTagName(element, "compressor");

		if (compressorElement != null) {
			regionAttributesBuilder.addPropertyValue("compressor", parseRefOrSingleNestedBeanDeclaration(
				compressorElement, parserContext, regionAttributesBuilder));
		}
	}

	@SuppressWarnings("unused")
	static void assertGemFireFeatureAvailable(Element element, ParserContext parserContext) {
		if (GemfireUtils.isGemfireFeatureUnavailable(element)) {
			parserContext.getReaderContext().error(String.format("'%1$s' is not supported in %2$s v%3$s",
				element.getLocalName(), GemfireUtils.GEMFIRE_NAME, GemfireUtils.GEMFIRE_VERSION), element);
		}
	}

	static void setCacheReference(Element element, BeanDefinitionBuilder builder) {
		builder.addPropertyReference(CACHE_PROPERTY_NAME, resolveCacheReference(element));
	}

	static String resolveCacheReference(Element element) {
		return resolveCacheReference(element.getAttribute(CACHE_REF_ATTRIBUTE_NAME));
	}

	static String resolveCacheReference(String cacheReference) {
		return SpringUtils.defaultIfEmpty(cacheReference, GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);
	}

	static void setRegionReference(Element element, BeanDefinitionBuilder builder) {
		builder.addPropertyReference(REGION_PROPERTY_NAME, resolveRegionReference(element));
	}

	static String resolveRegionReference(Element element) {
		return element.getAttribute(REGION_REF_ATTRIBUTE_NAME);
	}

	static void throwExceptionWhenGemFireFeatureUnavailable(GemfireFeature feature,
			String elementName, String attributeName, ParserContext parserContext) {

		if (GemfireUtils.isGemfireFeatureUnavailable(feature)) {
			String messagePrefix = (attributeName != null)
				? String.format("Attribute '%1$s' of element '%2$s'", attributeName, elementName)
				: String.format("Element '%1$s'", elementName);
			parserContext.getReaderContext().error(
				String.format("%1$s requires GemFire version 7 or later. The current version is %2$s.",
					messagePrefix, GemfireUtils.GEMFIRE_VERSION), null);
		}
	}
}
