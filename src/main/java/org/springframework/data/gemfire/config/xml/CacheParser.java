/*
 * Copyright 2010-2020 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.xml;

import java.util.List;
import java.util.Optional;

import org.apache.geode.internal.datasource.ConfigProperty;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.support.ManagedMap;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.config.support.CustomEditorBeanFactoryPostProcessor;
import org.springframework.data.gemfire.config.support.GemfireFeature;
import org.springframework.data.gemfire.config.support.PdxDiskStoreAwareBeanFactoryPostProcessor;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;

/**
 * Bean definition parser for the &lt;gfe:cache&gt; SDG XML namespace (XSD) element.
 *
 * @author Costin Leau
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser
 * @see org.springframework.beans.factory.xml.ParserContext
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.w3c.dom.Element
 */
class CacheParser extends AbstractSingleBeanDefinitionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getBeanClass(Element element) {
		return CacheFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder cacheBuilder) {

		super.doParse(element, cacheBuilder);

		registerGemFireBeanFactoryPostProcessors(getRegistry(parserContext));

		ParsingUtils.setPropertyValue(element, cacheBuilder, "cache-xml-location", "cacheXml");
		ParsingUtils.setPropertyReference(element, cacheBuilder, "properties-ref", "properties");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "use-bean-factory-locator");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "close");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "copy-on-read");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "critical-heap-percentage");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "critical-off-heap-percentage");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "eviction-heap-percentage");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "eviction-off-heap-percentage");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "enable-auto-reconnect");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "lock-lease");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "lock-timeout");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "message-sync-interval");
		parsePdxDiskStore(element, parserContext, cacheBuilder);
		ParsingUtils.setPropertyValue(element, cacheBuilder, "pdx-ignore-unread-fields");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "pdx-read-serialized");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "pdx-persistent");
		ParsingUtils.setPropertyReference(element, cacheBuilder, "pdx-serializer-ref", "pdxSerializer");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "search-timeout");
		ParsingUtils.setPropertyValue(element, cacheBuilder, "use-cluster-configuration");

		List<Element> txListeners = DomUtils.getChildElementsByTagName(element, "transaction-listener");

		if (!CollectionUtils.isEmpty(txListeners)) {

			ManagedList<Object> transactionListeners = new ManagedList<>();

			for (Element txListener : txListeners) {
				transactionListeners.add(ParsingUtils.parseRefOrNestedBeanDeclaration(
					txListener, parserContext, cacheBuilder));
			}

			cacheBuilder.addPropertyValue("transactionListeners", transactionListeners);
		}

		Element txWriter = DomUtils.getChildElementByTagName(element, "transaction-writer");

		if (txWriter != null) {
			cacheBuilder.addPropertyValue("transactionWriter",
				ParsingUtils.parseRefOrNestedBeanDeclaration(txWriter, parserContext, cacheBuilder));
		}

		Element gatewayConflictResolver =
			DomUtils.getChildElementByTagName(element, "gateway-conflict-resolver");

		if (gatewayConflictResolver != null) {
			ParsingUtils.throwExceptionWhenGemFireFeatureUnavailable(GemfireFeature.WAN, element.getLocalName(),
				"gateway-conflict-resolver", parserContext);
			cacheBuilder.addPropertyValue("gatewayConflictResolver", ParsingUtils.parseRefOrSingleNestedBeanDeclaration(
				gatewayConflictResolver, parserContext, cacheBuilder));
		}

		parseDynamicRegionFactory(element, cacheBuilder);
		parseJndiBindings(element, cacheBuilder);
	}

	protected BeanDefinitionRegistry getRegistry(ParserContext parserContext) {
		return parserContext.getRegistry();
	}

	private void registerGemFireBeanFactoryPostProcessors(BeanDefinitionRegistry registry) {

		BeanDefinitionReaderUtils.registerWithGeneratedName(
			BeanDefinitionBuilder.genericBeanDefinition(CustomEditorBeanFactoryPostProcessor.class)
				.getBeanDefinition(), registry);
	}

	private void parsePdxDiskStore(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {

		ParsingUtils.setPropertyValue(element, builder, "pdx-disk-store", "pdxDiskStoreName");

		String pdxDiskStoreName = element.getAttribute("pdx-disk-store");

		if (!StringUtils.isEmpty(pdxDiskStoreName)) {
			registerPdxDiskStoreAwareBeanFactoryPostProcessor(getRegistry(parserContext), pdxDiskStoreName);
		}
	}

	private void registerPdxDiskStoreAwareBeanFactoryPostProcessor(BeanDefinitionRegistry registry,
			String pdxDiskStoreName) {

		BeanDefinitionReaderUtils.registerWithGeneratedName(
			createPdxDiskStoreAwareBeanFactoryPostProcessorBeanDefinition(pdxDiskStoreName), registry);
	}

	private AbstractBeanDefinition createPdxDiskStoreAwareBeanFactoryPostProcessorBeanDefinition(
			String pdxDiskStoreName) {

		BeanDefinitionBuilder builder =
			BeanDefinitionBuilder.genericBeanDefinition(PdxDiskStoreAwareBeanFactoryPostProcessor.class);

		builder.addConstructorArgValue(pdxDiskStoreName);

		return builder.getBeanDefinition();
	}

	private void parseDynamicRegionFactory(Element element, BeanDefinitionBuilder cacheBuilder) {

		Element dynamicRegionFactory =
			DomUtils.getChildElementByTagName(element, "dynamic-region-factory");

		if (dynamicRegionFactory != null) {

			BeanDefinitionBuilder dynamicRegionSupport = buildDynamicRegionSupport(dynamicRegionFactory);

			postProcessDynamicRegionSupport(element, dynamicRegionSupport);
			cacheBuilder.addPropertyValue("dynamicRegionSupport", dynamicRegionSupport.getBeanDefinition());
		}
	}

	private BeanDefinitionBuilder buildDynamicRegionSupport(Element dynamicRegionFactory) {

		if (dynamicRegionFactory != null) {

			BeanDefinitionBuilder dynamicRegionSupport =
				BeanDefinitionBuilder.genericBeanDefinition(CacheFactoryBean.DynamicRegionSupport.class);

			String diskDirectory = dynamicRegionFactory.getAttribute("disk-dir");

			if (StringUtils.hasText(diskDirectory)) {
				dynamicRegionSupport.addPropertyValue("diskDir", diskDirectory);
			}

			String persistent = dynamicRegionFactory.getAttribute("persistent");

			if (StringUtils.hasText(persistent)) {
				dynamicRegionSupport.addPropertyValue("persistent", persistent);
			}

			String registerInterest = dynamicRegionFactory.getAttribute("register-interest");

			if (StringUtils.hasText(registerInterest)) {
				dynamicRegionSupport.addPropertyValue("registerInterest", registerInterest);
			}

			return dynamicRegionSupport;
		}

		return null;
	}

	/**
	 * @param dynamicRegionSupport {@link BeanDefinitionBuilder} for &lt;gfe:dynamic-region-factory&gt; element.
	 */
	protected void postProcessDynamicRegionSupport(Element element, BeanDefinitionBuilder dynamicRegionSupport) { }

	private void parseJndiBindings(Element element, BeanDefinitionBuilder cacheBuilder) {

		List<Element> jndiBindings = DomUtils.getChildElementsByTagName(element, "jndi-binding");

		if (!CollectionUtils.isEmpty(jndiBindings)) {

			ManagedList<Object> jndiDataSources = new ManagedList<>(jndiBindings.size());

			for (Element jndiBinding : jndiBindings) {

				BeanDefinitionBuilder jndiDataSource = BeanDefinitionBuilder.genericBeanDefinition(
					CacheFactoryBean.JndiDataSource.class);

				// NOTE 'jndi-name' and 'type' are required by the XSD so we should have at least 2 attributes.
				NamedNodeMap attributes = jndiBinding.getAttributes();

				ManagedMap<String, String> jndiAttributes = new ManagedMap<>(attributes.getLength());

				for (int index = 0, length = attributes.getLength(); index < length; index++) {
					Attr attribute = (Attr) attributes.item(index);
					jndiAttributes.put(attribute.getLocalName(), attribute.getValue());
				}

				jndiDataSource.addPropertyValue("attributes", jndiAttributes);

				List<Element> jndiProps = DomUtils.getChildElementsByTagName(jndiBinding, "jndi-prop");

				if (!CollectionUtils.isEmpty(jndiProps)) {

					ManagedList<Object> props = new ManagedList<>(jndiProps.size());

					for (Element jndiProp : jndiProps) {

						String key = jndiProp.getAttribute("key");
						String type = jndiProp.getAttribute("type");
						String value = jndiProp.getTextContent();

						type = (StringUtils.hasText(type) ? type : String.class.getName());

						props.add(BeanDefinitionBuilder.genericBeanDefinition(ConfigProperty.class)
							.addConstructorArgValue(key)
							.addConstructorArgValue(value)
							.addConstructorArgValue(type)
							.getBeanDefinition());
					}

					jndiDataSource.addPropertyValue("props", props);
				}

				jndiDataSources.add(jndiDataSource.getBeanDefinition());
			}

			cacheBuilder.addPropertyValue("jndiDataSources", jndiDataSources);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected String resolveId(Element element, AbstractBeanDefinition definition, ParserContext parserContext)
		throws BeanDefinitionStoreException {

		String name = Optional.of(super.resolveId(element, definition, parserContext))
			.filter(StringUtils::hasText)
			.map(StringUtils::trimWhitespace)
			.orElse(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

		if (!"gemfire-cache".equals(name)) {
			parserContext.getRegistry().registerAlias(name, "gemfire-cache");
		}

		return name;
	}
}
