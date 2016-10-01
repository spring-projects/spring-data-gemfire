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

import com.gemstone.gemfire.internal.datasource.ConfigProperty;

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
import org.springframework.data.gemfire.config.support.PdxDiskStoreAwareBeanFactoryPostProcessor;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

/**
 * Bean definition parser for the &lt;gfe:cache&gt; SDG XML namespace (XSD) element.
 *
 * @author Costin Leau
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser
 * @see org.springframework.data.gemfire.CacheFactoryBean
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
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		super.doParse(element, builder);

		registerGemFireBeanFactoryPostProcessors(getRegistry(parserContext));

		ParsingUtils.setPropertyValue(element, builder, "cache-xml-location", "cacheXml");
		ParsingUtils.setPropertyReference(element, builder, "properties-ref", "properties");
		ParsingUtils.setPropertyValue(element, builder, "use-bean-factory-locator");
		ParsingUtils.setPropertyValue(element, builder, "close");
		ParsingUtils.setPropertyValue(element, builder, "copy-on-read");
		ParsingUtils.setPropertyValue(element, builder, "critical-heap-percentage");
		ParsingUtils.setPropertyValue(element, builder, "eviction-heap-percentage");
		ParsingUtils.setPropertyValue(element, builder, "enable-auto-reconnect");
		ParsingUtils.setPropertyValue(element, builder, "lock-lease");
		ParsingUtils.setPropertyValue(element, builder, "lock-timeout");
		ParsingUtils.setPropertyValue(element, builder, "message-sync-interval");
		parsePdxDiskStore(element, parserContext, builder);
		ParsingUtils.setPropertyValue(element, builder, "pdx-ignore-unread-fields");
		ParsingUtils.setPropertyValue(element, builder, "pdx-read-serialized");
		ParsingUtils.setPropertyValue(element, builder, "pdx-persistent");
		ParsingUtils.setPropertyReference(element, builder, "pdx-serializer-ref", "pdxSerializer");
		ParsingUtils.setPropertyValue(element, builder, "search-timeout");
		ParsingUtils.setPropertyValue(element, builder, "use-cluster-configuration");

		List<Element> txListeners = DomUtils.getChildElementsByTagName(element, "transaction-listener");

		if (!CollectionUtils.isEmpty(txListeners)) {
			ManagedList<Object> transactionListeners = new ManagedList<Object>();

			for (Element txListener : txListeners) {
				transactionListeners.add(ParsingUtils.parseRefOrNestedBeanDeclaration(
					parserContext, txListener, builder));
			}

			builder.addPropertyValue("transactionListeners", transactionListeners);
		}

		Element txWriter = DomUtils.getChildElementByTagName(element, "transaction-writer");

		if (txWriter != null) {
			builder.addPropertyValue("transactionWriter", ParsingUtils.parseRefOrNestedBeanDeclaration(
				parserContext, txWriter, builder));
		}

		Element gatewayConflictResolver = DomUtils.getChildElementByTagName(element, "gateway-conflict-resolver");

		if (gatewayConflictResolver != null) {
			ParsingUtils.throwExceptionIfNotGemfireV7(element.getLocalName(), "gateway-conflict-resolver", parserContext);
			builder.addPropertyValue("gatewayConflictResolver", ParsingUtils.parseRefOrSingleNestedBeanDeclaration(
				parserContext, gatewayConflictResolver, builder));
		}

		parseDynamicRegionFactory(element, builder);
		parseJndiBindings(element, builder);
	}

	/* (non-Javadoc) */
	protected BeanDefinitionRegistry getRegistry(ParserContext parserContext) {
		return parserContext.getRegistry();
	}

	/* (non-Javadoc) */
	void registerGemFireBeanFactoryPostProcessors(BeanDefinitionRegistry registry) {
		BeanDefinitionReaderUtils.registerWithGeneratedName(BeanDefinitionBuilder.genericBeanDefinition(
			CustomEditorBeanFactoryPostProcessor.class).getBeanDefinition(), registry);
	}

	/* (non-Javadoc) */
	private void parsePdxDiskStore(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		ParsingUtils.setPropertyValue(element, builder, "pdx-disk-store", "pdxDiskStoreName");

		final String pdxDiskStoreName = element.getAttribute("pdx-disk-store");

		if (!StringUtils.isEmpty(pdxDiskStoreName)) {
			registerPdxDiskStoreAwareBeanFactoryPostProcessor(getRegistry(parserContext), pdxDiskStoreName);
		}
	}

	/* (non-Javadoc) */
	void registerPdxDiskStoreAwareBeanFactoryPostProcessor(BeanDefinitionRegistry registry, String pdxDiskStoreName) {
		BeanDefinitionReaderUtils.registerWithGeneratedName(
			createPdxDiskStoreAwareBeanFactoryPostProcessorBeanDefinition(pdxDiskStoreName), registry);
	}

	/* (non-Javadoc) */
	private AbstractBeanDefinition createPdxDiskStoreAwareBeanFactoryPostProcessorBeanDefinition(String pdxDiskStoreName) {
		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(
			PdxDiskStoreAwareBeanFactoryPostProcessor.class);
		builder.addConstructorArgValue(pdxDiskStoreName);
		return builder.getBeanDefinition();
	}

	/* (non-Javadoc) */
	private void parseDynamicRegionFactory(Element element, BeanDefinitionBuilder builder) {
		Element dynamicRegionFactory = DomUtils.getChildElementByTagName(element, "dynamic-region-factory");

		if (dynamicRegionFactory != null) {
			BeanDefinitionBuilder dynamicRegionSupport = buildDynamicRegionSupport(dynamicRegionFactory);
			postProcessDynamicRegionSupport(element, dynamicRegionSupport);
			builder.addPropertyValue("dynamicRegionSupport", dynamicRegionSupport.getBeanDefinition());
		}
	}

	/* (non-Javadoc) */
	private BeanDefinitionBuilder buildDynamicRegionSupport(Element dynamicRegionFactory) {
		if (dynamicRegionFactory != null) {
			BeanDefinitionBuilder dynamicRegionSupport = BeanDefinitionBuilder.genericBeanDefinition(
				CacheFactoryBean.DynamicRegionSupport.class);

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
	protected void postProcessDynamicRegionSupport(Element element, BeanDefinitionBuilder dynamicRegionSupport) {
	}

	/* (non-Javadoc) */
	private void parseJndiBindings(Element element, BeanDefinitionBuilder builder) {
		List<Element> jndiBindings = DomUtils.getChildElementsByTagName(element, "jndi-binding");

		if (!CollectionUtils.isEmpty(jndiBindings)) {
			ManagedList<Object> jndiDataSources = new ManagedList<Object>(jndiBindings.size());

			for (Element jndiBinding : jndiBindings) {
				BeanDefinitionBuilder jndiDataSource = BeanDefinitionBuilder.genericBeanDefinition(
					CacheFactoryBean.JndiDataSource.class);

				// NOTE 'jndi-name' and 'type' are required by the XSD so we should have at least 2 attributes.
				NamedNodeMap attributes = jndiBinding.getAttributes();
				ManagedMap<String, String> jndiAttributes = new ManagedMap<String, String>(attributes.getLength());

				for (int index = 0, length = attributes.getLength(); index < length; index++) {
					Attr attribute = (Attr) attributes.item(index);
					jndiAttributes.put(attribute.getLocalName(), attribute.getValue());
				}

				jndiDataSource.addPropertyValue("attributes", jndiAttributes);

				List<Element> jndiProps = DomUtils.getChildElementsByTagName(jndiBinding, "jndi-prop");

				if (!CollectionUtils.isEmpty(jndiProps)) {
					ManagedList<Object> props = new ManagedList<Object>(jndiProps.size());

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

			builder.addPropertyValue("jndiDataSources", jndiDataSources);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected String resolveId(Element element, AbstractBeanDefinition definition, ParserContext parserContext)
			throws BeanDefinitionStoreException {
		String name = super.resolveId(element, definition, parserContext);

		if (!StringUtils.hasText(name)) {
			name = GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME;
			// Set Cache bean alias for backwards compatibility...
			parserContext.getRegistry().registerAlias(name, "gemfire-cache");
		}

		return name;
	}
}
