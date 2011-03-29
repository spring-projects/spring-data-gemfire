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
import java.util.concurrent.ConcurrentMap;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.PartitionAttributesFactoryBean;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;

/**
 * Parser for &lt;partitioned-region;gt; definitions.
 * 
 * To avoid eager evaluations, the region attributes are declared as a nested definition.
 * 
 * @author Costin Leau
 */
class PartitionedRegionParser extends AliasReplacingBeanDefinitionParser {

	protected Class<?> getBeanClass(Element element) {
		return RegionFactoryBean.class;
	}

	@Override
	protected void doParseInternal(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		super.doParse(element, builder);

		// set the data policy
		String attr = element.getAttribute("persistent");

		if (Boolean.parseBoolean(attr)) {
			// check first for GemFire 6.5
			if (ConcurrentMap.class.isAssignableFrom(Region.class)) {
				builder.addPropertyValue("dataPolicy", "PERSISTENT_PARTITION");
			}
			else {
				parserContext.getReaderContext().error(
						"Can define persistent partitions only from GemFire 6.5 onwards - current version is ["
								+ CacheFactory.getVersion() + "]", element);
			}
		}
		else {
			builder.addPropertyValue("dataPolicy", DataPolicy.PARTITION);
		}

		ParsingUtils.setPropertyValue(element, builder, "name", "name");

		attr = element.getAttribute("cache-ref");
		// add cache reference (fallback to default if nothing is specified)
		builder.addPropertyReference("cache", (StringUtils.hasText(attr) ? attr : "gemfire-cache"));

		// region attributes
		BeanDefinitionBuilder attrBuilder = BeanDefinitionBuilder.genericBeanDefinition(RegionAttributesFactoryBean.class);

		ParsingUtils.parseEviction(parserContext, element, attrBuilder);
		ParsingUtils.parseDiskStorage(element, attrBuilder);

		// partition attributes
		BeanDefinitionBuilder parAttrBuilder = BeanDefinitionBuilder.genericBeanDefinition(PartitionAttributesFactoryBean.class);


		attr = element.getAttribute("colocated-with");

		if (StringUtils.hasText(attr)) {
			parAttrBuilder.addPropertyValue("colocatedWith", attr);
		}

		attr = element.getAttribute("local-max-memory");
		if (StringUtils.hasText(attr)) {
			parAttrBuilder.addPropertyValue("localMaxMemory", Integer.valueOf(attr));
		}

		attr = element.getAttribute("copies");
		if (StringUtils.hasText(attr)) {
			parAttrBuilder.addPropertyValue("redundantCopies", Integer.valueOf(attr));
		}

		attr = element.getAttribute("recovery-delay");
		if (StringUtils.hasText(attr)) {
			parAttrBuilder.addPropertyValue("recoveryDelay", Long.valueOf(attr));
		}

		attr = element.getAttribute("startup-recovery-delay");

		if (StringUtils.hasText(attr)) {
			parAttrBuilder.addPropertyValue("startupRecoveryDelay", Long.valueOf(attr));
		}

		attr = element.getAttribute("total-max-memory");
		if (StringUtils.hasText(attr)) {
			parAttrBuilder.addPropertyValue("totalMaxMemory", Long.valueOf(attr));
		}

		attr = element.getAttribute("total-buckets");
		if (StringUtils.hasText(attr)) {
			parAttrBuilder.addPropertyValue("totalNumBuckets", Integer.valueOf(attr));
		}


		List<Element> subElements = DomUtils.getChildElements(element);

		// parse nested cache-listener elements
		for (Element subElement : subElements) {
			String name = subElement.getLocalName();

			if ("cache-listener".equals(name)) {
				builder.addPropertyValue("cacheListeners", parseCacheListener(parserContext, subElement, builder));
			}

			else if ("cache-loader".equals(name)) {
				builder.addPropertyValue("cacheLoader", parseCacheLoader(parserContext, subElement, builder));
			}

			else if ("cache-writer".equals(name)) {
				builder.addPropertyValue("cacheWriter", parseCacheWriter(parserContext, subElement, builder));
			}

			else if ("partition-resolver".equals(name)) {
				parAttrBuilder.addPropertyValue("partitionResolver", parsePartitionResolver(parserContext, subElement,
						builder));
			}
		}

		// add partition attributes attributes
		attrBuilder.addPropertyValue("partitionAttributes", parAttrBuilder.getBeanDefinition());
		// add partition/overflow settings as attributes
		builder.addPropertyValue("attributes", attrBuilder.getBeanDefinition());
	}

	private Object parseCacheListener(ParserContext parserContext, Element subElement, BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder);
	}

	private Object parseCacheLoader(ParserContext parserContext, Element subElement, BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder);
	}

	private Object parseCacheWriter(ParserContext parserContext, Element subElement, BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder);
	}

	private Object parsePartitionResolver(ParserContext parserContext, Element subElement, BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder);
	}
}