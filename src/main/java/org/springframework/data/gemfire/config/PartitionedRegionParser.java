/*
 * Copyright 2010-2012 the original author or authors.
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
import org.springframework.util.StringUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;

/**
 * Parser for &lt;partitioned-region;gt; definitions.
 * 
 * To avoid eager evaluations, the region attributes are declared as a nested
 * definition.
 * 
 * @author Costin Leau
 * @author David Turanski
 */
class PartitionedRegionParser extends AbstractRegionParser {

	@Override
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion) {
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

		ParsingUtils.parseScope(element, builder);

		BeanDefinitionBuilder attrBuilder = subRegion ? builder : BeanDefinitionBuilder
				.genericBeanDefinition(RegionAttributesFactoryBean.class);

		super.doParseRegionCommon(element, parserContext, builder, attrBuilder, subRegion);
		//
		// partition attributes
		BeanDefinitionBuilder parAttrBuilder = BeanDefinitionBuilder
				.genericBeanDefinition(PartitionAttributesFactoryBean.class);

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
		//
		List<Element> subElements = DomUtils.getChildElementsByTagName(element, "partition-resolver");
		//
		// // parse nested cache-listener elements
		for (Element subElement : subElements) {
			parAttrBuilder.addPropertyValue("partitionResolver",
					parsePartitionResolver(parserContext, subElement, builder));
		}
		//
		// // add partition attributes attributes
		attrBuilder.addPropertyValue("partitionAttributes", parAttrBuilder.getBeanDefinition());
		// add partition/overflow settings as attributes
		if (!subRegion) {
			builder.addPropertyValue("attributes", attrBuilder.getBeanDefinition());
		}
	}

	private Object parsePartitionResolver(ParserContext parserContext, Element subElement, BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder);
	}
}