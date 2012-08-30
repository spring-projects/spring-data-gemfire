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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.FixedPartitionAttributesFactoryBean;
import org.springframework.data.gemfire.PartitionAttributesFactoryBean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.util.StringUtils;
import org.springframework.util.CollectionUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

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
	protected Class<?> getRegionFactoryClass() {
		return PartitionedRegionFactoryBean.class;
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion) {
		super.doParse(element, builder);

		BeanDefinitionBuilder attrBuilder = subRegion ? builder : BeanDefinitionBuilder
				.genericBeanDefinition(RegionAttributesFactoryBean.class);

		super.doParseCommonRegionConfiguration(element, parserContext, builder, attrBuilder, subRegion);
		//
		// partition attributes
		BeanDefinitionBuilder parAttrBuilder = BeanDefinitionBuilder
				.genericBeanDefinition(PartitionAttributesFactoryBean.class);

		String attr = element.getAttribute("colocated-with");

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
		Element subElement = DomUtils.getChildElementByTagName(element, "partition-resolver");
		// parse nested partition resolver element
		if (subElement != null) {
			parAttrBuilder.addPropertyValue("partitionResolver",
					parsePartitionResolver(parserContext, subElement, builder));
		}

		subElement = DomUtils.getChildElementByTagName(element, "partition-listener");
		// parse nested partition resolver element
		if (subElement != null) {
			parAttrBuilder.addPropertyValue("partitionListeners",
					parsePartitionListeners(parserContext, subElement, builder));
		}
		
		List<Element> fixedPartitions = DomUtils.getChildElementsByTagName(element, "fixed-partition");
		if (! CollectionUtils.isEmpty(fixedPartitions)){
			
		    @SuppressWarnings("rawtypes")
			ManagedList fixedPartitionAttributes = new ManagedList();
			for (Element fp: fixedPartitions) {
				BeanDefinitionBuilder fpaBuilder = BeanDefinitionBuilder.genericBeanDefinition(FixedPartitionAttributesFactoryBean.class);
				ParsingUtils.setPropertyValue(fp, fpaBuilder, "partition-name");
				ParsingUtils.setPropertyValue(fp, fpaBuilder, "num-buckets");
				ParsingUtils.setPropertyValue(fp, fpaBuilder, "primary");
				fixedPartitionAttributes.add(fpaBuilder.getBeanDefinition());
			}
			parAttrBuilder.addPropertyValue("fixedPartitionAttributes", fixedPartitionAttributes);
		}

		// // add partition attributes attributes
		attrBuilder.addPropertyValue("partitionAttributes", parAttrBuilder.getBeanDefinition());
		// add partition/overflow settings as attributes
		if (!subRegion) {
			builder.addPropertyValue("attributes", attrBuilder.getBeanDefinition());
		}
	}

	private Object parsePartitionResolver(ParserContext parserContext, Element subElement, BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrSingleNestedBeanDeclaration(parserContext, subElement, builder);
	}

	private Object parsePartitionListeners(ParserContext parserContext, Element subElement,
			BeanDefinitionBuilder builder) {
		return ParsingUtils.parseRefOrNestedBeanDeclaration(parserContext, subElement, builder);
	}
}