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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.FixedPartitionAttributesFactoryBean;
import org.springframework.data.gemfire.PartitionAttributesFactoryBean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
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
 * @author John Blum
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

		// partition attributes
		BeanDefinitionBuilder parAttrBuilder = BeanDefinitionBuilder
				.genericBeanDefinition(PartitionAttributesFactoryBean.class);

		ParsingUtils.setPropertyReference(element, parAttrBuilder, "colocated-with-ref", "colocatedWith");
		ParsingUtils.setPropertyValue(element, parAttrBuilder, "local-max-memory");
		ParsingUtils.setPropertyValue(element, parAttrBuilder, "copies","redundantCopies");
		ParsingUtils.setPropertyValue(element, parAttrBuilder, "recovery-delay");
		ParsingUtils.setPropertyValue(element, parAttrBuilder, "startup-recovery-delay");
		ParsingUtils.setPropertyValue(element, parAttrBuilder, "total-max-memory");
		ParsingUtils.setPropertyValue(element, parAttrBuilder, "total-buckets","totalNumBuckets");

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
