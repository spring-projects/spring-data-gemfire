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
import org.springframework.util.StringUtils;
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

		BeanDefinitionBuilder regionAttributesBuilder = (subRegion ? builder
			: BeanDefinitionBuilder.genericBeanDefinition(RegionAttributesFactoryBean.class));

		super.doParseCommonRegionConfiguration(element, parserContext, builder, regionAttributesBuilder, subRegion);

		BeanDefinitionBuilder partitionAttributesBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			PartitionAttributesFactoryBean.class);

		parseColocatedWith(element, builder, partitionAttributesBuilder, "colocated-with");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "local-max-memory");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "copies","redundantCopies");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "recovery-delay");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "startup-recovery-delay");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "total-max-memory");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "total-buckets","totalNumBuckets");

		Element subElement = DomUtils.getChildElementByTagName(element, "partition-resolver");
		// parse nested partition resolver element
		if (subElement != null) {
			partitionAttributesBuilder.addPropertyValue("partitionResolver",
				parsePartitionResolver(parserContext, subElement, builder));
		}

		subElement = DomUtils.getChildElementByTagName(element, "partition-listener");
		// parse nested partition listener element
		if (subElement != null) {
			partitionAttributesBuilder.addPropertyValue("partitionListeners",
				parsePartitionListeners(parserContext, subElement, builder));
		}

		List<Element> fixedPartitions = DomUtils.getChildElementsByTagName(element, "fixed-partition");

		if (!CollectionUtils.isEmpty(fixedPartitions)){
			@SuppressWarnings("rawtypes")
			ManagedList fixedPartitionAttributes = new ManagedList();
			for (Element fp: fixedPartitions) {
				BeanDefinitionBuilder fpaBuilder = BeanDefinitionBuilder.genericBeanDefinition(FixedPartitionAttributesFactoryBean.class);
				ParsingUtils.setPropertyValue(fp, fpaBuilder, "partition-name");
				ParsingUtils.setPropertyValue(fp, fpaBuilder, "num-buckets");
				ParsingUtils.setPropertyValue(fp, fpaBuilder, "primary");
				fixedPartitionAttributes.add(fpaBuilder.getBeanDefinition());
			}
			partitionAttributesBuilder.addPropertyValue("fixedPartitionAttributes", fixedPartitionAttributes);
		}

		// add partition attributes to region attributes
		regionAttributesBuilder.addPropertyValue("partitionAttributes", partitionAttributesBuilder.getBeanDefinition());
		// add partition/overflow settings as attributes to Region (via PartitionRegionFactoryBean -> RegionFactoryBean)
		if (!subRegion) {
			builder.addPropertyValue("attributes", regionAttributesBuilder.getBeanDefinition());
		}
	}

	private void parseColocatedWith(Element element, BeanDefinitionBuilder regionBuilder,
			BeanDefinitionBuilder partitionAttributesBuilder, String attributeName) {
		// NOTE rather than using a dependency (with depends-on) we could also set the colocatedWith property of the
		// PartitionAttributesFactoryBean with a reference to the Region "this" Partitioned Region will be colocated
		// with, where the colocated-with attribute refers to the the bean name/alias of the other, depended on Region
		// providing that the Region's name is also a bean alias of the bean definition.
		//ParsingUtils.setPropertyReference(element, partitionAttributesBuilder, attributeName, "colocatedWith");

		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, attributeName);

		String colocatedWithBeanAlias = element.getAttribute(attributeName);

		if (StringUtils.hasText(colocatedWithBeanAlias)) {
			regionBuilder.addDependsOn(colocatedWithBeanAlias);
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
