/*
 * Copyright 2010-2019 the original author or authors.
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

import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.BeanDefinition;
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
 * Bean definition parser for the &lt;gfe:partitioned-region&gt; SDG XML namespace (XSD) element.
 *
 * To avoid eager evaluations, Region attributes are declared as a nested bean definitions.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.PartitionedRegionFactoryBean
 * @see AbstractRegionParser
 */
class PartitionedRegionParser extends AbstractRegionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getRegionFactoryClass() {
		return PartitionedRegionFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("unchecked")
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder regionBuilder,
			boolean subRegion) {

		validateDataPolicyShortcutAttributesMutualExclusion(element, parserContext);

		BeanDefinitionBuilder regionAttributesBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(RegionAttributesFactoryBean.class);

		doParseRegionConfiguration(element, parserContext, regionBuilder, regionAttributesBuilder, subRegion);

		regionBuilder.addPropertyValue("attributes", regionAttributesBuilder.getBeanDefinition());

		BeanDefinitionBuilder partitionAttributesBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(PartitionAttributesFactoryBean.class);

		mergeTemplateRegionPartitionAttributes(element, parserContext, regionBuilder, partitionAttributesBuilder);

		parseColocatedWith(element, regionBuilder, partitionAttributesBuilder, "colocated-with");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "copies", "redundantCopies");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "local-max-memory");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "recovery-delay");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "startup-recovery-delay");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "total-buckets", "totalNumBuckets");
		ParsingUtils.setPropertyValue(element, partitionAttributesBuilder, "total-max-memory");

		Element partitionListenerSubElement = DomUtils.getChildElementByTagName(element, "partition-listener");

		if (partitionListenerSubElement != null) {
			partitionAttributesBuilder.addPropertyValue("partitionListeners",
				parsePartitionListeners(partitionListenerSubElement, parserContext, regionBuilder));
		}

		Element partitionResolverSubElement = DomUtils.getChildElementByTagName(element, "partition-resolver");

		if (partitionResolverSubElement != null) {
			partitionAttributesBuilder.addPropertyValue("partitionResolver",
				parsePartitionResolver(partitionResolverSubElement, parserContext, regionBuilder));
		}

		List<Element> fixedPartitionSubElements = DomUtils.getChildElementsByTagName(element, "fixed-partition");

		if (!CollectionUtils.isEmpty(fixedPartitionSubElements)){

			@SuppressWarnings("rawtypes")
			ManagedList fixedPartitionAttributes = new ManagedList();

			for (Element fixedPartition : fixedPartitionSubElements) {

				BeanDefinitionBuilder fixedPartitionAttributesBuilder =
					BeanDefinitionBuilder.genericBeanDefinition(FixedPartitionAttributesFactoryBean.class);

				ParsingUtils.setPropertyValue(fixedPartition, fixedPartitionAttributesBuilder, "partition-name");
				ParsingUtils.setPropertyValue(fixedPartition, fixedPartitionAttributesBuilder, "num-buckets");
				ParsingUtils.setPropertyValue(fixedPartition, fixedPartitionAttributesBuilder, "primary");

				fixedPartitionAttributes.add(fixedPartitionAttributesBuilder.getBeanDefinition());
			}

			partitionAttributesBuilder.addPropertyValue("fixedPartitionAttributes", fixedPartitionAttributes);
		}

		regionAttributesBuilder.addPropertyValue("partitionAttributes", partitionAttributesBuilder.getBeanDefinition());
	}

	/* (non-Javadoc) */
	void mergeTemplateRegionPartitionAttributes(Element element, ParserContext parserContext,
			BeanDefinitionBuilder regionBuilder, BeanDefinitionBuilder partitionAttributesBuilder) {

		String regionTemplateName = getParentName(element);

		if (StringUtils.hasText(regionTemplateName)) {
			if (parserContext.getRegistry().containsBeanDefinition(regionTemplateName)) {

				BeanDefinition templateRegion = parserContext.getRegistry().getBeanDefinition(regionTemplateName);

				BeanDefinition templateRegionAttributes = getRegionAttributesBeanDefinition(templateRegion);

				if (templateRegionAttributes != null) {
					if (templateRegionAttributes.getPropertyValues().contains("partitionAttributes")) {

						PropertyValue partitionAttributesProperty = templateRegionAttributes.getPropertyValues()
							.getPropertyValue("partitionAttributes");

						Object partitionAttributes = partitionAttributesProperty.getValue();

						if (partitionAttributes instanceof BeanDefinition) {
							partitionAttributesBuilder.getRawBeanDefinition()
								.overrideFrom((BeanDefinition) partitionAttributes);
						}
					}
				}
			}
			else {
				parserContext.getReaderContext().error(String.format(
					"The Region template [%1$s] must be 'defined before' the Region [%2$s] referring to the template!",
						regionTemplateName, resolveId(element, regionBuilder.getRawBeanDefinition(), parserContext)), element);
			}
		}
	}

	/* (non-Javadoc) */
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

	/* (non-Javadoc) */
	private Object parsePartitionResolver(Element subElement, ParserContext parserContext,
			BeanDefinitionBuilder builder) {

		return ParsingUtils.parseRefOrSingleNestedBeanDeclaration(subElement, parserContext, builder);
	}

	/* (non-Javadoc) */
	private Object parsePartitionListeners(Element subElement, ParserContext parserContext,
			BeanDefinitionBuilder builder) {

		return ParsingUtils.parseRefOrNestedBeanDeclaration(subElement, parserContext, builder);
	}
}
