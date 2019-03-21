/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.w3c.dom.Element;

/**
 * The TemplateRegionParser class is a generic Region parser used to parse Region Template bean definitions in the
 * Spring context configuration meta-data to encapsulate common Region 'attribute' configuration irrespective of the
 * Region's Data Policy (e.g. REPLICATE, PARTITION).
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.RegionFactoryBean
 * @see org.springframework.data.gemfire.config.AbstractRegionParser
 * @since 1.5.0
 */
class TemplateRegionParser extends AbstractRegionParser {

	@Override
	protected Class<?> getRegionFactoryClass() {
		return RegionFactoryBean.class;
	}

	@Override
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion) {
		BeanDefinitionBuilder regionAttributesBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			RegionAttributesFactoryBean.class);

		doParseCommonRegionConfiguration(element, parserContext, builder, regionAttributesBuilder, subRegion);

		builder.addPropertyValue("attributes", regionAttributesBuilder.getBeanDefinition());
	}

}
