/*
 * Copyright 2010-2019 the original author or authors.
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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.w3c.dom.Element;

/**
 * Bean definition parser for the &lt;gfe:replicated-region&gt; SDG XML namespace (XSD) element.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.ReplicatedRegionFactoryBean
 * @see AbstractRegionParser
 */
class ReplicatedRegionParser extends AbstractRegionParser {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class<?> getRegionFactoryClass() {
		return ReplicatedRegionFactoryBean.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion) {

		validateDataPolicyShortcutAttributesMutualExclusion(element, parserContext);

		ParsingUtils.parseScope(element, builder);

		BeanDefinitionBuilder regionAttributesBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			RegionAttributesFactoryBean.class);

		doParseRegionConfiguration(element, parserContext, builder, regionAttributesBuilder, subRegion);

		builder.addPropertyValue("attributes", regionAttributesBuilder.getBeanDefinition());
	}
}
