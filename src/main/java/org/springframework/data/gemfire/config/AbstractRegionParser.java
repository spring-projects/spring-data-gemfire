/*
 * Copyright 2012 the original author or authors.
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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.SubRegionFactoryBean;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

/**
 * Base class for all Region Parsers
 * 
 * @author David Turanski
 */
abstract class AbstractRegionParser extends AliasReplacingBeanDefinitionParser {
	protected final Log log = LogFactory.getLog(getClass());
	
	protected Class<?> getBeanClass(Element element) {
		if (element.hasAttribute("subregion")) {
			return SubRegionFactoryBean.class;
		}
		else {
			return RegionFactoryBean.class;
		}
	}

	@Override
	protected void doParseInternal(Element element, ParserContext parserContext, BeanDefinitionBuilder builder){
		super.doParse(element, builder);
		boolean subRegion = element.hasAttribute("subregion");
		
		doParseRegion(element, parserContext, builder, subRegion);
		
		if (subRegion) {
			builder.addPropertyValue("parent", parserContext.getContainingBeanDefinition().getAttribute("parent")); 
			builder.addPropertyValue("regionName", element.getAttribute(NAME_ATTRIBUTE));
		}
	}
	
	protected abstract void doParseRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder,
			boolean subRegion);

	protected void doParseSubRegion(Element element, Element subElement, ParserContext parserContext,
			BeanDefinitionBuilder builder, boolean subRegion) {
 
		String regionPath = null;
		String parentBeanName = null;
		if (subRegion) {
			parentBeanName = parserContext.getContainingBeanDefinition().getAttribute("regionPath").toString();
		}
		else {
			parentBeanName = getRegionNameFromElement(element);
		}
		regionPath = StringUtils
				.arrayToDelimitedString(
						new String[] { parentBeanName, getRegionNameFromElement(subElement) }, "/");
		if (!regionPath.startsWith("/")) {
			regionPath = "/" + regionPath;
		}

		/*
		 * The Region parser needs some context to handle recursion correctly 
		 */
		builder.getBeanDefinition().setAttribute("parent",
				new BeanDefinitionHolder(builder.getBeanDefinition(), parentBeanName));
		builder.getBeanDefinition().setAttribute("regionPath",regionPath);

		// Make recursive call
		BeanDefinition subRegionDef = this.parseSubRegion(subElement, parserContext, builder);
		//TODO: Is there a better work-around?
		/*
		 * This setting prevents the BF from generating a name for this been
		 */
		subRegionDef.setScope(BeanDefinition.SCOPE_PROTOTYPE);
        
		if (log.isDebugEnabled()) {
			log.debug("registering subregion as " + regionPath);
		}
		this.registerBeanDefinition(new BeanDefinitionHolder(subRegionDef, regionPath), parserContext.getRegistry());
	}

	private BeanDefinition parseSubRegion(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
        /*
         * Easy way to mark this element as a subregion
         */
		element.setAttribute("subregion", "true");
		BeanDefinition beanDefinition = parserContext.getDelegate().parseCustomElement(element,
				builder.getBeanDefinition());
		return beanDefinition;
	}
	
	private String getRegionNameFromElement(Element element){
		String name = element.getAttribute(NAME_ATTRIBUTE);
		return StringUtils.hasText(name)? name: element.getAttribute(ID_ATTRIBUTE);
	}
}