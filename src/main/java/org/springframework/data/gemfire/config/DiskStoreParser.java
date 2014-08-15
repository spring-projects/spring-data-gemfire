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
import java.util.concurrent.atomic.AtomicBoolean;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.data.gemfire.DiskStoreFactoryBean;
import org.springframework.data.gemfire.DiskStoreFactoryBean.DiskDir;
import org.springframework.util.CollectionUtils;
import org.springframework.util.xml.DomUtils;
import org.w3c.dom.Element;

/**
 * Parser for &lt;disk-store&gt; bean definitions.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser
 * @see org.springframework.data.gemfire.DiskStoreFactoryBean
 * @see org.w3c.dom.Element
 */
public class DiskStoreParser extends AbstractSingleBeanDefinitionParser {

	private static final AtomicBoolean registered = new AtomicBoolean(false);

	private static void registerDiskStoreBeanPostProcessor(final ParserContext parserContext) {
		if (registered.compareAndSet(false, true)) {
			AbstractBeanDefinition diskStoreBeanPostProcessor = BeanDefinitionBuilder
				.genericBeanDefinition(DiskStoreBeanPostProcessor.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE)
				.getBeanDefinition();

			BeanDefinitionReaderUtils.registerWithGeneratedName(diskStoreBeanPostProcessor,
				parserContext.getRegistry());
		}
	}

	@Override
	protected Class<?> getBeanClass(Element element) {
		return DiskStoreFactoryBean.class;
	}

	@Override
	protected void doParse(Element element, ParserContext parserContext, BeanDefinitionBuilder builder) {
		super.doParse(element, parserContext, builder);

		registerDiskStoreBeanPostProcessor(parserContext);

		builder.setLazyInit(false);

		ParsingUtils.setPropertyReference(element, builder, "cache-ref", "cache");
		ParsingUtils.setPropertyValue(element, builder, "allow-force-compaction");
		ParsingUtils.setPropertyValue(element, builder, "auto-compact");
		ParsingUtils.setPropertyValue(element, builder, "compaction-threshold");
		ParsingUtils.setPropertyValue(element, builder, "max-oplog-size");
		ParsingUtils.setPropertyValue(element, builder, "queue-size");
		ParsingUtils.setPropertyValue(element, builder, "time-interval");
		ParsingUtils.setPropertyValue(element, builder, "write-buffer-size");

		List<Element> diskDirElements = DomUtils.getChildElementsByTagName(element, "disk-dir");

		if (!CollectionUtils.isEmpty(diskDirElements)) {
			ManagedList<AbstractBeanDefinition> diskDirs = new ManagedList<AbstractBeanDefinition>();

			for (Element diskDirElement : diskDirElements) {
				BeanDefinitionBuilder diskDirBuilder = BeanDefinitionBuilder.genericBeanDefinition(DiskDir.class);

				diskDirBuilder.addConstructorArgValue(diskDirElement.getAttribute("location"));

				if (diskDirElement.hasAttribute("max-size")) {
					diskDirBuilder.addConstructorArgValue(diskDirElement.getAttribute("max-size"));
				}

				diskDirs.add(diskDirBuilder.getBeanDefinition());
			}

			builder.addPropertyValue("diskDirs", diskDirs);
		}
	}

}
