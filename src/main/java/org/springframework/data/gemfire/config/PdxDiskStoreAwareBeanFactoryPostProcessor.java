/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config;

import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.data.gemfire.DiskStoreFactoryBean;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The PdxDiskStoreAwareBeanFactoryPostProcessor class is a BeanFactoryPostProcessor that modifies all Region bean
 * definitions in the Spring BeanFactory to form a dependency on the Cache's PDX Disk Store bean.  A persistent Region
 * may contain PDX typed data, in which case, the PDX type meta-data stored to disk needs to be loaded before the Region
 * having PDX data is loaded from disk.
 * <p/>
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @since 1.3.3
 */
@SuppressWarnings("unused")
public class PdxDiskStoreAwareBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

	private static final String[] EMPTY_STRING_ARRAY = new String[0];

	private String pdxDiskStoreName;

	public PdxDiskStoreAwareBeanFactoryPostProcessor() {
	}

	public PdxDiskStoreAwareBeanFactoryPostProcessor(final String pdxDiskStoreName) {
		setPdxDiskStoreName(pdxDiskStoreName);
	}

	public String getPdxDiskStoreName() {
		Assert.state(!StringUtils.isEmpty(pdxDiskStoreName), "The PDX Disk Store name was not properly initialized!");
		return pdxDiskStoreName;
	}

	public final void setPdxDiskStoreName(final String pdxDiskStoreName) {
		this.pdxDiskStoreName = pdxDiskStoreName;
	}

	@Override
	public void postProcessBeanFactory(final ConfigurableListableBeanFactory beanFactory) throws BeansException {
		for (String beanName : beanFactory.getBeanDefinitionNames()) {
			// NOTE do not add the PDX Disk Store bean dependency to itself!
			if (!beanName.equalsIgnoreCase(getPdxDiskStoreName())) {
				BeanDefinition beanDefinition = beanFactory.getBeanDefinition(beanName);

				// NOTE an optimization to only inspect Region bean definitions for persistent Regions (NOTE, the
				// persistent Region might also define a Disk Store, or would depend on the DEFAULT Disk Store,
				// which may depend on PDX type meta-data).
				// NOTE add the dependency to the Disk Store bean definition in case the Disk Store bean is defined
				// before the Region that uses it!
				// TODO what else depends on a PDX Disk Store besides Regions?
				if (isPersistentRegion(beanDefinition) || isDiskStore(beanDefinition)) {
					addPdxDiskStoreDependency(beanDefinition);
				}
			}
		}
	}

	private boolean isDiskStore(final BeanDefinition beanDefinition) {
		return (beanDefinition instanceof AbstractBeanDefinition
			&& isDiskStore((AbstractBeanDefinition) beanDefinition));
	}

	private boolean isDiskStore(final AbstractBeanDefinition beanDefinition) {
		return (beanDefinition.hasBeanClass()
			&& DiskStoreFactoryBean.class.isAssignableFrom(beanDefinition.getBeanClass()));
	}

	private boolean isPersistentRegion(final BeanDefinition beanDefinition) {
		return (beanDefinition instanceof AbstractBeanDefinition
			&& isPersistentRegion((AbstractBeanDefinition) beanDefinition));
	}

	private boolean isPersistentRegion(final AbstractBeanDefinition beanDefinition) {
		return (isRegion(beanDefinition) && isPersistent(beanDefinition));
	}

	// NOTE Class.isAssignableFrom is not null-safe, hence the AbstractBeanDefinition.hasBeanClass call!
	private boolean isRegion(final AbstractBeanDefinition beanDefinition) {
		return (beanDefinition.hasBeanClass()
			&& RegionLookupFactoryBean.class.isAssignableFrom(beanDefinition.getBeanClass()));
	}

	private boolean isPersistent(final AbstractBeanDefinition beanDefinition) {
		PropertyValue persistentPropertyValue = beanDefinition.getPropertyValues().getPropertyValue("persistent");

		return (persistentPropertyValue != null
			&& Boolean.parseBoolean(String.valueOf(persistentPropertyValue.getValue())));
	}

	private void addPdxDiskStoreDependency(final BeanDefinition beanDefinition) {
		String[] newDependsOn = (String[]) ArrayUtils.insert(getDependsOn(beanDefinition), 0, getPdxDiskStoreName());

		beanDefinition.setDependsOn(newDependsOn);
	}

	private String[] getDependsOn(final BeanDefinition beanDefinition) {
		return (beanDefinition.getDependsOn() != null ? beanDefinition.getDependsOn() : EMPTY_STRING_ARRAY);
	}

}
