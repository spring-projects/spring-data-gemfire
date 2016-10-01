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

package org.springframework.data.gemfire.config.support;

import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;

/**
 * {@link PdxDiskStoreAwareBeanFactoryPostProcessor} is a Spring {@link BeanFactoryPostProcessor} that modifies
 * all GemFire Async Event Queue, Region and Disk Store beans in the Spring container to form a dependency on
 * the Cache's PDX {@link DiskStore} bean.
 *
 * A persistent Region may contain PDX typed data, in which case, the PDX type meta-data stored to disk needs to be
 * loaded before the Region having PDX data is loaded from disk.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @see com.gemstone.gemfire.cache.DiskStore
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue
 * @since 1.3.3
 */
public class PdxDiskStoreAwareBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

	protected static final String[] EMPTY_STRING_ARRAY = new String[0];

	private final String pdxDiskStoreName;

	/**
	 * Constructs an instance of the {@link PdxDiskStoreAwareBeanFactoryPostProcessor} class initialized with
	 * the given PDX {@link DiskStore} name.
	 *
	 * @param pdxDiskStoreName name of the GemFire PDX {@link DiskStore}.
	 * @throws IllegalArgumentException if the GemFire PDX {@link DiskStore} name is unspecified.
	 */
	public PdxDiskStoreAwareBeanFactoryPostProcessor(String pdxDiskStoreName) {
		Assert.hasText(pdxDiskStoreName, "The PDX DiskStore name must be specified");
		this.pdxDiskStoreName = pdxDiskStoreName;
	}

	/**
	 * Returns the name of the GemFire PDX {@link DiskStore}.
	 *
	 * @return the name of the GemFire PDX {@link DiskStore}.
	 */
	public String getPdxDiskStoreName() {
		return pdxDiskStoreName;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		postProcessPdxDiskStoreDependencies(beanFactory, AsyncEventQueue.class, DiskStore.class, Region.class);
	}

	/**
	 * Post processes all beans in the Spring container, application context that may potentially have a dependency,
	 * or requirement on the PDX-based Disk Store being present before the bean itself can be processed and created.
	 * For instance, Regions that might contained PDX-based Key types require the PDX type meta-data to be loaded
	 * before the Region's data (Keys) are accessed.
	 *
	 * @param beanFactory the BeanFactory used to evaluate beans in context for PDX Disk Store dependencies.
	 * @param beanTypes an array of Class types indicating the type of beans to evaluate.
	 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory#getBeanNamesForType(Class)
	 */
	private void postProcessPdxDiskStoreDependencies(ConfigurableListableBeanFactory beanFactory,
			final Class<?>... beanTypes) {
		for (Class<?> beanType : beanTypes) {
			for (String beanName : beanFactory.getBeanNamesForType(beanType)) {
				if (!beanName.equalsIgnoreCase(getPdxDiskStoreName())) {
					BeanDefinition beanDefinition = beanFactory.getBeanDefinition(beanName);

					// NOTE for simplicity sake, we add a bean dependency to any bean definition for a bean
					// (Async Event Queue, Region, Disk Store or otherwise) that may potentially require
					// the PDX Disk Store to exist.
					// NOTE this logic could be optimized to include Disk Store, persistent Async Event Queues
					// and persistent Regions (either by way of 'persistent' attribute, Data Policy
					// or [Client]RegionShortcut) that also does not have an explicit Disk Store reference.
					addPdxDiskStoreDependency(beanDefinition);
				}
			}
		}
	}

	/**
	 * Adds the PDX Disk Store bean name as a dependency at the beginning of the list of dependencies declared
	 * by the Bean.
	 *
	 * @param beanDefinition the BeanDefinition to add the PDX Disk Store dependency to.
	 * @see #getDependsOn(org.springframework.beans.factory.config.BeanDefinition)
	 * @see org.springframework.beans.factory.config.BeanDefinition#setDependsOn(String[])
	 */
	private void addPdxDiskStoreDependency(BeanDefinition beanDefinition) {
		String[] newDependsOn = (String[]) ArrayUtils.insert(getDependsOn(beanDefinition), 0, getPdxDiskStoreName());
		beanDefinition.setDependsOn(newDependsOn);
	}

	/**
	 * Gets the current list of dependencies declared in the BeanDefinition for the Bean, returning an
	 * empty String array if the dependsOn property is null.
	 *
	 * @param beanDefinition the BeanDefinition of the Bean containing the dependencies.
	 * @return an array of Bean names that this Bean depends on, or an empty String array if the dependencies
	 * are undefined.
	 * @see #addPdxDiskStoreDependency(org.springframework.beans.factory.config.BeanDefinition)
	 * @see org.springframework.beans.factory.config.BeanDefinition#getDependsOn()
	 */
	private String[] getDependsOn(BeanDefinition beanDefinition) {
		return SpringUtils.defaultIfNull(beanDefinition.getDependsOn(), EMPTY_STRING_ARRAY);
	}
}
