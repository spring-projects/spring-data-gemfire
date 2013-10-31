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
import org.springframework.data.gemfire.wan.AsyncEventQueueFactoryBean;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;

/**
 * The PdxDiskStoreAwareBeanFactoryPostProcessor class is a BeanFactoryPostProcessor that modifies all Region bean
 * definitions in the Spring BeanFactory to form a dependency on the Cache's PDX Disk Store bean.  A persistent Region
 * may contain PDX typed data, in which case, the PDX type meta-data stored to disk needs to be loaded before the Region
 * having PDX data is loaded from disk.
 * <p/>
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @since 1.3.3
 */
@SuppressWarnings("unused")
public class PdxDiskStoreAwareBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

	protected static final String DATA_POLICY_PROPERTY = "dataPolicy";
	protected static final String DATA_POLICY_NAME_PROPERTY = "dataPolicyName";
	protected static final String PERSISTENT_KEYWORD = "PERSISTENT";
	protected static final String PERSISTENT_PROPERTY = "persistent";
	protected static final String SHORTCUT_PROPERTY = "shortcut";

	protected static final String[] EMPTY_STRING_ARRAY = new String[0];

	private final String pdxDiskStoreName;

	public PdxDiskStoreAwareBeanFactoryPostProcessor(final String pdxDiskStoreName) {
		Assert.isTrue(StringUtils.hasText(pdxDiskStoreName), "The name of the PDX Disk Store must be specified!");
		this.pdxDiskStoreName = pdxDiskStoreName;
	}

	public String getPdxDiskStoreName() {
		return pdxDiskStoreName;
	}

	@Override
	public void postProcessBeanFactory(final ConfigurableListableBeanFactory beanFactory) throws BeansException {
		Assert.state(!beanFactory.isConfigurationFrozen(),
			"The BeanFactory configuration meta-data is frozen and cannot be modified further!");
		//postProcessPdxDiskStoreDependencies(beanFactory);
		postProcessPdxDiskStoreDependencies(beanFactory, AsyncEventQueue.class, DiskStore.class, Region.class);
		//postProcessPdxDiskStoreDependencies(beanFactory, AsyncEventQueueFactoryBean.class, DiskStoreFactoryBean.class,
		//	RegionLookupFactoryBean.class);
	}

	private void postProcessPdxDiskStoreDependencies(final ConfigurableListableBeanFactory beanFactory, final Class<?>... beanTypes) {
		for (Class<?> beanType : beanTypes) {
			for (String beanName : beanFactory.getBeanNamesForType(beanType)) {
				if (!beanName.equalsIgnoreCase(getPdxDiskStoreName())) {
					BeanDefinition beanDefinition = beanFactory.getBeanDefinition(beanName);

					if (isDiskStore(beanDefinition) || (isPersistent(beanDefinition)
							&& !hasDiskStoreReference(beanFactory, beanDefinition))) {
						addPdxDiskStoreDependency(beanDefinition);
					}
				}
			}
		}
	}

	// TODO remove
	private void postProcessPdxDiskStoreDependencies(final ConfigurableListableBeanFactory beanFactory) {
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
				if (isDiskStore(beanDefinition) || isPersistentAsyncEventQueue(beanDefinition)
						|| isPersistentRegion(beanDefinition)) {
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
			&& (DiskStoreFactoryBean.class.isAssignableFrom(beanDefinition.getBeanClass()))
				|| DiskStore.class.isAssignableFrom(beanDefinition.getBeanClass()));
	}

	// TODO remove
	private boolean isPersistentAsyncEventQueue(final BeanDefinition beanDefinition) {
		return (beanDefinition instanceof AbstractBeanDefinition
			&& isPersistentAsyncEventQueue((AbstractBeanDefinition) beanDefinition));
	}

	// TODO remove
	private boolean isPersistentAsyncEventQueue(final AbstractBeanDefinition beanDefinition) {
		return (isAsyncEventQueue(beanDefinition) && isPersistent(beanDefinition));
	}

	// TODO remove
	private boolean isAsyncEventQueue(final AbstractBeanDefinition beanDefinition) {
		return (beanDefinition.hasBeanClass()
			&& AsyncEventQueueFactoryBean.class.isAssignableFrom(beanDefinition.getBeanClass()));
	}

	// TODO remove
	private boolean isPersistentRegion(final BeanDefinition beanDefinition) {
		return (beanDefinition instanceof AbstractBeanDefinition
			&& isPersistentRegion((AbstractBeanDefinition) beanDefinition));
	}

	// TODO remove
	private boolean isPersistentRegion(final AbstractBeanDefinition beanDefinition) {
		return (isRegion(beanDefinition) && isPersistent(beanDefinition));
	}

	// TODO remove
	// NOTE Class.isAssignableFrom is not null-safe, hence the AbstractBeanDefinition.hasBeanClass call!
	private boolean isRegion(final AbstractBeanDefinition beanDefinition) {
		return (beanDefinition.hasBeanClass()
			&& RegionLookupFactoryBean.class.isAssignableFrom(beanDefinition.getBeanClass()));
	}

	private boolean hasDiskStoreReference(final ConfigurableListableBeanFactory beanFactory,
			final BeanDefinition beanDefinition) {
		String diskStoreName = getPropertyValue(beanDefinition, "diskStoreName");
		return (StringUtils.hasText(diskStoreName) && beanFactory.containsBeanDefinition(diskStoreName));
	}

	// TODO will property placeholders be a problem or will the PropertyPlaceholderConfigurer BeanFactoryPostProcessor
	// execute before this BeanFactoryPostProcessor???
	private boolean isPersistent(final BeanDefinition beanDefinition) {
		boolean persistent = getPropertyValue(beanDefinition, DATA_POLICY_PROPERTY, DATA_POLICY_NAME_PROPERTY)
			.contains(PERSISTENT_KEYWORD);

		persistent |= Boolean.parseBoolean(getPropertyValue(beanDefinition, PERSISTENT_PROPERTY));

		persistent |= getPropertyValue(beanDefinition, SHORTCUT_PROPERTY).contains(PERSISTENT_KEYWORD);

		return persistent;
	}

	/**
	 * Gets the value of a potentially multi-named property on a BeanDefinition returning the first non-null value.
	 * <p/>
	 * @param beanDefinition the BeanDefinition of the Bean with the property identified by name(s).
	 * @param propertyNames a String array containing all the possible names of the property.
	 * @return the first non-null value of the property, which might have multi-names (aliases).
	 * @see #getPropertyValue(org.springframework.beans.factory.config.BeanDefinition, String)
	 */
	private String getPropertyValue(final BeanDefinition beanDefinition, final String... propertyNames) {
		String propertyValue = null;

		for (String propertyName : propertyNames) {
			propertyValue = getPropertyValue(beanDefinition, propertyName);
			if (!isNull(propertyValue)) {
				break;
			}
		}

		return String.valueOf(propertyValue);
	}

	/**
	 * Gets the value of the property identified by name from the Bean's BeanDefinition.
	 * <p/>
	 * @param beanDefinition the BeanDefinition containing the configuration meta-data and values of the properties
	 * for the Bean.
	 * @param propertyName a String identifying the property by name.
	 * @return a String value of the named property for the Bean.
	 * @see #getPropertyValue(org.springframework.beans.factory.config.BeanDefinition, String...)
	 */
	private String getPropertyValue(final BeanDefinition beanDefinition, final String propertyName) {
		PropertyValue propertyValue = beanDefinition.getPropertyValues().getPropertyValue(propertyName);
		return String.valueOf(propertyValue != null ? propertyValue.getValue() : null);
	}

	/**
	 * Determines whether the specified String value is null.  The String value is null if it is a null references
	 * or is equal to the "null" String irrespective of case or whitespace.
	 * <p/>
	 * @param value the String to evaluate for null value.
	 * @return a boolean indicating whether the String value is null.
	 */
	private boolean isNull(final String value) {
		return (value == null || "null".equalsIgnoreCase(value.trim()));
	}

	/**
	 * Adds the PDX Disk Store dependency to the beginning of the list of dependencies declared by the Bean.
	 * <p/>
	 * @param beanDefinition the BeanDefinition to add the dependency to the PDX Disk Store on.
	 * @see org.springframework.beans.factory.config.BeanDefinition#setDependsOn(String[])
	 */
	private void addPdxDiskStoreDependency(final BeanDefinition beanDefinition) {
		String[] newDependsOn = (String[]) ArrayUtils.insert(getDependsOn(beanDefinition), 0, getPdxDiskStoreName());

		beanDefinition.setDependsOn(newDependsOn);
	}

	/**
	 * Gets the current list of dependencies declared in the BeanDefinition for the Bean, returning an
	 * empty String array if the dependsOn property is null.
	 * <p/>
	 * @param beanDefinition the BeanDefinition of the Bean containing the dependencies.
	 * @return an array of Bean names that this Bean depends on, or an empty String array if the dependencies
	 * are undefined.
	 * @see org.springframework.beans.factory.config.BeanDefinition#getDependsOn()
	 */
	private String[] getDependsOn(final BeanDefinition beanDefinition) {
		return (beanDefinition.getDependsOn() != null ? beanDefinition.getDependsOn() : EMPTY_STRING_ARRAY);
	}

}
