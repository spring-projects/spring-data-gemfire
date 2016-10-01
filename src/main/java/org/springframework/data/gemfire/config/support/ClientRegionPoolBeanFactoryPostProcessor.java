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
 *
 */

package org.springframework.data.gemfire.config.support;

import java.util.HashSet;
import java.util.Set;

import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.util.SpringUtils;

/**
 * {@link ClientRegionPoolBeanFactoryPostProcessor} is a Spring {@link BeanFactoryPostProcessor} implementation
 * ensuring a proper dependency is declared between a GemFire client {@link com.gemstone.gemfire.cache.Region}
 * and the GemFire client {@link com.gemstone.gemfire.cache.client.Pool} it references and uses, providing
 * the GemFire client {@link com.gemstone.gemfire.cache.client.Pool} has been defined and configured with
 * Spring (Data GemFire) configuration meta-data (e.g. XML).
 *
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @since 1.8.2
 */
public class ClientRegionPoolBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

	protected static final String POOL_NAME_PROPERTY = "poolName";

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		Set<String> clientRegionBeanNames = new HashSet<String>();
		Set<String> poolBeanNames = new HashSet<String>();

		for (String beanName : beanFactory.getBeanDefinitionNames()) {
			BeanDefinition beanDefinition = beanFactory.getBeanDefinition(beanName);

			if (isClientRegionBean(beanDefinition)) {
				clientRegionBeanNames.add(beanName);
			}
			else if (isPoolBean(beanDefinition)) {
				poolBeanNames.add(beanName);
			}
		}

		for (String clientRegionBeanName : clientRegionBeanNames) {
			BeanDefinition clientRegionBean = beanFactory.getBeanDefinition(clientRegionBeanName);
			String poolName = getPoolName(clientRegionBean);

			if (poolBeanNames.contains(poolName)) {
				SpringUtils.addDependsOn(clientRegionBean, poolName);
			}
		}
	}

	/* (non-Javadoc)*/
	boolean isClientRegionBean(BeanDefinition beanDefinition) {
		return ClientRegionFactoryBean.class.getName().equals(beanDefinition.getBeanClassName());
	}

	/* (non-Javadoc)*/
	boolean isPoolBean(BeanDefinition beanDefinition) {
		return PoolFactoryBean.class.getName().equals(beanDefinition.getBeanClassName());
	}

	/* (non-Javadoc) */
	String getPoolName(BeanDefinition clientRegionBean) {
		PropertyValue poolNameProperty = clientRegionBean.getPropertyValues().getPropertyValue(POOL_NAME_PROPERTY);
		return (poolNameProperty != null ? String.valueOf(poolNameProperty.getValue()) : null);
	}
}
