/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;

/**
 * The ClientRegionAndPoolBeanFactoryPostProcessor class is a Spring {@link BeanFactoryPostProcessor} that ensures
 * a proper dependency is declared between a GemFire client Region and the GemFire Pool it references and uses, if
 * the GemFire Pool has been defined and configured in Spring (Data GemFire) configuration meta-data (XML).
 *
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @since 1.8.2
 */
public class ClientRegionAndPoolBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

	protected static final String POOL_NAME_PROPERTY = "poolName";

	/* (non-Javadoc)*/
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
				addDependsOn(clientRegionBean, poolName);
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

	/* (non-Javadoc) */
	BeanDefinition addDependsOn(BeanDefinition bean, String beanName) {
		String[] dependsOn = bean.getDependsOn();
		List<String> dependsOnList = new ArrayList<String>();

		if (dependsOn != null) {
			Collections.addAll(dependsOnList, dependsOn);
		}

		dependsOnList.add(beanName);
		bean.setDependsOn(dependsOnList.toArray(new String[dependsOnList.size()]));

		return bean;
	}
}
