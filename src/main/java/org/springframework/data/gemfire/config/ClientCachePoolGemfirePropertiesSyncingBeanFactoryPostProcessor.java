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

package org.springframework.data.gemfire.config;

import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.util.StringUtils;

/**
 * The ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessor class is a Spring {@link BeanFactoryPostProcessor}
 * implementation responsible for making sure the GemFire {@link java.util.Properties} between the GemFire ClientCache
 * and GemFire Pool are identical.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @since 1.8.0
 */
@Deprecated
class ClientCachePoolGemfirePropertiesSyncingBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

	protected static final String PROPERTIES_PROPERTY_NAME = "properties";

	/**
	 * Post processes the given {@link ConfigurableListableBeanFactory} by syncing the gemfire.properties used
	 * to configure the GemFire Distributed System constructed by the {@link PoolFactoryBean} during Pool creation
	 * and resolved by the {@link ClientCacheFactoryBean} during ClientCache creation.
	 *
	 * @param beanFactory the {@link ConfigurableListableBeanFactory} to post process.
	 * @throws BeansException if errors occur during post processing.
	 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
	 */
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		BeanDefinition poolFactoryBeanDefinition = null;
		String gemfirePropertiesBeanName = null;

		for (String beanName : beanFactory.getBeanDefinitionNames()) {
			BeanDefinition beanDefinition = beanFactory.getBeanDefinition(beanName);

			if (ClientCacheFactoryBean.class.getName().equals(beanDefinition.getBeanClassName())) {
				PropertyValue gemfirePropertiesPropertyValue = beanDefinition.getPropertyValues()
					.getPropertyValue(PROPERTIES_PROPERTY_NAME);

				gemfirePropertiesBeanName = (gemfirePropertiesPropertyValue != null
					? ((RuntimeBeanReference) gemfirePropertiesPropertyValue.getValue()).getBeanName() : null);
			}
			else if (PoolFactoryBean.class.getName().equals(beanDefinition.getBeanClassName())) {
				poolFactoryBeanDefinition = beanDefinition;
			}
		}

		if (poolFactoryBeanDefinition != null && StringUtils.hasText(gemfirePropertiesBeanName)) {
			poolFactoryBeanDefinition.getPropertyValues().add(PROPERTIES_PROPERTY_NAME,
				new RuntimeBeanReference(gemfirePropertiesBeanName));
		}
	}

}
