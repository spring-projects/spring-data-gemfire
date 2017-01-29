/*
 * Copyright 2012-2018 the original author or authors.
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

import java.util.Arrays;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.StringUtils;

/**
 * {@link ClientRegionPoolBeanFactoryPostProcessor} is a Spring {@link BeanFactoryPostProcessor} implementation
 * ensuring a proper dependency is declared between a GemFire client {@link org.apache.geode.cache.Region}
 * and the GemFire client {@link org.apache.geode.cache.client.Pool} it references and uses, providing
 * the GemFire client {@link org.apache.geode.cache.client.Pool} has been defined and configured with
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
	@SuppressWarnings("all")
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {

		Set<String> clientRegionBeanNames = new HashSet<>();
		Set<String> poolBeanNames = new HashSet<>();

		Arrays.stream(beanFactory.getBeanDefinitionNames()).forEach(beanName -> {

			BeanDefinition beanDefinition = beanFactory.getBeanDefinition(beanName);

			if (isClientRegionBean(beanDefinition)) {
				clientRegionBeanNames.add(beanName);
			}
			else if (isPoolBean(beanDefinition)) {
				poolBeanNames.add(beanName);
			}
		});

		clientRegionBeanNames.forEach(clientRegionBeanName -> {

			BeanDefinition clientRegionBean = beanFactory.getBeanDefinition(clientRegionBeanName);

			String poolName = getPoolName(clientRegionBean);

			if (poolBeanNames.contains(poolName)) {
				SpringUtils.addDependsOn(clientRegionBean, poolName);
			}
		});
	}

	boolean isBeanDefinitionOfType(BeanDefinition beanDefinition, Class<?> type) {

		return Optional.of(beanDefinition)
			.map(it -> beanDefinition.getBeanClassName())
			.filter(StringUtils::hasText)
			.map(beanClassName -> type.getName().equals(beanClassName))
			.orElseGet(() ->
				Optional.ofNullable(beanDefinition.getFactoryMethodName())
					.filter(StringUtils::hasText)
					.filter(it -> beanDefinition instanceof AnnotatedBeanDefinition)
					.map(it -> ((AnnotatedBeanDefinition) beanDefinition).getFactoryMethodMetadata())
					.map(methodMetadata -> type.getName().equals(methodMetadata.getReturnTypeName()))
					.orElse(false)
			);
	}

	/* (non-Javadoc)*/
	boolean isClientRegionBean(BeanDefinition beanDefinition) {
		return isBeanDefinitionOfType(beanDefinition, ClientRegionFactoryBean.class);
	}

	/* (non-Javadoc)*/
	boolean isPoolBean(BeanDefinition beanDefinition) {
		return isBeanDefinitionOfType(beanDefinition, PoolFactoryBean.class);
	}

	/* (non-Javadoc) */
	String getPoolName(BeanDefinition clientRegionBean) {
		PropertyValue poolNameProperty = clientRegionBean.getPropertyValues().getPropertyValue(POOL_NAME_PROPERTY);
		return (poolNameProperty != null ? String.valueOf(poolNameProperty.getValue()) : null);
	}
}
