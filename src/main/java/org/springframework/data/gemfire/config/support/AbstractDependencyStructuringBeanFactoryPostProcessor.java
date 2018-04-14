/*
 * Copyright 2018 the original author or authors.
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

package org.springframework.data.gemfire.config.support;

import java.util.Optional;

import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.util.StringUtils;

/**
 * The AbstractDependencyStructuringBeanFactoryPostProcessor class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractDependencyStructuringBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

	protected boolean isBeanDefinitionOfType(BeanDefinition beanDefinition, Class<?> type) {

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

	protected boolean isClientCacheBean(BeanDefinition beanDefinition) {
		return isBeanDefinitionOfType(beanDefinition, ClientCacheFactoryBean.class);
	}

	protected boolean isClientRegionBean(BeanDefinition beanDefinition) {
		return isBeanDefinitionOfType(beanDefinition, ClientRegionFactoryBean.class);
	}

	protected boolean isPoolBean(BeanDefinition beanDefinition) {
		return isBeanDefinitionOfType(beanDefinition, PoolFactoryBean.class);
	}
}
