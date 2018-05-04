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
import java.util.function.Predicate;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.query.Index;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.core.type.MethodMetadata;
import org.springframework.data.gemfire.GenericRegionFactoryBean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The {@link AbstractDependencyStructuringBeanFactoryPostProcessor} class is a Spring {@link BeanFactoryPostProcessor}
 * post processing the Spring {@link BeanFactory} to help ensure that the dependencies between different Apache Geode
 * or Pivotal GemFire objects (e.g. {@link Region} and a {@link LuceneIndex} or an OQL {@link Index}) have been
 * properly declared in order to the lifecycle of those components are upheld according to Apache Geode
 * or Pivotal GemFire requirements/rules.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @since 2.1.0
 */
@SuppressWarnings("unused")
public abstract class AbstractDependencyStructuringBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

	protected BeanDefinition addDependsOn(BeanDefinition beanDefinition, String... beanNames) {
		return SpringUtils.addDependsOn(beanDefinition, beanNames);
	}

	protected Optional<Object> getPropertyValue(BeanDefinition beanDefinition, String propertyName) {
		return SpringUtils.getPropertyValue(beanDefinition, propertyName);
	}

	protected boolean isBeanDefinitionOfType(BeanDefinition beanDefinition, Class<?> type) {

		Assert.notNull(type, "Class type must not be null");

		return isBeanDefinitionOfType(beanDefinition, typeName -> type.getName().equals(typeName));
	}

	protected boolean isBeanDefinitionOfType(BeanDefinition beanDefinition, String typeName) {

		return isBeanDefinitionOfType(beanDefinition,
			typeNameArgument -> String.valueOf(typeName).equals(typeNameArgument));
	}

	protected boolean isBeanDefinitionOfType(BeanDefinition beanDefinition, Predicate<String> typeFilter) {

		return Optional.of(beanDefinition)
			.map(it -> beanDefinition.getBeanClassName())
			.filter(StringUtils::hasText)
			.map(typeFilter::test)
			.orElseGet(() ->
				Optional.ofNullable(beanDefinition.getFactoryMethodName())
					.filter(StringUtils::hasText)
					.filter(it -> beanDefinition instanceof AnnotatedBeanDefinition)
					.map(it -> ((AnnotatedBeanDefinition) beanDefinition).getFactoryMethodMetadata())
					.map(MethodMetadata::getReturnTypeName)
					.map(typeFilter::test)
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

	protected Predicate<String> isRegionBeanType() {

		Predicate<String> genericRegionBeanType =
			typeName -> GenericRegionFactoryBean.class.getName().equals(typeName);

		return genericRegionBeanType.or(typeName -> ClientRegionFactoryBean.class.getName().equals(typeName))
			.or(typeName -> LocalRegionFactoryBean.class.getName().equals(typeName))
			.or(typeName -> PartitionedRegionFactoryBean.class.getName().equals(typeName))
			.or(typeName -> ReplicatedRegionFactoryBean.class.getName().equals(typeName));
	}
}
