/*
 * Copyright 2020 the original author or authors.
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
 */
package org.springframework.data.gemfire.client.support;

import org.apache.geode.cache.client.Pool;

import org.apache.shiro.util.StringUtils;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.data.gemfire.client.PoolResolver;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;

/**
 * {@link PoolResolver} implementation that uses the Spring {@link BeanFactory} to resolve managed {@link Pool} objects.
 * This means the {@link Pool} was configured and initialized by the Spring container given the {@link Pool} would be a
 * proper bean declaration in this case.
 *
 * @author John Blum
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.data.gemfire.client.PoolResolver
 * @since 2.3.0
 */
public class BeanFactoryPoolResolver implements BeanFactoryAware, PoolResolver {

	private BeanFactory beanFactory;

	/**
	 * Constructs a new instance of the {@link BeanFactoryPoolResolver} initialized with
	 * the given Spring {@link BeanFactory} used to resolve managed {@link Pool} objects.
	 *
	 * @param beanFactory Spring {@link BeanFactory} used to resolve managed {@link Pool} objects.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	public BeanFactoryPoolResolver(@NonNull BeanFactory beanFactory) {
		setBeanFactory(beanFactory);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public final void setBeanFactory(@NonNull BeanFactory beanFactory) throws BeansException {

		Assert.notNull(beanFactory, "BeanFactory must not be null");

		this.beanFactory = beanFactory;
	}

	/**
	 * Returns a reference to the configured Spring {@link BeanFactory} used to resolve managed {@link Pool} objects.
	 *
	 * @return a reference to the configured Spring {@link BeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory getBeanFactory() {
		return this.beanFactory;
	}

	/**
	 * Resolves the managed, {@link String named} Apache Geode {@link Pool} from the Spring {@link BeanFactory}.
	 *
	 * @param poolName {@link String name} of the {@link Pool} to resolve.
	 * @return the resolved, {@link String named}, managed {@link Pool} object or {@literal null} if no {@link Pool}
	 * with the given {@link String name} could be resolved from the configured Spring {@link BeanFactory}.
	 * @see org.apache.geode.cache.client.Pool
	 * @see #getBeanFactory()
	 */
	@Nullable @Override
	public Pool resolve(@Nullable String poolName) {

		BeanFactory beanFactory = getBeanFactory();

		return StringUtils.hasText(poolName) && beanFactory.containsBean(poolName)
			? beanFactory.getBean(poolName, Pool.class)
			: null;
	}
}
