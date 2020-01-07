/*
 * Copyright 2018-2020 the original author or authors.
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
package org.springframework.data.gemfire.config.annotation;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer;
import org.springframework.lang.Nullable;

/**
 * Composition of {@link PoolConfigurer}.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.PoolConfigurer
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer
 * @since 2.2.0
 */
public class LazyResolvingComposablePoolConfigurer
		extends AbstractLazyResolvingComposableConfigurer<PoolFactoryBean, PoolConfigurer>
		implements PoolConfigurer {

	public static LazyResolvingComposablePoolConfigurer create() {
		return create(null);
	}

	public static LazyResolvingComposablePoolConfigurer create(@Nullable BeanFactory beanFactory) {
		return new LazyResolvingComposablePoolConfigurer().with(beanFactory);
	}

	@Override
	protected Class<PoolConfigurer> getConfigurerType() {
		return PoolConfigurer.class;
	}
}
