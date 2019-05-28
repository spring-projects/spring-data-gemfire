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
package org.springframework.data.gemfire.config.annotation;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.data.gemfire.DiskStoreFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer;
import org.springframework.lang.Nullable;

/**
 * Composition of {@link DiskStoreConfigurer}.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.DiskStoreFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer
 * @since 2.2.0
 */
public class LazyResolvingComposableDiskStoreConfigurer
		extends AbstractLazyResolvingComposableConfigurer<DiskStoreFactoryBean, DiskStoreConfigurer>
		implements DiskStoreConfigurer {

	public static LazyResolvingComposableDiskStoreConfigurer create() {
		return create(null);
	}

	public static LazyResolvingComposableDiskStoreConfigurer create(@Nullable BeanFactory beanFactory) {
		return new LazyResolvingComposableDiskStoreConfigurer().with(beanFactory);
	}

	@Override
	protected Class<DiskStoreConfigurer> getConfigurerType() {
		return DiskStoreConfigurer.class;
	}
}
