/*
 * Copyright 2018 the original author or authors.
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
import org.springframework.data.gemfire.ConfigurableRegionFactoryBean;
import org.springframework.data.gemfire.PeerRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer;
import org.springframework.lang.Nullable;

/**
 * Composition for {@link RegionConfigurer}.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.data.gemfire.ConfigurableRegionFactoryBean
 * @see org.springframework.data.gemfire.PeerRegionFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer
 * @since 2.2.0
 */
public class LazyResolvingComposableRegionConfigurer
		extends AbstractLazyResolvingComposableConfigurer<ConfigurableRegionFactoryBean<?, ?>, RegionConfigurer>
		implements RegionConfigurer {

	public static LazyResolvingComposableRegionConfigurer create() {
		return create(null);
	}

	public static LazyResolvingComposableRegionConfigurer create(@Nullable BeanFactory beanFactory) {
		return new LazyResolvingComposableRegionConfigurer().with(beanFactory);
	}

	@Override
	protected Class<RegionConfigurer> getConfigurerType() {
		return RegionConfigurer.class;
	}

	@Override
	public void configure(String beanName, ClientRegionFactoryBean<?, ?> bean) {
		resolveConfigurers().forEach(configurer -> configurer.configure(beanName, bean));
	}

	@Override
	public void configure(String beanName, PeerRegionFactoryBean<?, ?> peerRegionFactoryBean) {
		resolveConfigurers().forEach(configurer -> configurer.configure(beanName, peerRegionFactoryBean));
	}
}
