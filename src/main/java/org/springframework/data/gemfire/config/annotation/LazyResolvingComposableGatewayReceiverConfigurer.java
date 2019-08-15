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
import org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer;
import org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean;
import org.springframework.lang.Nullable;

/**
 * Composition of {@link GatewayReceiverConfigurer}.
 *
 * @author Udo Kohlmeyer
 * @see org.springframework.data.gemfire.config.annotation.GatewayReceiverConfigurer
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer
 * @see org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean
 * @since 2.2.0
 */
public class LazyResolvingComposableGatewayReceiverConfigurer
		extends AbstractLazyResolvingComposableConfigurer<GatewayReceiverFactoryBean, GatewayReceiverConfigurer>
		implements GatewayReceiverConfigurer {

	public static LazyResolvingComposableGatewayReceiverConfigurer create() {
		return create(null);
	}

	public static LazyResolvingComposableGatewayReceiverConfigurer create(@Nullable BeanFactory beanFactory) {
		return new LazyResolvingComposableGatewayReceiverConfigurer().with(beanFactory);
	}

	@Override
	protected Class<GatewayReceiverConfigurer> getConfigurerType() {
		return GatewayReceiverConfigurer.class;
	}
}
