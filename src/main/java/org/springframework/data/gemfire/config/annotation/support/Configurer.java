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
package org.springframework.data.gemfire.config.annotation.support;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.core.Ordered;

/**
 * The {@link Configurer} interface defines a contract for implementing objects that can modify
 * some aspect of configuration given a reference to the Spring {@link FactoryBean} responsible
 * for the configuration of some {@link Object} or Spring bean.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.core.Ordered
 * @since 2.2.0
 */
@FunctionalInterface
public interface Configurer<T extends FactoryBean<?>> extends Ordered {

	/**
	 * Applies additional configuration to the given Spring {@link FactoryBean}.
	 *
	 * @param beanName {@link String} containing the name of the Spring bean.
	 * @param factoryBean {@link FactoryBean} used to construct, configure and initialize the {@link Object}
	 * or Spring Bean.
	 */
	void configure(String beanName, T factoryBean);

	/**
	 * Defines the {@literal order} of this {@link Configurer} bean relative to other {@link Configurer Configurers}.
	 *
	 * @return the {@literal order} in which this {@link Configurer} is applied.
	 */
	@Override
	default int getOrder() {
		return 0;
	}
}
