/*
 * Copyright 2017-2018 the original author or authors.
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

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.support.GemfireBeanFactoryLocator;

/**
 * The {@link BeanFactoryLocatorConfiguration} class extends the Spring application configuration by enabling
 * Spring Data GemFire/Geode's {@link GemfireBeanFactoryLocator} in order to auto-wire and configure any
 * GemFire/Geode Objects declared in GemFire/Geode config (e.g. XML or properties).
 *
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EnableBeanFactoryLocator
 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator
 * @since 2.0.0
 */
@Configuration
@SuppressWarnings("unused")
public class BeanFactoryLocatorConfiguration {

	/**
	 * Declares and registers a Spring {@link BeanPostProcessor} and post processes a Spring Data GemFire/Geode
	 * {@link CacheFactoryBean} by setting the {@literal useBeanFactoryLocator} property to {@literal true}.
	 *
	 * @return a Spring {@link BeanPostProcessor} used to post process the SDG {@link CacheFactoryBean}.
	 * @see org.springframework.beans.factory.config.BeanPostProcessor
	 */
	@Bean
	public BeanPostProcessor cacheFactoryBeanPostProcessor() {

		return new BeanPostProcessor() {

			@Override
			@SuppressWarnings("all")
			public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

				if (bean instanceof CacheFactoryBean) {
					((CacheFactoryBean) bean).setUseBeanFactoryLocator(true);
				}

				return bean;
			}
		};
	}
}
