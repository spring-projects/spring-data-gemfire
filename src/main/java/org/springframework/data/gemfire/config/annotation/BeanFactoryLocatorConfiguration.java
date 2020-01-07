/*
 * Copyright 2017-2020 the original author or authors.
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

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.support.GemfireBeanFactoryLocator;

/**
 * The {@link BeanFactoryLocatorConfiguration} class extends the Spring application configuration by enabling
 * SDG's {@link GemfireBeanFactoryLocator} in order to auto-wire and configure any Pivotal GemFire/Apache Geode objects
 * declared in Pivotal GemFire/Apache Geode config (e.g. XML or properties).
 *
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
 * @see org.springframework.data.gemfire.config.annotation.EnableBeanFactoryLocator
 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator
 * @since 2.0.0
 */
@Configuration
@SuppressWarnings("unused")
public class BeanFactoryLocatorConfiguration {

	/**
	 * Declares and registers a Spring {@link BeanPostProcessor} bean to post process a Spring Data Geode
	 * {@link CacheFactoryBean} or {@link ClientCacheFactoryBean} by setting the {@literal useBeanFactoryLocator}
	 * property to {@literal true}.
	 *
	 * @return a Spring {@link BeanPostProcessor} used to post process an SDG {@link CacheFactoryBean}.
	 * @see org.springframework.beans.factory.config.BeanPostProcessor
	 */
	@Bean
	public BeanPostProcessor useBeanFactoryLocatorBeanPostProcessor() {

		return new BeanPostProcessor() {

			@Override
			public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

				if (bean instanceof CacheFactoryBean) {
					((CacheFactoryBean) bean).setUseBeanFactoryLocator(true);
				}

				return bean;
			}
		};
	}

	/**
	 * Declares and registers a {@link ClientCacheConfigurer} bean to configure a {@link ClientCacheFactoryBean}
	 * by setting the {@literal useBeanFactoryLocator} property to {@literal true}.
	 *
	 * @return a {@link ClientCacheConfigurer} used to configure and set the SDG {@link ClientCacheFactoryBean}'s
	 * {@literal useBeanFactoryLocator} property to {@literal true}.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
	 */
	@Bean
	public ClientCacheConfigurer useBeanFactoryLocatorClientCacheConfigurer() {
		return (beanName, bean) -> bean.setUseBeanFactoryLocator(true);
	}

	/**
	 * Declares and registers a {@link PeerCacheConfigurer} bean to configure a {@link CacheFactoryBean}
	 * by setting the {@literal useBeanFactoryLocator} property to {@literal true}.
	 *
	 * @return a {@link PeerCacheConfigurer} used to configure and set the SDG {@link CacheFactoryBean}'s
	 * {@literal useBeanFactoryLocator} property to {@literal true}.
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
	 */
	@Bean
	public PeerCacheConfigurer useBeanFactoryLocatorPeerCacheConfigurer() {
		return (beanName, bean) -> bean.setUseBeanFactoryLocator(true);
	}
}
