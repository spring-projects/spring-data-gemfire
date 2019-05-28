/*
 * Copyright 2017-2019 the original author or authors.
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

import org.apache.geode.cache.Region;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.PeerRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.CacheTypeAwareRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.Configurer;

/**
 * The {@link RegionConfigurer} interface defines a contract for implementations to customize the configuration
 * of Entity-defined {@link Region Regions} when a user annotates her Spring application {@link Configuration}
 * class with {@link EnableEntityDefinedRegions}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.PeerRegionFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EnableCachingDefinedRegions
 * @see org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions
 * @see org.springframework.data.gemfire.config.annotation.support.Configurer
 * @see CacheTypeAwareRegionFactoryBean
 * @since 2.0.0
 */
public interface RegionConfigurer extends Configurer<ClientRegionFactoryBean<?, ?>> {

	/**
	 * Configuration callback method providing a reference to a {@link PeerRegionFactoryBean} used to construct, configure
	 * and initialize an instance of a peer {@link Region}.
	 *
	 * @param beanName name of {@link Region} bean declared in the Spring application context.
	 * @param bean reference to the {@link PeerRegionFactoryBean}.
	 * @see PeerRegionFactoryBean
	 */
	default void configure(String beanName, PeerRegionFactoryBean<?, ?> bean) { }

	/**
	 * Configuration callback method providing a reference to a {@link ClientRegionFactoryBean} used to construct,
	 * configure and initialize an instance of a client {@link Region}.
	 *
	 * @param beanName name of {@link Region} bean declared in the Spring application context.
	 * @param bean reference to the {@link ClientRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
	 */
	default void configure(String beanName, ClientRegionFactoryBean<?, ?> bean) { }

}
