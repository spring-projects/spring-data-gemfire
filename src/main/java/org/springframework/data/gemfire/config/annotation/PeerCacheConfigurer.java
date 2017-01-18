/*
 * Copyright 2016 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import org.apache.geode.cache.Cache;
import org.springframework.data.gemfire.CacheFactoryBean;

/**
 * The {@link PeerCacheConfigurer} interface defines a contract for implementations to customize the configuration
 * of a {@link CacheFactoryBean} used to construct, configure and initialize an instance of a peer {@link Cache}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Cache
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
 * @since 1.9.0
 */
public interface PeerCacheConfigurer {

	/**
	 * Configuration callback method providing a reference to a {@link CacheFactoryBean} used to construct,
	 * configure and initialize an instance of a peer {@link Cache}.
	 *
	 * @param beanName name of peer {@link Cache} bean declared in the Spring application context.
	 * @param bean reference to the {@link CacheFactoryBean}.
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 */
	void configure(String beanName, CacheFactoryBean bean);

}
