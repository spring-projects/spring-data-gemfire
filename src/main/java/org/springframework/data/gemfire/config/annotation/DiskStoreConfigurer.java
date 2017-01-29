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

import org.apache.geode.cache.DiskStore;
import org.springframework.data.gemfire.DiskStoreFactoryBean;

/**
 * The {@link DiskStoreConfigurer} interface defines a contract for implementations to customize the configuration
 * of a {@link DiskStoreFactoryBean} used to construct, configure and initialize a {@link DiskStore}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.DiskStore
 * @see org.springframework.data.gemfire.DiskStoreFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStore
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStores
 * @since 1.1.0
 */
public interface DiskStoreConfigurer {

	/**
	 * Configuration callback method providing a reference to a {@link DiskStoreFactoryBean} used to construct,
	 * configure and initialize an instance of {@link DiskStore}.
	 *
	 * @param beanName name of the {@link DiskStore} bean declared in the Spring application context.
	 * @param bean reference to the {@link DiskStoreFactoryBean}.
	 * @see org.springframework.data.gemfire.DiskStoreFactoryBean
	 */
	void configure(String beanName, DiskStoreFactoryBean bean);

}
