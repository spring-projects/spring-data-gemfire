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

import org.apache.geode.cache.client.Pool;

import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.Configurer;

/**
 * The {@link PoolConfigurer} interface defines a contract for implementing {@link Object Objects} in order to
 * customize the configuration of a {@link PoolFactoryBean} used to construct, configure and initialize a {@link Pool}.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EnablePool
 * @see org.springframework.data.gemfire.config.annotation.EnablePools
 * @see org.springframework.data.gemfire.config.annotation.support.Configurer
 * @since 2.0.0
 */
@FunctionalInterface
public interface PoolConfigurer extends Configurer<PoolFactoryBean> {

}
