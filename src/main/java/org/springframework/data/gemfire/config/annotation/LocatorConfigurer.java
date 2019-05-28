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

import org.apache.geode.distributed.Locator;

import org.springframework.data.gemfire.LocatorFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.Configurer;

/**
 * A Spring Configurer used to apply additional, customized configuration for an Apache Geode or Pivotal GemFire
 * {@link Locator}.
 *
 * This Configurer is particularly useful when using {@link LocatorApplication} annotation to configure and bootstrap
 * an Apache Geode or Pivotal GemFire {@link Locator}.
 *
 * This Configurer is NOT applied when configuring an enabling an embedded {@link Locator}
 * using SDG's {@link @EnableLocator} annotation.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see org.apache.geode.distributed.Locator
 * @see org.springframework.data.gemfire.LocatorFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.support.Configurer
 * @since 2.2.0
 */
@FunctionalInterface
public interface LocatorConfigurer extends Configurer<LocatorFactoryBean> {

}
