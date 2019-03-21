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

package org.springframework.data.gemfire.cache.config;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.annotation.Import;

/**
 * The {@link EnableGemfireCaching} annotation enables Pivotal GemFire or Apache Geode as a caching provider
 * in Spring's Cache Abstraction.
 *
 * @author John Blum
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.Target
 * @see org.springframework.context.annotation.Import
 * @see <a href="http://docs.spring.io/spring/docs/current/spring-framework-reference/htmlsingle/#cache">Cache Abstraction</a>
 * @see <a href="http://docs.spring.io/spring/docs/current/spring-framework-reference/htmlsingle/#cache-store-configuration-gemfire">GemFire-based Cache</a>
 * @see <a href="http://docs.spring.io/spring-data-gemfire/docs/current/reference/html/#apis:spring-cache-abstraction">Support for Spring Cache Abstraction</a>
 * @since 2.0.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(GemfireCachingConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableGemfireCaching {

}
