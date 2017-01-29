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

package org.springframework.data.gemfire.cache.config;

import org.apache.geode.cache.GemFireCache;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.cache.GemfireCacheManager;

/**
 * The {@link GemfireCachingConfiguration} class is a Spring {@link Configuration @Configuration} class
 * used to configure Pivotal GemFire or Apache Geode as the caching provider in Spring's Cache Abstraction.
 *
 * This {@link Configuration @Configuration} class is specifically responsible for declaring and registering
 * Spring Data GemFire/Geode's {@link GemfireCacheManager} implementation to properly enable either Pivotal GemFire
 * or Apache Geode as the caching provider used with Springs Cache Abstraction.
 *
 * Additionally, this Spring {@link Configuration @Configuration} class also enables the Spring Cache Abstraction
 * by declaring Spring's {@link EnableCaching} annotation for the user extending or importing this class using
 * the SDG provided {@link EnableGemfireCaching} annotation.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.springframework.cache.annotation.EnableCaching
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.cache.GemfireCacheManager
 * @see org.springframework.data.gemfire.cache.config.EnableGemfireCaching
 * @see <a href="http://docs.spring.io/spring/docs/current/spring-framework-reference/htmlsingle/#cache">Cache Abstraction</a>
 * @see <a href="http://docs.spring.io/spring/docs/current/spring-framework-reference/htmlsingle/#cache-store-configuration-gemfire">GemFire-based Cache</a>
 * @see <a href="http://docs.spring.io/spring-data-gemfire/docs/current/reference/html/#apis:spring-cache-abstraction">Support for Spring Cache Abstraction</a>
 * @since 2.0.0
 */
@Configuration
@EnableCaching
@SuppressWarnings("unused")
public class GemfireCachingConfiguration {

	/**
	 * SDG's {@link GemfireCacheManager} used to position Pivotal GemFire or Apache Geode as the caching provider
	 * in Spring's Cache Abstraction.
	 *
	 * @return an instance of {@link GemfireCacheManager}.
	 * @see org.springframework.data.gemfire.cache.GemfireCacheManager
	 * @see org.apache.geode.cache.GemFireCache
	 */
	@Bean
	public GemfireCacheManager cacheManager(GemFireCache gemfireCache) {

		GemfireCacheManager gemfireCacheManager = new GemfireCacheManager();

		gemfireCacheManager.setCache(gemfireCache);

		return gemfireCacheManager;
	}
}
