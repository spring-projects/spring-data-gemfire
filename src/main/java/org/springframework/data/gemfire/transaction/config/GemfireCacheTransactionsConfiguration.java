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

package org.springframework.data.gemfire.transaction.config;

import org.apache.geode.cache.GemFireCache;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.transaction.GemfireTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 * The {@link GemfireCacheTransactionsConfiguration} class is a Spring {@link Configuration @Configuration} class
 * used to enable Spring's Transaction Management infrastructure along with SDG's {@link GemfireTransactionManager}
 * to manage local, cache transactions for either Pivotal GemFire or Apache Geode.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.transaction.GemfireTransactionManager
 * @see org.springframework.transaction.annotation.EnableTransactionManagement
 * @since 2.0.0
 */
@Configuration
@EnableTransactionManagement
@SuppressWarnings("unused")
public class GemfireCacheTransactionsConfiguration {

	/**
	 * Declares and registers SDG's {@link GemfireTransactionManager} as the {@literal transactionManager}
	 * in Spring's Transaction Management infrastructure to manage local, GemFire/Geode cache transactions.
	 *
	 * @param gemfireCache reference to the {@link GemFireCache}.
	 * @return a new instance of {@link GemfireTransactionManager} initialized with the given {@link GemFireCache}.
	 * @see org.springframework.data.gemfire.transaction.GemfireTransactionManager
	 * @see org.apache.geode.cache.GemFireCache
	 */
	@Bean
	public GemfireTransactionManager transactionManager(GemFireCache gemfireCache) {
		return new GemfireTransactionManager(gemfireCache);
	}
}
