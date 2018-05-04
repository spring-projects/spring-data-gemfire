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

package org.springframework.data.gemfire.search.lucene;

import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.geode.cache.lucene.LuceneServiceProvider;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.util.Assert;

/**
 * Spring {@link FactoryBean} used to get an instance of the {@link GemFireCache} {@link LuceneService}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.lucene.LuceneService
 * @see org.apache.geode.cache.lucene.LuceneServiceProvider
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public class LuceneServiceFactoryBean extends AbstractFactoryBeanSupport<LuceneService> implements InitializingBean {

	private GemFireCache gemfireCache;

	private LuceneService luceneService;

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterPropertiesSet() throws Exception {

		GemFireCache gemfireCache = getCache();

		Assert.state(gemfireCache != null,
			"A reference to the GemFireCache was not properly configured");

		this.luceneService = resolveLuceneService(gemfireCache);
	}

	/**
	 * Attempts to resolve the Singleton instance of the {@link GemFireCache} {@link LuceneService}
	 * from given the {@link GemFireCache}.
	 *
	 * @param gemFireCache {@link GemFireCache} used to resolve the {@link LuceneService} instance.
	 * @return a single instance of the GemFire {@link LuceneService}.
	 * @see org.apache.geode.cache.GemFireCache
	 * @see org.apache.geode.cache.lucene.LuceneService
	 */
	protected LuceneService resolveLuceneService(GemFireCache gemFireCache) {
		return LuceneServiceProvider.get(gemfireCache);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public LuceneService getObject() throws Exception {
		return this.luceneService;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Class<?> getObjectType() {

		return Optional.ofNullable(this.luceneService)
			.<Class<?>>map(LuceneService::getClass)
			.orElse(LuceneService.class);
	}

	/**
	 * Sets a reference to the single {@link GemFireCache} instance.
	 *
	 * @param gemfireCache {@link GemFireCache} reference.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	public void setCache(GemFireCache gemfireCache) {
		this.gemfireCache = gemfireCache;
	}

	/**
	 * Returns a reference to the single {@link GemFireCache} instance.
	 *
	 * @return a reference to the single {@link GemFireCache} instance.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	protected GemFireCache getCache() {
		return this.gemfireCache;
	}
}
