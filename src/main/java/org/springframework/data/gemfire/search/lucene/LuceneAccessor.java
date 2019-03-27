/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.search.lucene;

import static org.springframework.data.gemfire.util.SpringUtils.safeGetValue;

import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneQuery;
import org.apache.geode.cache.lucene.LuceneQueryException;
import org.apache.geode.cache.lucene.LuceneQueryFactory;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.geode.cache.lucene.LuceneServiceProvider;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.gemfire.search.lucene.support.LuceneOperationsSupport;
import org.springframework.data.gemfire.util.CacheUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * {@link LuceneAccessor} is an abstract class supporting implementations of the {@link LuceneOperations} interface
 * encapsulating common functionality necessary to execute Lucene queries.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.data.gemfire.search.lucene.support.LuceneOperationsSupport
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.lucene.LuceneQuery
 * @see org.apache.geode.cache.lucene.LuceneQueryFactory
 * @see org.apache.geode.cache.lucene.LuceneService
 * @see org.apache.geode.cache.lucene.LuceneServiceProvider
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public abstract class LuceneAccessor extends LuceneOperationsSupport implements InitializingBean {

	private GemFireCache gemfireCache;

	private LuceneIndex luceneIndex;

	private LuceneService luceneService;

	private Region<?, ?> region;

	private String indexName;
	private String regionPath;

	/**
	 * Constructs an uninitialized instance of {@link LuceneAccessor}.
	 */
	public LuceneAccessor() {
	}

	/**
	 * Constructs an instance of the {@link LuceneAccessor} initialized with the given {@link LuceneIndex}
	 * used to perform Lucene queries (searches).
	 *
	 * @param luceneIndex {@link LuceneIndex} used in Lucene queries.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 */
	public LuceneAccessor(LuceneIndex luceneIndex) {
		this.luceneIndex = luceneIndex;
	}

	/**
	 * Constructs an instance of the {@link LuceneAccessor} initialized with the given Lucene index name
	 * and {@link Region} reference upon which Lucene queries are executed.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex} used in Lucene queries.
	 * @param region {@link Region} on which Lucene queries are executed.
	 * @see org.apache.geode.cache.Region
	 */
	public LuceneAccessor(String indexName, Region<?, ?> region) {
		this.indexName = indexName;
		this.region = region;
	}

	/**
	 * Constructs an instance of the {@link LuceneAccessor} initialized with the given Lucene index name
	 * and {@link Region} reference upon which Lucene queries are executed.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex} used in Lucene queries.
	 * @param regionPath {@link String} containing the name of the {@link Region} on which Lucene queries are executed.
	 */
	public LuceneAccessor(String indexName, String regionPath) {
		this.indexName = indexName;
		this.regionPath = regionPath;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		this.gemfireCache = resolveCache();
		this.luceneService = resolveLuceneService();
		this.indexName = resolveIndexName();
		this.regionPath = resolveRegionPath();
	}

	/**
	 * Creates an instance of the {@link LuceneQueryFactory} to create and execute {@link LuceneQuery Lucene queries}.
	 *
	 * @return an instance of the {@link LuceneQueryFactory} to create and execute {@link LuceneQuery Lucene queries}.
	 * @see org.apache.geode.cache.lucene.LuceneQueryFactory
	 * @see #getLuceneService()
	 */
	public LuceneQueryFactory createLuceneQueryFactory() {
		return resolveLuceneService().createLuceneQueryFactory();
	}

	/**
	 * Creates an instance of the {@link LuceneQueryFactory} to create and execute {@link LuceneQuery Lucene queries}.
	 *
	 * @param resultLimit limit to the number of results returned by the query.
	 * @return an instance of the {@link LuceneQueryFactory} to create and execute {@link LuceneQuery Lucene queries}.
	 * @see org.apache.geode.cache.lucene.LuceneQueryFactory
	 * @see #createLuceneQueryFactory(int, int)
	 */
	public LuceneQueryFactory createLuceneQueryFactory(int resultLimit) {
		return createLuceneQueryFactory(resultLimit, DEFAULT_PAGE_SIZE);
	}

	/**
	 * Creates an instance of the {@link LuceneQueryFactory} to create and execute {@link LuceneQuery Lucene queries}.
	 *
	 * @param resultLimit limit to the number of results returned by the query.
	 * @param pageSize number of results appearing on a single page.
	 * @return an instance of the {@link LuceneQueryFactory} to create and execute {@link LuceneQuery Lucene queries}.
	 * @see #createLuceneQueryFactory()
	 */
	public LuceneQueryFactory createLuceneQueryFactory(int resultLimit, int pageSize) {
		return createLuceneQueryFactory().setLimit(resultLimit).setPageSize(pageSize);
	}

	/**
	 * Resolves a reference to the {@link GemFireCache}.
	 *
	 * @return a reference to the single instance of the {@link GemFireCache}.
	 * @see org.springframework.data.gemfire.util.CacheUtils#resolveGemFireCache()
	 * @see org.apache.geode.cache.GemFireCache
	 * @see #getCache()
	 */
	protected GemFireCache resolveCache() {
		return Optional.ofNullable(getCache()).orElseGet(CacheUtils::resolveGemFireCache);
	}

	/**
	 * Resolves the {@link LuceneService} used by this data access object to perform Lucene queries.
	 *
	 * @return a reference to the {@link GemFireCache} {@link LuceneService}.
	 * @see org.apache.geode.cache.lucene.LuceneService
	 * @see #getLuceneService()
	 * @see #resolveCache()
	 * @see #resolveLuceneService(GemFireCache)
	 */
	protected LuceneService resolveLuceneService() {
		return Optional.ofNullable(getLuceneService()).orElseGet(() -> resolveLuceneService(resolveCache()));
	}

	/**
	 * Resolves the {@link LuceneService} used by this data access object to perform Lucene queries.
	 *
	 * @param gemfireCache {@link GemFireCache} used to resolve the {@link LuceneService}.
	 * @return a reference to the {@link GemFireCache} {@link LuceneService}.
	 * @throws IllegalArgumentException if {@link GemFireCache} is {@literal null}.
	 * @see org.apache.geode.cache.lucene.LuceneService
	 * @see org.apache.geode.cache.GemFireCache
	 */
	protected LuceneService resolveLuceneService(GemFireCache gemfireCache) {
		Assert.notNull(gemfireCache, "Cache reference was not properly configured");
		return LuceneServiceProvider.get(gemfireCache);
	}

	/**
	 * Resolves the name of the {@link LuceneIndex} required in the Lucene data access, query operations
	 * when a {@link LuceneIndex} is not specifically provided.
	 *
	 * @return the name of the resolve {@link LuceneIndex}.
	 * @throws IllegalStateException if the name of the {@link LuceneIndex} cannot be resolved.
	 * @see org.apache.geode.cache.lucene.LuceneIndex#getName()
	 * @see #getIndexName()
	 * @see #getLuceneIndex()
	 */
	protected String resolveIndexName() {
		String resolvedIndexName = Optional.ofNullable(getIndexName())
			.orElseGet(() -> safeGetValue(() -> getLuceneIndex().getName()));

		Assert.state(StringUtils.hasText(resolvedIndexName),
			"The name of the Lucene Index could not be resolved");

		return resolvedIndexName;
	}

	/**
	 * Resolves the fully-qualified pathname of the {@link Region} to which the Lucene data access, query operations
	 * are performed and the {@link LuceneIndex} is applied, when a {@link String region path}
	 * is not specifically provided.
	 *
	 * @return a {@link String} containing the fully-qualified pathname of the {@link Region} on which the Lucene
	 * data access, query operations are performed and the {@link LuceneIndex} is applied.
	 * @throws IllegalStateException if the fully-qualified pathname of the {@link Region} cannot be resolved.
	 * @see #getRegionPath()
	 * @see #getRegion()
	 */
	protected String resolveRegionPath() {
		String resolvedRegionPath = Optional.ofNullable(getRegionPath())
			.orElseGet(() -> safeGetValue(() -> getRegion().getFullPath()));

		Assert.state(StringUtils.hasText(resolvedRegionPath), "Region path could not be resolved");

		return resolvedRegionPath;
	}

	/**
	 * Sets a reference to the {@link GemFireCache}.
	 *
	 * @param <T> {@link Class} type of the {@link LuceneAccessor}.
	 * @param gemfireCache {@link GemFireCache} reference.
	 * @return this {@link LuceneAccessor}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	@SuppressWarnings("unchecked")
	public <T extends LuceneAccessor> T setCache(GemFireCache gemfireCache) {
		this.gemfireCache = gemfireCache;
		return (T) this;
	}

	/**
	 * Returns a reference to the {@link GemFireCache}.
	 *
	 * @return a reference to the {@link GemFireCache}.
	 * @see org.apache.geode.cache.GemFireCache
	 * @see #resolveCache()
	 */
	protected GemFireCache getCache() {
		return this.gemfireCache;
	}

	/**
	 * Sets the name of the {@link LuceneIndex} used in Lucene queries.
	 *
	 * @param <T> {@link Class} type of the {@link LuceneAccessor}.
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex}.
	 * @return this {@link LuceneAccessor}.
	 * @see #setLuceneIndex(LuceneIndex)
	 */
	@SuppressWarnings("unchecked")
	public <T extends LuceneAccessor> T setIndexName(String indexName) {
		this.indexName = indexName;
		return (T) this;
	}

	/**
	 * Returns the name of the {@link LuceneIndex} used in Lucene queries.
	 *
	 * @return a {@link String} containing the name of the {@link LuceneIndex}.
	 * @see #resolveIndexName()
	 * @see #getLuceneIndex()
	 */
	public String getIndexName() {
		return this.indexName;
	}

	/**
	 * Sets a reference to the {@link LuceneIndex} used in Lucene queries.
	 *
	 * @param <T> {@link Class} type of the {@link LuceneAccessor}.
	 * @param luceneIndex {@link LuceneIndex} used in Lucene data access, query operations.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 * @return this {@link LuceneAccessor}.
	 * @see #setIndexName(String)
	 */
	@SuppressWarnings("unchecked")
	public <T extends LuceneAccessor> T setLuceneIndex(LuceneIndex luceneIndex) {
		this.luceneIndex = luceneIndex;
		return (T) this;
	}

	/**
	 * Returns a reference to the {@link LuceneIndex} used in Lucene queries.
	 *
	 * @return a {@link LuceneIndex} used in Lucene data access, query operations.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 * @see #resolveIndexName()
	 * @see #getIndexName()
	 */
	public LuceneIndex getLuceneIndex() {
		return this.luceneIndex;
	}

	/**
	 * Sets a reference to the {@link LuceneService} used to perform Lucene query data access operations.
	 *
	 * @param <T> {@link Class} type of the {@link LuceneAccessor}.
	 * @param luceneService {@link LuceneService} used to perform Lucene queries.
	 * @return this {@link LuceneAccessor}.
	 * @see org.apache.geode.cache.lucene.LuceneService
	 */
	@SuppressWarnings("unchecked")
	public <T extends LuceneAccessor> T setLuceneService(LuceneService luceneService) {
		this.luceneService = luceneService;
		return (T) this;
	}

	/**
	 * Returns a reference to the {@link LuceneService} used to perform Lucene query data access operations.
	 *
	 * @return a reference to the {@link LuceneService} used to perform Lucene queries.
	 * @see org.apache.geode.cache.lucene.LuceneService
	 */
	protected LuceneService getLuceneService() {
		return this.luceneService;
	}

	/**
	 * Sets a reference to the {@link Region} used to specify Lucene queries.
	 *
	 * @param <T> {@link Class} type of the {@link LuceneAccessor}.
	 * @param region {@link Region} used to specify Lucene queries.
	 * @return this {@link LuceneAccessor}.
	 * @see org.apache.geode.cache.Region
	 * @see #setRegionPath(String)
	 */
	@SuppressWarnings("unchecked")
	public <T extends LuceneAccessor> T setRegion(Region<?, ?> region) {
		this.region = region;
		return (T) this;
	}

	/**
	 * Returns a reference to the {@link Region} used to specify Lucene queries.
	 *
	 * @return a {@link Region} used to specify Lucene queries.
	 * @see org.apache.geode.cache.Region
	 * @see #resolveRegionPath()
	 * @see #getRegionPath()
	 */
	public Region<?, ?> getRegion() {
		return this.region;
	}

	/**
	 * Sets a fully-qualified pathname to the {@link Region} used to specify Lucene queries.
	 *
	 * @param <T> {@link Class} type of the {@link LuceneAccessor}.
	 * @param regionPath {@link String} containing the fully-qualified pathname to the {@link Region}
	 * used to specify Lucene queries.
	 * @return this {@link LuceneAccessor}.
	 * @see #setRegion(Region)
	 */
	@SuppressWarnings("unchecked")
	public <T extends LuceneAccessor> T setRegionPath(String regionPath) {
		this.regionPath = regionPath;
		return (T) this;
	}

	/**
	 * Returns a fully-qualified pathname to the {@link Region} used to specify Lucene queries.
	 *
	 * @return a {@link String} containing the fully-qualified pathname to the {@link Region}
	 * used to specify Lucene queries.
	 * @see #resolveRegionPath()
	 * @see #getRegion()
	 */
	public String getRegionPath() {
		return this.regionPath;
	}

	/* (non-Javadoc) */
	protected <T> T doFind(LuceneQueryExecutor<T> queryExecutor, Object query, String regionPath, String indexName) {
		try {
			return queryExecutor.execute();
		}
		catch (LuceneQueryException e) {
			throw new DataRetrievalFailureException(String.format(
				"Failed to execute Lucene Query [%1$s] on Region [%2$s] with Lucene Index [%3$s]",
					query, regionPath, indexName), e);
		}
	}

	/* (non-Javadoc) */
	@FunctionalInterface
	protected interface LuceneQueryExecutor<T> {
		T execute() throws LuceneQueryException;
	}
}
