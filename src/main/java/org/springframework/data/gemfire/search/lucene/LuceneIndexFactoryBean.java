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

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeList;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeMap;
import static org.springframework.util.CollectionUtils.isEmpty;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.geode.cache.lucene.LuceneServiceProvider;
import org.apache.lucene.analysis.Analyzer;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.util.CacheUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} used to construct {@link LuceneIndex Lucene Indexes} on application domain object fields.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.lucene.LuceneService
 * @see org.apache.geode.cache.lucene.LuceneServiceProvider
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public class LuceneIndexFactoryBean implements FactoryBean<LuceneIndex>,
		BeanFactoryAware, BeanNameAware, InitializingBean, DisposableBean {

	protected static final boolean DEFAULT_DESTROY = false;

	private boolean destroy = DEFAULT_DESTROY;

	private BeanFactory beanFactory;

	private GemFireCache gemfireCache;

	private List<String> fields;

	private LuceneIndex luceneIndex;

	private LuceneService luceneService;

	private Map<String, Analyzer> fieldAnalyzers;

	private Region<?, ?> region;

	private String indexName;
	private String regionPath;

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		String indexName = getIndexName();

		this.gemfireCache = resolveCache();
		this.luceneService = resolveLuceneService();
		this.regionPath = resolveRegionPath();

		setLuceneIndex(createIndex(indexName, getRegionPath()));
	}

	/**
	 * Creates a {@link LuceneIndex} with the given {@code indexName} on the {@link GemFireCache} {@link Region}
	 * identified by the given {@code regionPath}.
	 *
	 * @param indexName {@link String} containing the name for the {@link LuceneIndex}.
	 * @param regionPath {@link String} containing the fully-qualified pathname to
	 * the {@link GemFireCache} {@link Region}.
	 * @return a new instance of {@link LuceneIndex} with the given {@code indexName} on the named {@link Region}.
	 * @see org.apache.geode.cache.lucene.LuceneService#createIndex(String, String, Map)
	 * @see org.apache.geode.cache.lucene.LuceneService#createIndex(String, String, String...)
	 * @see org.apache.geode.cache.lucene.LuceneService#getIndex(String, String)
	 * @see #resolveLuceneService()
	 * @see #getFieldAnalyzers()
	 * @see #getFields()
	 * @see #resolveFields(List)
	 */
	protected LuceneIndex createIndex(String indexName, String regionPath) {
		LuceneService luceneService = resolveLuceneService();

		Map<String, Analyzer> fieldAnalyzers = getFieldAnalyzers();

		if (isEmpty(fieldAnalyzers)) {
			luceneService.createIndex(indexName, regionPath, asArray(resolveFields(getFields())));
		}
		else {
			luceneService.createIndex(indexName, regionPath, fieldAnalyzers);
		}

		return luceneService.getIndex(indexName, regionPath);
	}

	/**
	 * Converts the {@link List} of {@link String Strings} into an {@link String[]} array.
	 *
	 * @param list {@link List} to convert into a typed array.
	 * @return a {@link String[]} array for the {@link List} of {@link String Strings}.
	 * @see java.util.List#toArray(Object[])
	 */
	private String[] asArray(List<String> list) {
		return list.toArray(new String[list.size()]);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	@SuppressWarnings("deprecation")
	public void destroy() throws Exception {
		LuceneIndex luceneIndex = getObject();

		if (isLuceneIndexDestroyable(luceneIndex)) {
			resolveLuceneService().destroyIndex(luceneIndex);
		}
	}

	/**
	 * Determine whether the given {@link LuceneIndex} created by this {@link FactoryBean} is destroyable.
	 *
	 * @param luceneIndex {@link LuceneIndex} subject to destruction.
	 * @return a boolean value indicating whether the given {@link LuceneIndex} created by this {@link FactoryBean}
	 * is destroyable.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 * @see #isDestroy()
	 */
	protected boolean isLuceneIndexDestroyable(LuceneIndex luceneIndex) {
		return (luceneIndex != null && isDestroy());
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public LuceneIndex getObject() throws Exception {
		if (this.luceneIndex == null) {
			setLuceneIndex(Optional.ofNullable(resolveLuceneService())
				.map((luceneService) -> luceneService.getIndex(getIndexName(), resolveRegionPath()))
					.orElse(null));
		}

		return this.luceneIndex;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Class<?> getObjectType() {
		return Optional.ofNullable(this.luceneIndex).<Class<?>>map(LuceneIndex::getClass).orElse(LuceneIndex.class);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean isSingleton() {
		return true;
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
	 * Resolves the {@link List} of fields on the object to index.
	 *
	 * @param fields {@link List} of fields to evaluate.
	 * @return a resolve {@link List} of object fields to index.
	 */
	protected List<String> resolveFields(List<String> fields) {
		return (!isEmpty(fields) ? fields : Collections.singletonList(LuceneService.REGION_VALUE_FIELD));
	}

	/**
	 * Resolves the {@link LuceneService} used by this {@link FactoryBean} to create the {@link LuceneIndex}.
	 *
	 * @return a reference to the {@link GemFireCache}, {@link LuceneService}.
	 * @see org.springframework.beans.factory.BeanFactory#getBean(Class)
	 * @see org.apache.geode.cache.lucene.LuceneService
	 * @see #getBeanFactory()
	 * @see #getLuceneService()
	 * @see #resolveCache()
	 * @see #resolveLuceneService(GemFireCache)
	 */
	protected LuceneService resolveLuceneService() {
		return Optional.ofNullable(getLuceneService()).orElseGet(() ->
			Optional.ofNullable(getBeanFactory()).map(beanFactory -> {
				try {
					return beanFactory.getBean(LuceneService.class);
				}
				catch (BeansException ignore) {
					return null;
				}
			}).orElseGet(() -> resolveLuceneService(resolveCache())));
	}

	/**
	 * Resolves the {@link LuceneService} used by this {@link FactoryBean} to create the {@link LuceneIndex}.
	 *
	 * @param gemfireCache {@link GemFireCache} used to resolve the {@link LuceneService}.
	 * @return a reference to the {@link GemFireCache} {@link LuceneService}.
	 * @throws IllegalArgumentException if {@link GemFireCache} is {@literal null}.
	 * @see org.apache.geode.cache.lucene.LuceneService
	 * @see org.apache.geode.cache.GemFireCache
	 */
	protected LuceneService resolveLuceneService(GemFireCache gemfireCache) {
		Assert.notNull(gemfireCache, "A reference to the GemFireCache was not properly configured");
		return LuceneServiceProvider.get(gemfireCache);
	}

	/**
	 * Attempts to resolve the {@link GemFireCache} {@link Region} on which the {@link LuceneIndex} will be created.
	 *
	 * @return a reference to the {@link GemFireCache} {@link Region} on which he {@link LuceneIndex} will be created.
	 * Returns {@literal null} if the {@link Region} cannot be resolved.
	 * @see org.apache.geode.cache.RegionService#getRegion(String)
	 * @see org.apache.geode.cache.Region
	 * @see #resolveCache()
	 * @see #getRegionPath()
	 */
	protected Region<?, ?> resolveRegion() {
		return Optional.ofNullable(getRegion()).orElseGet(() -> {
			GemFireCache cache = resolveCache();
			String regionPath = getRegionPath();

			return (cache != null && StringUtils.hasText(regionPath) ? cache.getRegion(regionPath) : null);
		});
	}

	/**
	 * Resolves the fully-qualified pathname of the {@link GemFireCache} {@link Region} on which the {@link LuceneIndex}
	 * will be created.
	 *
	 * @return a {@link String} containing the fully-qualified pathname of the {@link GemFireCache} {@link Region}
	 * on which the {@link LuceneIndex} will be created.
	 * @throws IllegalStateException if the {@link Region} pathname could not resolved.
	 * @see #resolveRegion()
	 * @see #getRegionPath()
	 */
	protected String resolveRegionPath() {
		String regionPath = Optional.ofNullable(resolveRegion())
			.map(Region::getFullPath).orElseGet(this::getRegionPath);

		Assert.state(StringUtils.hasText(regionPath), "Either Region or regionPath must be specified");

		return regionPath;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/**
	 * Returns a reference to the containing Spring {@link BeanFactory} if set.
	 *
	 * @return a reference to the containing Spring {@link BeanFactory} if set.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory getBeanFactory() {
		return this.beanFactory;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setBeanName(String name) {
		setIndexName(name);
	}

	/**
	 * Sets a reference to the {@link GemFireCache}.
	 *
	 * @param gemfireCache {@link GemFireCache} reference.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	public void setCache(GemFireCache gemfireCache) {
		this.gemfireCache = gemfireCache;
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
	 * Sets whether to destroy the {@link LuceneIndex} on shutdown.
	 *
	 * @param destroy boolean value indicating whether to destroy the {@link LuceneIndex} on shutdown.
	 */
	public void setDestroy(boolean destroy) {
		this.destroy = destroy;
	}

	/**
	 * Determines whether the {@link LuceneIndex} will be destroyed on shutdown.
	 *
	 * @return a boolean value indicating whether the {@link LuceneIndex} will be destroyed on shutdown.
	 * @see org.springframework.beans.factory.DisposableBean#destroy()
	 */
	protected boolean isDestroy() {
		return this.destroy;
	}

	/**
	 * Set a {@link Map} of application domain object field names to {@link Analyzer Analyzers} used in the construction
	 * of the {@link LuceneIndex Lucene Indexes} for each field.
	 *
	 * @param fieldAnalyzers {@link Map} of fields names to {@link Analyzer Analyzers}.
	 * @see org.apache.lucene.analysis.Analyzer
	 * @see java.util.Map
	 */
	public void setFieldAnalyzers(Map<String, Analyzer> fieldAnalyzers) {
		this.fieldAnalyzers = fieldAnalyzers;
	}

	/**
	 * Returns a {@link Map} of application domain object field names to {@link Analyzer Analyzers} used in
	 * the construction of the {@link LuceneIndex Lucene Indexes} for each field.
	 *
	 * @return a {@link Map} of fields names to {@link Analyzer Analyzers}.
	 * @see org.apache.lucene.analysis.Analyzer
	 * @see java.util.Map
	 * @see #getFields()
	 */
	protected Map<String, Analyzer> getFieldAnalyzers() {
		return nullSafeMap(this.fieldAnalyzers);
	}

	/**
	 * Sets the application domain object fields to index.
	 *
	 * @param fields array of {@link String Strings} containing the names of the object fields ot index.
	 * @see #setFields(List)
	 */
	public void setFields(String... fields) {
		setFields(Arrays.asList(nullSafeArray(fields, String.class)));
	}

	/**
	 * Sets the application domain object fields to index.
	 *
	 * @param fields {@link List} of {@link String Strings} containing the names of the object fields ot index.
	 */
	public void setFields(List<String> fields) {
		this.fields = fields;
	}

	/**
	 * Returns a {@link List} of application domain object fields to be indexed.
	 *
	 * @return a {@link List} of application domain object fields to be indexed.
	 * @see #getFieldAnalyzers()
	 * @see java.util.List
	 */
	protected List<String> getFields() {
		return nullSafeList(this.fields);
	}

	/**
	 * Sets the name of the {@link LuceneIndex} as identified in the {@link GemFireCache}.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex}.
	 * @see #setBeanName(String)
	 */
	public void setIndexName(String indexName) {
		this.indexName = indexName;
	}

	/**
	 * Returns the name of the {@link LuceneIndex} as identified in the {@link GemFireCache}.
	 *
	 * @return a {@link String} containing the name of the {@link LuceneIndex}.
	 * @throws IllegalStateException if the {@code indexName} was not specified.
	 */
	protected String getIndexName() {
		Assert.state(StringUtils.hasText(this.indexName), "indexName was not properly initialized");
		return this.indexName;
	}

	/**
	 * Sets the {@link LuceneIndex} as the index created by this {@link FactoryBean}.
	 *
	 * This method is for testing purposes only!
	 *
	 * @param luceneIndex {@link LuceneIndex} created by this {@link FactoryBean}.
	 * @return this {@link LuceneIndexFactoryBean}.
	 * @see org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 */
	protected LuceneIndexFactoryBean setLuceneIndex(LuceneIndex luceneIndex) {
		this.luceneIndex = luceneIndex;
		return this;
	}

	/**
	 * Sets a reference to the {@link LuceneService} used by this {@link FactoryBean} to create the {@link LuceneIndex}.
	 *
	 * @param luceneService {@link LuceneService} used to create the {@link LuceneIndex}.
	 * @see org.apache.geode.cache.lucene.LuceneService
	 */
	public void setLuceneService(LuceneService luceneService) {
		this.luceneService = luceneService;
	}

	/**
	 * Returns a reference to the {@link LuceneService} used by this {@link FactoryBean} to create
	 * the {@link LuceneIndex}.
	 *
	 * @return a reference to the {@link LuceneService} used to create the {@link LuceneIndex}.
	 * @see org.apache.geode.cache.lucene.LuceneService
	 * @see #resolveLuceneService()
	 */
	protected LuceneService getLuceneService() {
		return this.luceneService;
	}

	/**
	 * Sets a reference to the {@link GemFireCache} {@link Region} on which the {@link LuceneIndex} will be created.
	 *
	 * @param region {@link Region} on which the {@link LuceneIndex} will be created.
	 * @see org.apache.geode.cache.Region
	 * @see #setRegionPath(String)
	 */
	public void setRegion(Region<?, ?> region) {
		this.region = region;
	}

	/**
	 * Returns a reference to the {@link GemFireCache} {@link Region} on which the {@link LuceneIndex} will be created.
	 *
	 * @return a reference to the {@link Region} on which the {@link LuceneIndex} will be created.
	 * @see org.apache.geode.cache.Region
	 * @see #getRegionPath()
	 * @see #resolveRegion()
	 */
	protected Region<?, ?> getRegion() {
		return this.region;
	}

	/**
	 * Sets the fully-qualified pathname to the {@link GemFireCache} {@link Region} on which the {@link LuceneIndex}
	 * will be created.
	 *
	 * @param pathname {@link String} containing the fully-qualified pathname to the {@link GemFireCache} {@link Region}
	 * on which the {@link LuceneIndex} will be created.
	 * @see #setRegion(Region)
	 */
	public void setRegionPath(String pathname) {
		this.regionPath = pathname;
	}

	/**
	 * Returns the fully-qualified pathname to the {@link GemFireCache} {@link Region} on which the {@link LuceneIndex}
	 * will be created.
	 *
	 * @return a {@link String} containing the fully-qualified pathname to the {@link GemFireCache} {@link Region}
	 * on which the {@link LuceneIndex} will be created.
	 * @see #getRegion()
	 */
	protected String getRegionPath() {
		return this.regionPath;
	}
}
