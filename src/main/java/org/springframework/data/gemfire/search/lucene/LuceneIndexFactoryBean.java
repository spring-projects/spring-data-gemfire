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

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeList;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeMap;
import static org.springframework.util.CollectionUtils.isEmpty;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneIndexFactory;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.geode.cache.lucene.LuceneServiceProvider;
import org.apache.lucene.analysis.Analyzer;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.config.annotation.IndexConfigurer;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.data.gemfire.util.CacheUtils;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} used to construct, configure and initialize {@link LuceneIndex Lucene Indexes}
 * on application domain object fields.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.lucene.LuceneIndexFactory
 * @see org.apache.geode.cache.lucene.LuceneService
 * @see org.apache.geode.cache.lucene.LuceneServiceProvider
 * @see org.apache.lucene.analysis.Analyzer
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public class LuceneIndexFactoryBean extends AbstractFactoryBeanSupport<LuceneIndex>
		implements DisposableBean, InitializingBean {

	protected static final boolean DEFAULT_DESTROY = false;

	private boolean destroy = DEFAULT_DESTROY;

	private GemFireCache gemfireCache;

	private List<IndexConfigurer> indexConfigurers = Collections.emptyList();

	private IndexConfigurer compositeIndexConfigurer = new IndexConfigurer() {

		@Override
		public void configure(String beanName, LuceneIndexFactoryBean bean) {
			nullSafeCollection(indexConfigurers).forEach(indexConfigurer -> indexConfigurer.configure(beanName, bean));
		}
	};

	private List<String> fields;

	private LuceneIndex luceneIndex;

	private LuceneService luceneService;

	private Map<String, Analyzer> fieldAnalyzers;

	private Region<?, ?> region;

	private String indexName;
	private String regionPath;

	@Override
	public void afterPropertiesSet() throws Exception {

		String indexName = getIndexName();

		applyIndexConfigurers(indexName);

		this.gemfireCache = resolveCache();
		this.luceneService = resolveLuceneService();
		this.regionPath = resolveRegionPath();

		setLuceneIndex(resolveLuceneIndex(indexName, getRegionPath()));
	}

	/**
	 * Applies the composite {@link IndexConfigurer IndexConfigurers} to apply addition configuration
	 * to this {@link LuceneIndexFactoryBean}.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex}.
	 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
	 * @see #applyIndexConfigurers(String, IndexConfigurer...)
	 * @see #getCompositeRegionConfigurer()
	 */
	private void applyIndexConfigurers(String indexName) {
		applyIndexConfigurers(indexName, getCompositeRegionConfigurer());
	}

	/**
	 * Applies the given array of {@link IndexConfigurer IndexConfigurers} to this {@link LuceneIndexFactoryBean}.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex}.
	 * @param indexConfigurers array of {@link IndexConfigurer IndexConfigurers} applied
	 * to this {@link LuceneIndexFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
	 * @see #applyIndexConfigurers(String, Iterable)
	 */
	protected void applyIndexConfigurers(String indexName, IndexConfigurer... indexConfigurers) {
		applyIndexConfigurers(indexName, Arrays.asList(nullSafeArray(indexConfigurers, IndexConfigurer.class)));
	}

	/**
	 * Applies the given {@link Iterable} of {@link IndexConfigurer IndexConfigurers}
	 * to this {@link LuceneIndexFactoryBean}.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex}.
	 * @param indexConfigurers {@link Iterable} of {@link IndexConfigurer IndexConfigurers} applied
	 * to this {@link LuceneIndexFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
	 */
	protected void applyIndexConfigurers(String indexName, Iterable<IndexConfigurer> indexConfigurers) {
		stream(nullSafeIterable(indexConfigurers).spliterator(), false)
			.forEach(indexConfigurer -> indexConfigurer.configure(indexName, this));
	}

	/**
	 * Creates a {@link LuceneIndex} with the given {@code indexName} on the {@link GemFireCache} {@link Region}
	 * identified by the given {@code regionPath}.
	 *
	 * @param indexName {@link String} containing the name for the {@link LuceneIndex}.
	 * @param regionPath {@link String} containing the fully-qualified pathname to
	 * the {@link GemFireCache} {@link Region}.
	 * @return a new instance of {@link LuceneIndex} with the given {@code indexName} on the named {@link Region}.
	 * @see org.apache.geode.cache.lucene.LuceneService#createIndexFactory()
	 * @see org.apache.geode.cache.lucene.LuceneService#getIndex(String, String)
	 * @see org.apache.geode.cache.lucene.LuceneIndexFactory#create(String, String)
	 * @see #resolveLuceneService()
	 * @see #postProcess(LuceneIndexFactory)
	 * @see #getFieldAnalyzers()
	 * @see #getFields()
	 * @see #resolveFields(List)
	 */
	protected LuceneIndex createLuceneIndex(String indexName, String regionPath) {

		LuceneService luceneService = resolveLuceneService();

		LuceneIndexFactory indexFactory = luceneService.createIndexFactory();

		Map<String, Analyzer> fieldAnalyzers = getFieldAnalyzers();

		if (isEmpty(fieldAnalyzers)) {
			indexFactory.setFields(asArray(resolveFields(getFields())));
		}
		else {
			indexFactory.setFields(fieldAnalyzers);
		}

		indexFactory = postProcess(indexFactory);
		indexFactory.create(indexName, regionPath);

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
	 * Performs additional post processing to the newly created {@link LuceneIndexFactory}.
	 *
	 * @param luceneIndexFactory {@link LuceneIndexFactory} to post process.
	 * @return the given {@link LuceneIndexFactory}.
	 * @see org.apache.geode.cache.lucene.LuceneIndexFactory
	 */
	protected LuceneIndexFactory postProcess(LuceneIndexFactory luceneIndexFactory) {
		return luceneIndexFactory;
	}

	/**
	 * Performs additional post processing to the newly created {@link LuceneIndex}.
	 *
	 * @param luceneIndex {@link LuceneIndex} created by this {@link LuceneIndexFactoryBean}.
	 * @return the given {@link LuceneIndex}.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 */
	protected LuceneIndex postProcess(LuceneIndex luceneIndex) {
		return luceneIndex;
	}

	/**
	 * Attempts to resolve a {@link LuceneIndex} by the given {@link String indexName} first then attempts to create
	 * the {@link LuceneIndex} with the given {@link Region#getFullPath() Region path}.
	 *
	 * @param indexName {@link String name} of the {@link LuceneIndex} to resolve.
	 * @param regionPath {@link Region#getFullPath() Region path} on which the {@link LuceneIndex} is applied.
	 * @return the resolved {@link LuceneIndex} by the given {@link String indexName} or the created {@link LuceneIndex}
	 * with the given {@link Region#getFullPath() Region path} if the {@link LuceneIndex} could not be resolved by
	 * {@link String indexName}.
	 * @see org.apache.geode.cache.lucene.LuceneService#getIndex(String, String)
	 * @see #createLuceneIndex(String, String)
	 * @see #getLuceneIndex()
	 */
	protected LuceneIndex resolveLuceneIndex(String indexName, String regionPath) {

		Supplier<LuceneIndex> luceneIndexSupplier = () ->
			Optional.ofNullable(resolveLuceneService())
				.map(luceneService -> luceneService.getIndex(indexName, regionPath))
				.orElseGet(() -> postProcess(createLuceneIndex(indexName, regionPath)));

		return getLuceneIndex().orElseGet(luceneIndexSupplier);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	@SuppressWarnings("all")
	public void destroy() throws Exception {

		LuceneIndex luceneIndex = getObject();

		if (isLuceneIndexDestroyable(luceneIndex)) {
			resolveLuceneService().destroyIndex(luceneIndex.getName(), luceneIndex.getRegionPath());
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
		return luceneIndex != null && isDestroy();
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

		return Optional.ofNullable(this.luceneIndex)
			.<Class<?>>map(LuceneIndex::getClass)
			.orElse(LuceneIndex.class);
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
		return !isEmpty(fields) ? fields : Collections.singletonList(LuceneService.REGION_VALUE_FIELD);
	}

	/**
	 * Resolves the appropriate {@link LuceneIndexFactory} from the {@link LuceneService}.
	 *
	 * @return a newly created instance of the {@link LuceneIndexFactory} from the resolved {@link LuceneService}.
	 * @see #resolveLuceneService()
	 */
	protected LuceneIndexFactory resolveLuceneIndexFactory() {
		return resolveLuceneService().createIndexFactory();
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

		return Optional.ofNullable(getLuceneService())
			.orElseGet(() -> Optional.ofNullable(getBeanFactory())
				.map(it -> SpringUtils.safeGetValue(() -> it.getBean(LuceneService.class), (LuceneService) null))
				.orElseGet(() -> resolveLuceneService(resolveCache())));
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

			return cache != null && StringUtils.hasText(regionPath) ? cache.getRegion(regionPath) : null;
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
			.map(Region::getFullPath)
			.orElseGet(this::getRegionPath);

		Assert.state(StringUtils.hasText(regionPath), "Either Region or regionPath must be specified");

		return regionPath;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setBeanName(String name) {
		super.setBeanName(name);
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
	 * Returns a reference to the Composite {@link IndexConfigurer} used to apply additional configuration
	 * to this {@link LuceneIndexFactoryBean} on Spring container initialization.
	 *
	 * @return the Composite {@link IndexConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
	 */
	protected IndexConfigurer getCompositeRegionConfigurer() {
		return this.compositeIndexConfigurer;
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
	 * Null-safe operation to set an array of {@link IndexConfigurer IndexConfigurers} used to apply
	 * additional configuration to this {@link LuceneIndexFactoryBean} when using Annotation-based configuration.
	 *
	 * @param indexConfigurers array of {@link IndexConfigurer IndexConfigurers} used to apply
	 * additional configuration to this {@link LuceneIndexFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
	 * @see #setIndexConfigurers(List)
	 */
	public void setIndexConfigurers(IndexConfigurer... indexConfigurers) {
		setIndexConfigurers(Arrays.asList(nullSafeArray(indexConfigurers, IndexConfigurer.class)));
	}

	/**
	 * Null-safe operation to set an {@link Iterable} of {@link IndexConfigurer IndexConfigurers} used to apply
	 * additional configuration to this {@link LuceneIndexFactoryBean} when using Annotation-based configuration.
	 *
	 * @param indexConfigurers {@link Iterable } of {@link IndexConfigurer IndexConfigurers} used to apply
	 * additional configuration to this {@link LuceneIndexFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
	 */
	public void setIndexConfigurers(List<IndexConfigurer> indexConfigurers) {
		this.indexConfigurers = Optional.ofNullable(indexConfigurers).orElseGet(Collections::emptyList);
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
	 * Returns an {@link Optional} reference to the {@link LuceneIndex} created by this {@link LuceneIndexFactoryBean}.
	 *
	 * @return an {@link Optional} reference to the {@link LuceneIndex} created by this {@link LuceneIndexFactoryBean}.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 * @see java.util.Optional
	 */
	public Optional<LuceneIndex> getLuceneIndex() {
		return Optional.ofNullable(this.luceneIndex);
	}

	/**
	 * Sets the given {@link LuceneIndex} as the index created by this {@link FactoryBean}.
	 *
	 * This method is generally used for testing purposes only.
	 *
	 * @param luceneIndex {@link LuceneIndex} created by this {@link FactoryBean}.
	 * @return this {@link LuceneIndexFactoryBean}.
	 * @see org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 */
	public LuceneIndexFactoryBean setLuceneIndex(LuceneIndex luceneIndex) {

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
