/*
 * Copyright 2011-2013 the original author or authors.
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

package org.springframework.data.gemfire;

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexExistsException;
import org.apache.geode.cache.query.IndexInvalidException;
import org.apache.geode.cache.query.IndexNameConflictException;
import org.apache.geode.cache.query.IndexStatistics;
import org.apache.geode.cache.query.QueryService;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.data.gemfire.config.annotation.IndexConfigurer;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} used to construct, configure and initialize {@link Index Indexes}
 * using a declarative approach.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionService
 * @see org.apache.geode.cache.query.Index
 * @see org.apache.geode.cache.query.QueryService
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.data.gemfire.IndexFactoryBean.IndexWrapper
 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 * @since 1.0.0
 */
public class IndexFactoryBean extends AbstractFactoryBeanSupport<Index> implements InitializingBean {

	private boolean define = false;
	private boolean override = true;

	private Index index;

	private IndexType indexType;

	//@Autowired(required = false)
	private List<IndexConfigurer> indexConfigurers = Collections.emptyList();

	private IndexConfigurer compositeIndexConfigurer = new IndexConfigurer() {

		@Override
		public void configure(String beanName, IndexFactoryBean bean) {
			nullSafeCollection(indexConfigurers).forEach(indexConfigurer -> indexConfigurer.configure(beanName, bean));
		}
	};

	private QueryService queryService;

	private RegionService cache;

	private String expression;
	private String from;
	private String imports;
	private String indexName;
	private String name;

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterPropertiesSet() throws Exception {

		this.indexName = Optional.ofNullable(this.name).filter(StringUtils::hasText).orElse(getBeanName());

		Assert.hasText(this.indexName, "Index name is required");

		applyIndexConfigurers(this.indexName);

		Assert.notNull(cache, "Cache is required");

		this.queryService = lookupQueryService();

		Assert.notNull(queryService, "QueryService is required to create an Index");
		Assert.hasText(expression, "Index expression is required");
		Assert.hasText(from, "Index from clause is required");

		if (IndexType.isKey(indexType)) {
			Assert.isNull(imports, "imports are not supported with a KEY Index");
		}

		this.index = createIndex(queryService, indexName);
	}

	/* (non-Javadoc) */
	private void applyIndexConfigurers(String indexName) {
		applyIndexConfigurers(indexName, getCompositeRegionConfigurer());
	}

	/**
	 * Null-safe operation to apply the given array of {@link IndexConfigurer IndexConfigurers}
	 * to this {@link IndexFactoryBean}.
	 *
	 * @param indexName {@link String} containing the name of the {@link Index}.
	 * @param indexConfigurers array of {@link IndexConfigurer IndexConfigurers} applied
	 * to this {@link IndexFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 * @see #applyIndexConfigurers(String, Iterable)
	 */
	protected void applyIndexConfigurers(String indexName, IndexConfigurer... indexConfigurers) {
		applyIndexConfigurers(indexName, Arrays.asList(nullSafeArray(indexConfigurers, IndexConfigurer.class)));
	}

	/**
	 * Null-safe operation to apply the given {@link Iterable} of {@link IndexConfigurer IndexConfigurers}
	 * to this {@link IndexFactoryBean}.
	 *
	 * @param indexName {@link String} containing the name of the {@link Index}.
	 * @param indexConfigurers {@link Iterable} of {@link IndexConfigurer IndexConfigurers} applied
	 * to this {@link IndexFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 */
	protected void applyIndexConfigurers(String indexName, Iterable<IndexConfigurer> indexConfigurers) {
		stream(nullSafeIterable(indexConfigurers).spliterator(), false)
			.forEach(indexConfigurer -> indexConfigurer.configure(indexName, this));
	}

	/* (non-Javadoc) */
	QueryService doLookupQueryService() {
		return Optional.ofNullable(this.queryService).orElseGet(() ->
			(this.cache instanceof ClientCache ? ((ClientCache) this.cache).getLocalQueryService()
				: this.cache.getQueryService()));
	}

	/* (non-Javadoc) */
	QueryService lookupQueryService() {
		if (getBeanFactory().containsBean(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE)) {
			return getBeanFactory().getBean(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE,
				QueryService.class);
		}
		else {
			return registerQueryServiceBean(doLookupQueryService());
		}
	}

	/* (non-Javadoc) */
	QueryService registerQueryServiceBean(QueryService queryService) {
		if (isDefine()) {
			((ConfigurableBeanFactory) getBeanFactory()).registerSingleton(
				GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE, queryService);
		}

		return queryService;
	}

	/* (non-Javadoc) */
	Index createIndex(QueryService queryService, String indexName) throws Exception {

		Index existingIndex = getExistingIndex(queryService, indexName);

		if (existingIndex != null) {
			if (override) {
				queryService.removeIndex(existingIndex);
			}
			else {
				return existingIndex;
			}
		}

		try {
			if (IndexType.isKey(indexType)) {
				return createKeyIndex(queryService, indexName, expression, from);
			}
			else if (IndexType.isHash(indexType)) {
				return createHashIndex(queryService, indexName, expression, from, imports);
			}
			else {
				return createFunctionalIndex(queryService, indexName, expression, from, imports);
			}
		}
		catch (IndexExistsException e) {
			throw new GemfireIndexException(String.format(
				"An Index with a different name having the same definition as this Index (%1$s) already exists",
					indexName), e);
		}
		catch (IndexNameConflictException e) {
			// NOTE technically, the only way for an IndexNameConflictException to be thrown is if
			// queryService.remove(existingIndex) above silently fails, since otherwise, when override is 'false',
			// the existingIndex is already being returned.  Given this state of affairs, an Index with the provided
			// name is unresolvable based on what the user intended to happen, so just rethrow an Exception.
			throw new GemfireIndexException(String.format(
				"Failed to remove the existing Index%1$sbefore re-creating Index with name (%2$s)",
					(override ? " on override " : " "), indexName), e);
		}
		catch (Exception e) {
			if (existingIndex != null) {
				Collection<Index> indexes = queryService.getIndexes();

				if (CollectionUtils.isEmpty(indexes) || !indexes.contains(existingIndex)) {
					queryService.getIndexes().add(existingIndex);
					return existingIndex;
				}
			}

			throw e;
		}
	}

	/* (non-Javadoc) */
	Index createKeyIndex(QueryService queryService, String indexName, String expression, String from) throws Exception {

		if (isDefine()) {
			queryService.defineKeyIndex(indexName, expression, from);
			return new IndexWrapper(queryService, indexName);
		}
		else {
			return queryService.createKeyIndex(indexName, expression, from);
		}
	}

	/* (non-Javadoc) */
	Index createHashIndex(QueryService queryService, String indexName, String expression, String from,
			String imports) throws Exception {

		boolean hasImports = StringUtils.hasText(imports);

		if (isDefine()) {
			if (hasImports) {
				queryService.defineHashIndex(indexName, expression, from, imports);
			}
			else {
				queryService.defineHashIndex(indexName, expression, from);
			}

			return new IndexWrapper(queryService, indexName);
		}
		else {
			if (hasImports) {
				return queryService.createHashIndex(indexName, expression, from, imports);
			}
			else {
				return queryService.createHashIndex(indexName, expression, from);
			}
		}
	}

	/* (non-Javadoc) */
	Index createFunctionalIndex(QueryService queryService, String indexName, String expression, String from,
			String imports) throws Exception {

		boolean hasImports = StringUtils.hasText(imports);

		if (isDefine()) {
			if (hasImports) {
				queryService.defineIndex(indexName, expression, from , imports);
			}
			else {
				queryService.defineIndex(indexName, expression, from);
			}

			return new IndexWrapper(queryService, indexName);
		}
		else {
			if (hasImports) {
				return queryService.createIndex(indexName, expression, from, imports);
			}
			else {
				return queryService.createIndex(indexName, expression, from);
			}
		}
	}

	/* (non-Javadoc) */
	Index getExistingIndex(QueryService queryService, String indexName) {

		for (Index index : nullSafeCollection(queryService.getIndexes())) {
			if (index.getName().equalsIgnoreCase(indexName)) {
				return index;
			}
		}

		return null;
	}

	/**
	 * Returns a reference to the Composite {@link IndexConfigurer} used to apply additional configuration
	 * to this {@link IndexFactoryBean} on Spring container initialization.
	 *
	 * @return the Composite {@link IndexConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
	 */
	protected IndexConfigurer getCompositeRegionConfigurer() {
		return this.compositeIndexConfigurer;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Index getObject() {
		return Optional.ofNullable(this.index)
			.orElseGet(() -> this.index = getExistingIndex(queryService, indexName));
	}

	/**
	 * @inheritDoc
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Class<?> getObjectType() {
		return Optional.ofNullable(this.index).map(Index::getClass).orElse((Class) Index.class);
	}

	/**
	 * Sets the underlying cache used for creating indexes.
	 *
	 * @param cache cache used for creating indexes.
	 */
	public void setCache(RegionService cache) {
		this.cache = cache;
	}

	/**
	 * Sets the query service used for creating indexes.
	 *
	 * @param service query service used for creating indexes.
	 */
	public void setQueryService(QueryService service) {
		this.queryService = service;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Sets a boolean condition to indicate whether the Index declared and defined by this FactoryBean will only be
	 * defined initially, or defined and created.  If defined-only, the IndexFactoryBean will receive a callback at
	 * the end of the Spring container lifecycle to subsequently "create" all "defined-only" Indexes once, in a
	 * single operation.
	 *
	 * @param define a boolean value indicating the define or define/create status.  If true, the Index declared
	 * by this FactoryBean will only be defined initially and subsequently created when this SmartLifecycle bean
	 * receives an appropriate callback from the Spring container; if false, the Index will be created immediately.
	 */
	public void setDefine(boolean define) {
		this.define = define;
	}

	/* (non-Javadoc) */
	protected boolean isDefine() {
		return define;
	}

	/**
	 * @param expression the expression to set
	 */
	public void setExpression(String expression) {
		this.expression = expression;
	}

	/**
	 * @param from the from to set
	 */
	public void setFrom(String from) {
		this.from = from;
	}

	/**
	 * @param imports the imports to set
	 */
	public void setImports(String imports) {
		this.imports = imports;
	}

	/**
	 * Null-safe operation to set an array of {@link IndexConfigurer IndexConfigurers} used to apply
	 * additional configuration to this {@link IndexFactoryBean} when using Annotation-based configuration.
	 *
	 * @param indexConfigurers array of {@link IndexConfigurer IndexConfigurers} used to apply
	 * additional configuration to this {@link IndexFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
	 * @see #setIndexConfigurers(List)
	 */
	public void setIndexConfigurers(IndexConfigurer... indexConfigurers) {
		setIndexConfigurers(Arrays.asList(nullSafeArray(indexConfigurers, IndexConfigurer.class)));
	}

	/**
	 * Null-safe operation to set an {@link Iterable} of {@link IndexConfigurer IndexConfigurers} used to apply
	 * additional configuration to this {@link IndexFactoryBean} when using Annotation-based configuration.
	 *
	 * @param indexConfigurers {@link Iterable } of {@link IndexConfigurer IndexConfigurers} used to apply
	 * additional configuration to this {@link IndexFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
	 */
	public void setIndexConfigurers(List<IndexConfigurer> indexConfigurers) {
		this.indexConfigurers = Optional.ofNullable(indexConfigurers).orElseGet(Collections::emptyList);
	}

	/**
	 * @param override the override to set
	 */
	public void setOverride(boolean override) {
		this.override = override;
	}

	/**
	 * @param type the type to set
	 */
	public void setType(String type) {
		setType(IndexType.valueOfIgnoreCase(type));
	}

	/**
	 * Sets the type of GemFire Index to create.
	 *
	 * @param indexType the IndexType enumerated value indicating the type of GemFire Index
	 * that will be created by this Spring FactoryBean.
	 * @see org.springframework.data.gemfire.IndexType
	 */
	public void setType(IndexType indexType) {
		this.indexType = indexType;
	}

	/* (non-Javadoc) */
	protected static final class IndexWrapper implements Index {

		private Index index;

		private final QueryService queryService;

		private final String indexName;

		protected IndexWrapper(QueryService queryService, String indexName) {
			Assert.notNull(queryService, "QueryService must not be null");
			Assert.hasText(indexName, "The name of the Index must be specified!");
			this.queryService = queryService;
			this.indexName = indexName;

		}

		protected synchronized Index getIndex() {
			if (this.index == null) {
				String localIndexName = getIndexName();

				for (Index localIndex : getQueryService().getIndexes()) {
					if (localIndex.getName().equals(localIndexName)) {
						this.index = localIndex;
						break;
					}
				}

				if (this.index == null) {
					throw new GemfireIndexException(new IndexInvalidException(String.format(
						"index with name (%1$s) was not found", localIndexName)));
				}
			}

			return index;
		}

		protected String getIndexName() {
			Assert.state(StringUtils.hasText(indexName), "The Index 'name' was not properly initialized!");
			return indexName;
		}

		protected QueryService getQueryService() {
			return queryService;
		}

		@Override
		public String getName() {
			return getIndex().getName();
		}

		@Override
		public String getCanonicalizedFromClause() {
			return getIndex().getCanonicalizedFromClause();
		}

		@Override
		public String getCanonicalizedIndexedExpression() {
			return getIndex().getCanonicalizedIndexedExpression();
		}

		@Override
		public String getCanonicalizedProjectionAttributes() {
			return getIndex().getCanonicalizedProjectionAttributes();
		}

		@Override
		public String getFromClause() {
			return getIndex().getFromClause();
		}

		@Override
		public String getIndexedExpression() {
			return getIndex().getIndexedExpression();
		}

		@Override
		public String getProjectionAttributes() {
			return getIndex().getProjectionAttributes();
		}

		@Override
		public Region<?, ?> getRegion() {
			return getIndex().getRegion();
		}

		@Override
		public IndexStatistics getStatistics() {
			return getIndex().getStatistics();
		}

		@Override
		@SuppressWarnings("deprecation")
		public org.apache.geode.cache.query.IndexType getType() {
			return getIndex().getType();
		}

		@Override
		public boolean equals(Object obj) {
			if (obj == this) {
				return true;
			}

			if (!(obj instanceof IndexWrapper || obj instanceof Index)) {
				return false;
			}

			if (obj instanceof IndexWrapper) {
				return (getIndexName().equals(((IndexWrapper) obj).getIndexName()));
			}

			return getIndex().equals(obj);
		}

		@Override
		public int hashCode() {
			int hashValue = 37;
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getIndexName());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(index);
			return hashValue;
		}

		@Override
		public String toString() {
			return (index != null ? String.valueOf(index) : getIndexName());
		}
	}
}
