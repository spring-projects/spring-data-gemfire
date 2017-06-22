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

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.SpringUtils.defaultIfNull;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionService;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.IndexExistsException;
import com.gemstone.gemfire.cache.query.IndexNameConflictException;
import com.gemstone.gemfire.cache.query.IndexStatistics;
import com.gemstone.gemfire.cache.query.QueryService;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} for simple and easy declarative creation of GemFire Indexes.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory
 * @see org.springframework.data.gemfire.IndexFactoryBean.IndexWrapper
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.RegionService
 * @see com.gemstone.gemfire.cache.query.Index
 * @see com.gemstone.gemfire.cache.query.QueryService
 * @since 1.0.0
 */
public class IndexFactoryBean extends AbstractFactoryBeanSupport<Index> implements InitializingBean {

	public static final String BASIC_INDEX_DEFINITION = "{ expression = '%1$s', from = '%2$s', type = %3$s }";

	public static final String DETAILED_INDEX_DEFINITION =
		"{ name = '%1$s', expression = '%2$s', from = '%3$s', imports = '%4$s', type = %5$s }";

	private boolean define = false;
	private boolean ignoreIfExists = false;
	private boolean override = false;

	private Index index;

	private IndexType indexType;

	private QueryService queryService;

	private RegionService cache;

	private String expression;
	private String from;
	private String imports;
	private String indexName;
	private String name;

	@Override
	public void afterPropertiesSet() throws Exception {

		this.cache = resolveCache();
		this.indexName = resolveIndexName();
		this.queryService = resolveQueryService();

		assertIndexDefinitionConfiguration();

		this.index = createIndex(this.queryService, this.indexName);

		registerAlias(getBeanName(), this.indexName);
	}

	/* (non-Javadoc) */
	private void assertIndexDefinitionConfiguration() {

		Assert.hasText(this.expression, "Index expression is required");
		Assert.hasText(this.from, "Index from clause is required");

		if (IndexType.isKey(this.indexType)) {
			Assert.isTrue(StringUtils.isEmpty(this.imports), "Imports are not supported with a KEY Index");
		}
	}

	/* (non-Javadoc) */
	RegionService resolveCache() {

		RegionService resolvedCache = (this.cache != null ? this.cache : GemfireUtils.resolveGemFireCache());

		Assert.state(resolvedCache != null, "Cache is required");

		return resolvedCache;
	}

	/* (non-Javadoc) */
	String resolveIndexName() {

		String resolvedIndexName = (StringUtils.hasText(this.name) ? this.name : getBeanName());

		Assert.hasText(resolvedIndexName, "Index name is required");

		return resolvedIndexName;
	}

	/* (non-Javadoc) */
	QueryService resolveQueryService() {

		QueryService resolvedQueryService = (this.queryService != null ? this.queryService : lookupQueryService());

		Assert.state(resolvedQueryService != null, "QueryService is required to create an Index");

		return resolvedQueryService;
	}

	/* (non-Javadoc) */
	QueryService lookupQueryService() {

		String queryServiceBeanName = GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE;

		BeanFactory beanFactory = getBeanFactory();

		return (beanFactory != null && beanFactory.containsBean(queryServiceBeanName)
			? beanFactory.getBean(queryServiceBeanName, QueryService.class)
			: registerQueryServiceBean(queryServiceBeanName, doLookupQueryService()));
	}

	/* (non-Javadoc) */
	QueryService doLookupQueryService() {

		return (queryService != null ? queryService
			: (cache instanceof ClientCache ? ((ClientCache) cache).getLocalQueryService()
				: cache.getQueryService()));
	}

	/* (non-Javadoc) */
	QueryService registerQueryServiceBean(String beanName, QueryService queryService) {

		BeanFactory beanFactory = getBeanFactory();

		if (beanFactory instanceof ConfigurableBeanFactory) {
			if (isDefine()) {
				((ConfigurableBeanFactory) beanFactory).registerSingleton(beanName, queryService);
			}
		}

		return queryService;
	}

	/* (non-Javadoc) */
	void registerAlias(String beanName, String indexName) {

		BeanFactory beanFactory = getBeanFactory();

		if (beanFactory instanceof ConfigurableBeanFactory) {
			if (beanName != null && !beanName.equals(indexName)) {
				((ConfigurableBeanFactory) beanFactory).registerAlias(beanName, indexName);
			}
		}
	}

	/* (non-Javadoc) */
	Index createIndex(QueryService queryService, String indexName) throws Exception {
		return createIndex(queryService, indexName, false);
	}

	/* (non-Javadoc) */
	private Index createIndex(QueryService queryService, String indexName, boolean retryAttempted) throws Exception {

		IndexType indexType = this.indexType;

		String expression = this.expression;
		String from = this.from;
		String imports = this.imports;

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
		catch (IndexExistsException cause) {

			// Same definition, different name

			Index existingIndexByDefinition =
				tryToFindExistingIndexByDefinition(queryService, expression, from, indexType);

			if (isIgnorable(existingIndexByDefinition)) {

				logWarning("WARNING! You are choosing to ignore this Index [%1$s] and return the existing"
						+ " Index having the same basic definition [%2$s] but with a different name [%3$s];"
						+ " Make sure no OQL Query Hints refer to this Index by name [%1$s]",
					indexName, toBasicIndexDefinition(), existingIndexByDefinition.getName());

				return handleIgnore(existingIndexByDefinition);
			}
			else if (isOverridable(existingIndexByDefinition, retryAttempted)) {

				// Log an informational warning to caution the user about using the override
				logWarning("WARNING! You are attempting to 'override' an existing Index [%1$s]"
						+ " having the same basic definition [%2$s] as the Index that will be created"
						+ " by this IndexFactoryBean [%3$s]; 'Override' effectively 'renames' the existing"
						+ " Index [%1$s] by removing it then recreating it under the new name [%3$s] with"
						+ " the same definition; You should be careful to update any existing OQL Query"
						+ " Hints referring to the old Index name [%1$s] to now use the new name [%3$s]",
					existingIndexByDefinition.getName(), toBasicIndexDefinition(), indexName);

				return handleOverride(existingIndexByDefinition, queryService, indexName);
			}
			else {

				String existingIndexName = (existingIndexByDefinition != null
					? existingIndexByDefinition.getName() : "unknown");

				throw new GemfireIndexException(String.format(
					"An Index with a different name [%1$s] having the same definition [%2$s] already exists;"
						+ " You may attempt to override the existing Index [%1$s] with the new name [%3$s]"
						+ " by setting the 'override' property to 'true'",
					existingIndexName, toBasicIndexDefinition(), indexName), cause);
			}
		}
		catch (IndexNameConflictException cause) {

			// Same name; possibly different definition

 			Index existingIndexByName = tryToFindExistingIndexByName(queryService, indexName);

 			if (isIgnorable(existingIndexByName)) {
 				return handleIgnore(warnOnIndexDefinitionMismatch(existingIndexByName, indexName, "Returning"));
			}
			else if (isOverridable(existingIndexByName, retryAttempted)) {
 				return handleSmartOverride(warnOnIndexDefinitionMismatch(existingIndexByName, indexName,
					"Overriding"), queryService, indexName);
			}
			else {

				String existingIndexDefinition = (existingIndexByName != null
					? String.format(DETAILED_INDEX_DEFINITION, existingIndexByName.getName(),
						existingIndexByName.getIndexedExpression(), existingIndexByName.getFromClause(), "unknown",
							existingIndexByName.getType())
					: "unknown");

				throw new GemfireIndexException(String.format(
					"An Index with the same name [%1$s] having possibly a different definition already exists;"
						+ " you may choose to ignore this Index definition [%2$s] and use the existing Index"
						+ " definition [%3$s] by setting the 'ignoreIfExists' property to 'true'",
					indexName, toDetailedIndexDefinition(), existingIndexDefinition), cause);
			}
		}
		catch (Exception cause) {
			throw new GemfireIndexException(String.format("Failed to create Index [%s]",
				toDetailedIndexDefinition()), cause);
		}
	}

	/* (non-Javadoc) */
	private boolean isIgnorable(Index existingIndex) {
		return (isIgnoreIfExists() && existingIndex != null);
	}

	/* (non-Javadoc) */
	@SuppressWarnings("all")
	private boolean isIndexDefinitionMatch(Index index) {

		boolean result = false;

		if (index != null) {

			IndexType thisIndexType = defaultIfNull(this.indexType, IndexType.FUNCTIONAL);

			result = ObjectUtils.nullSafeEquals(index.getIndexedExpression(), this.expression)
				&& ObjectUtils.nullSafeEquals(index.getFromClause(), this.from)
				&& ObjectUtils.nullSafeEquals(IndexType.valueOf(index.getType()), thisIndexType);

		}

		return result;
	}

	/* (non-Javadoc) */
	private boolean isNotIndexDefinitionMatch(Index index) {
		return !isIndexDefinitionMatch(index);
	}

	/* (non-Javadoc) */
	private boolean isOverridable(Index existingIndex, boolean retryAttempted) {
		return (isOverride() && existingIndex != null && !retryAttempted);
	}

	/* (non-Javadoc) */
	private Index warnOnIndexDefinitionMismatch(Index existingIndex, String indexName, String action) {

		if (isNotIndexDefinitionMatch(existingIndex)) {

			String existingIndexDefinition = String.format(BASIC_INDEX_DEFINITION, existingIndex.getIndexedExpression(),
				existingIndex.getFromClause(), IndexType.valueOf(existingIndex.getType()));

			logWarning("WARNING! %1$s existing Index [%2$s] having a definition [%3$s]"
					+ " that does not match the Index defined [%4$s] by this IndexFactoryBean [%5$s]",
				action, existingIndex.getName(), existingIndexDefinition, toBasicIndexDefinition(), indexName);
		}

		return existingIndex;
	}

	/* (non-Javadoc) */
	private Index handleIgnore(Index existingIndex) {

		registerAlias(getBeanName(), existingIndex.getName());

		return existingIndex;
	}

	/* (non-Javadoc) */
	private Index handleOverride(Index existingIndex, QueryService queryService, String indexName) {
		try {
			// No way to tell whether the QueryService.remove(:Index) was successful or not! o.O
			// Should return a boolean! Does it throw an RuntimeException? Javadoc is useless; #sigh
			queryService.removeIndex(existingIndex);

			return createIndex(queryService, indexName, true);
		}
		catch (Exception cause) {
			throw new GemfireIndexException(String.format(
				"Attempt to 'override' existing Index [%1$s] with the Index that would be created"
					+ " by this IndexFactoryBean [%2$s] failed; you should verify the state of"
					+ " your system and make sure the previously existing Index [%1$s] still exits",
				existingIndex.getName(), indexName), cause);
		}
	}

	/* (non-Javadoc) */
	private Index handleSmartOverride(Index existingIndex, QueryService queryService, String indexName) {

		return (existingIndex.getName().equalsIgnoreCase(indexName) && isIndexDefinitionMatch(existingIndex)
			? existingIndex
			: handleOverride(existingIndex, queryService, indexName));
	}

	/* (non-Javadoc) */
	String toBasicIndexDefinition() {
		return String.format(BASIC_INDEX_DEFINITION, this.expression, this.from, this.indexType);
	}

	/* (non-Javadoc) */
	String toDetailedIndexDefinition() {
		return String.format(DETAILED_INDEX_DEFINITION,
			this.name, this.expression, this.from, this.imports, this.indexType);
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
	Index createHashIndex(QueryService queryService, String indexName, String expression, String from, String imports)
			throws Exception {

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
	Index tryToFindExistingIndexByDefinition(QueryService queryService,
			String expression, String fromClause, IndexType indexType) {

		for (Index index : nullSafeCollection(queryService.getIndexes())) {
			if (index.getIndexedExpression().equalsIgnoreCase(expression)
				&& index.getFromClause().equalsIgnoreCase(fromClause)
				&& indexType.equals(IndexType.valueOf(index.getType()))) {

				return index;
			}
		}

		return null;
	}

	/* (non-Javadoc) */
	Index tryToFindExistingIndexByName(QueryService queryService, String indexName) {

		for (Index index : nullSafeCollection(queryService.getIndexes())) {
			if (index.getName().equalsIgnoreCase(indexName)) {
				return index;
			}
		}

		return null;
	}

	/**
	 * Returns a reference to the {@link Index} created by this {@link IndexFactoryBean}.
	 *
	 * @return a reference to the {@link Index} created by this {@link IndexFactoryBean}.
	 * @see com.gemstone.gemfire.cache.query.Index
	 */
	public Index getIndex() {
		return this.index;
	}

	/* (non-Javadoc) */
	@Override
	public Index getObject() {

		this.index = (this.index != null ? this.index
			: tryToFindExistingIndexByName(resolveQueryService(), resolveIndexName()));

		return this.index;
	}

	/* (non-Javadoc) */
	@Override
	public Class<?> getObjectType() {
		Index index = getIndex();
		return (index != null ? index.getClass() : Index.class);
	}

	/**
	 * Sets a reference to the {@link RegionService}.
	 *
	 * @param cache reference to the {@link RegionService}.
	 * @see com.gemstone.gemfire.cache.RegionService
	 */
	public void setCache(RegionService cache) {
		this.cache = cache;
	}

	/**
	 * Sets the name of the {@link Index}.
	 *
	 * @param name {@link String} containing the name given to the {@link Index}.
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Sets the {@link QueryService} used to create the {@link Index}.
	 *
	 * @param service {@link QueryService} used to create the {@link Index}.
	 * @see com.gemstone.gemfire.cache.query.QueryService
	 */
	public void setQueryService(QueryService service) {
		this.queryService = service;
	}

	/**
	 * Sets a boolean condition to indicate whether the {@link Index} declared and defined by this
	 * {@link IndexFactoryBean} will only be defined initially, or defined and created.  If defined-only,
	 * the {@link IndexFactoryBean} will receive a callback at the end of the Spring container lifecycle
	 * to subsequently "create" all "defined-only" {@link Index Indexes} once, in a single operation.
	 *
	 * @param define a boolean value indicating the define or define/create status.  If {@literal true},
	 * the {@link Index} declared by this {@link IndexFactoryBean} will only be defined initially
	 * and subsequently created when this bean receives an appropriate callback from the Spring container;
	 * if {@literal false}, the {@link Index} will be created immediately.
	 */
	public void setDefine(boolean define) {
		this.define = define;
	}

	/**
	 * Returns a boolean indicating whether the {@link Index} declared and defined by this {@link IndexFactoryBean}
	 * will only be defined initially, or defined and created.  If defined-only, the {@link IndexFactoryBean}
	 * will receive a callback at the end of the Spring container lifecycle to subsequently "create" all "defined-only"
	 * {@link Index Indexes} once, in a single operation.
	 *
	 * @return a boolean value indicating the define or define/create status.  If {@literal true}, the {@link Index}
	 * declared by this {@link IndexFactoryBean} will only be defined initially and subsequently created when this bean
	 * receives an appropriate callback from the Spring container; if {@literal false}, the {@link Index}
	 * will be created immediately.
	 */
	protected boolean isDefine() {
		return define;
	}

	/**
	 * @param expression Index expression to set
	 */
	public void setExpression(String expression) {
		this.expression = expression;
	}

	/**
	 * @param from Index from clause to set
	 */
	public void setFrom(String from) {
		this.from = from;
	}

	/**
	 * @param imports Index imports to set
	 */
	public void setImports(String imports) {
		this.imports = imports;
	}

	/**
	 * Configures whether to ignore the {@link Index} defined by this {@link IndexFactoryBean}
	 * when an {@link IndexExistsException} or {@link IndexNameConflictException} is thrown.
	 *
	 * An {@link IndexExistsException} is thrown when there exists another {@link Index} with the same definition
	 * but with another name.
	 *
	 * An {@link IndexNameConflictException} is thrown when there exists another {@link Index} with the same name
	 * but possibly a different definition.
	 *
	 * When {@literal ignoreIfExists} is set to {@literal true} and an {@link IndexExistsException} is thrown,
	 * then the existing {@link Index} will be returned as the object of this {@link IndexFactoryBean} creation
	 * and the name of the existing {@link Index} is added as an alias for this bean.
	 *
	 * When {@literal ignoreIfExists} is set to {@literal true} and {@link IndexNameConflictException} is thrown,
	 * then the existing {@link Index} will be returned as the object of this {@link IndexFactoryBean} creation.
	 * A warning is logged if the definition of this {@link IndexFactoryBean} and the existing {@link Index}
	 * are different.
	 *
	 * {@literal ignoreIfExists} takes precedence over {@link #isOverride() override}.
	 *
	 * Defaults to {@literal false}.
	 *
	 * @param ignore boolean value indicating whether to ignore the {@link Index} defined by
	 * this {@link IndexFactoryBean}. Default is {@literal false}.
	 * @see #setOverride(boolean)
	 */
	public void setIgnoreIfExists(boolean ignore) {
		this.ignoreIfExists = ignore;
	}

	/**
	 * Determines whether to ignore the {@link Index} defined by this {@link IndexFactoryBean}
	 * when an {@link IndexExistsException} or {@link IndexNameConflictException} is thrown.
	 *
	 * An {@link IndexExistsException} is thrown when there exists another {@link Index} with the same definition
	 * but with another name.
	 *
	 * An {@link IndexNameConflictException} is thrown when there exists another {@link Index} with the same name
	 * but possibly a different definition.
	 *
	 * When {@literal ignoreIfExists} is set to {@literal true} and an {@link IndexExistsException} is thrown,
	 * then the existing {@link Index} will be returned as the object of this {@link IndexFactoryBean} creation
	 * and the name of the existing {@link Index} is added as an alias for this bean.
	 *
	 * When {@literal ignoreIfExists} is set to {@literal true} and {@link IndexNameConflictException} is thrown,
	 * then the existing {@link Index} will be returned as the object of this {@link IndexFactoryBean} creation.
	 * A warning is logged if the definition of this {@link IndexFactoryBean} and the existing {@link Index}
	 * are different.
	 *
	 * {@literal ignoreIfExists} takes precedence over {@link #isOverride() override}.
	 *
	 * Defaults to {@literal false}.
	 *
	 * @return a boolean value indicating whether to ignore the {@link Index} defined by this {@link IndexFactoryBean}.
	 * Default is {@literal false}.
	 * @see #setIgnoreIfExists(boolean)
	 */
	public boolean isIgnoreIfExists() {
		return this.ignoreIfExists;
	}

	/**
	 * Configures whether to override an existing {@link Index} having the same definition but different name
	 * as the {@link Index} that would be created by this {@link IndexFactoryBean}.
	 *
	 * An {@link IndexExistsException} is thrown when there exists another {@link Index} with the same definition
	 * but with another name.
	 *
	 * An {@link IndexNameConflictException} is thrown when there exists another {@link Index} with the same name
	 * but possibly a different definition.
	 *
	 * With {@literal override} set to {@literal true} when an {@link IndexExistsException} is thrown, then override
	 * is effectively the same as "renaming" the existing {@link Index}.  In other words, the existing {@link Index}
	 * will be {@link QueryService#removeIndex(Index) removed} and recreated by this {@link IndexFactoryBean}
	 * under the new {@link #resolveIndexName() name} having the same definition.
	 *
	 * With {@literal override} set to {@literal true} when an {@link IndexNameConflictException} is thrown,
	 * then overriding the existing {@link Index} is equivalent to changing the existing {@link Index} definition.
	 * When this happens, a warning is logged.  If the existing {@link Index} definition is the same then overriding
	 * effectively just rebuilds the {@link Index}.
	 *
	 * {@literal ignoreIfExists} takes precedence over {@literal override}.
	 *
	 * Defaults to {@literal false}.
	 *
	 * @param override boolean value indicating whether an existing {@link Index} will be removed and recreated
	 * by this {@link IndexFactoryBean}. Default is {@literal false}.
	 * @see #setIgnoreIfExists(boolean)
	 */
	public void setOverride(boolean override) {
		this.override = override;
	}

	/**
	 * Determines whether to override an existing {@link Index} having the same definition but different name
	 * as the {@link Index} that would be created by this {@link IndexFactoryBean}.
	 *
	 * An {@link IndexExistsException} is thrown when there exists another {@link Index} with the same definition
	 * but with another name.
	 *
	 * An {@link IndexNameConflictException} is thrown when there exists another {@link Index} with the same name
	 * but possibly a different definition.
	 *
	 * With {@literal override} set to {@literal true} when an {@link IndexExistsException} is thrown, then override
	 * is effectively the same as "renaming" the existing {@link Index}.  In other words, the existing {@link Index}
	 * will be {@link QueryService#removeIndex(Index) removed} and recreated by this {@link IndexFactoryBean}
	 * under the new {@link #resolveIndexName() name} having the same definition.
	 *
	 * With {@literal override} set to {@literal true} when an {@link IndexNameConflictException} is thrown,
	 * then overriding the existing {@link Index} is equivalent to changing the existing {@link Index} definition.
	 * When this happens, a warning is logged.  If the existing {@link Index} definition is the same then overriding
	 * effectively just rebuilds the {@link Index}.
	 *
	 * {@literal ignoreIfExists} takes precedence over {@literal override}.
	 *
	 * Defaults to {@literal false}.
	 *
	 * @return a boolean value indicating whether an existing {@link Index} will be removed and recreated
	 * by this {@link IndexFactoryBean}. Default is {@literal false}.
	 * @see #setOverride(boolean)
	 */
	public boolean isOverride() {
		return this.override;
	}

	/**
	 * Set the {@link IndexType type} of the {@link Index} as a {@link String}.
	 *
	 * @param type {@link String} specifying the {@link IndexType type} of the {@link Index}.
	 * @see org.springframework.data.gemfire.IndexType#valueOf(String)
	 * @see #setType(IndexType)
	 */
	public void setType(String type) {
		setType(IndexType.valueOfIgnoreCase(type));
	}

	/**
	 * Set the {@link IndexType type} of the {@link Index}.
	 *
	 * @param type {@link IndexType} indicating the type of the {@link Index}.
	 * @see org.springframework.data.gemfire.IndexType
	 */
	public void setType(IndexType type) {
		this.indexType = type;
	}

	/* (non-Javadoc) */
	protected static final class IndexWrapper implements Index {

		private Index index;

		private final QueryService queryService;

		private final String indexName;

		protected IndexWrapper(QueryService queryService, String indexName) {

			Assert.notNull(queryService, "QueryService is required");
			Assert.hasText(indexName, "Name of Index is required");

			this.queryService = queryService;
			this.indexName = indexName;
		}

		/* (non-Javadoc) */
		protected synchronized Index resolveIndex() {

			if (this.index == null) {

				String localIndexName = getIndexName();

				for (Index localIndex : nullSafeCollection(getQueryService().getIndexes())) {
					if (localIndex.getName().equals(localIndexName)) {
						this.index = localIndex;
						break;
					}
				}

				if (this.index == null) {
					throw new GemfireIndexException(String.format("Index with name [%s] was not found", localIndexName),
						(Exception) null);
				}
			}

			return index;
		}

		/* (non-Javadoc) */
		protected Index getIndex() {
			return this.index;
		}

		/* (non-Javadoc) */
		protected String getIndexName() {
			Assert.state(StringUtils.hasText(this.indexName), "Index name is required");
			return this.indexName;
		}

		/* (non-Javadoc) */
		protected QueryService getQueryService() {
			return this.queryService;
		}

		@Override
		public String getName() {
			return resolveIndex().getName();
		}

		@Override
		public String getCanonicalizedFromClause() {
			return resolveIndex().getCanonicalizedFromClause();
		}

		@Override
		public String getCanonicalizedIndexedExpression() {
			return resolveIndex().getCanonicalizedIndexedExpression();
		}

		@Override
		public String getCanonicalizedProjectionAttributes() {
			return resolveIndex().getCanonicalizedProjectionAttributes();
		}

		@Override
		public String getFromClause() {
			return resolveIndex().getFromClause();
		}

		@Override
		public String getIndexedExpression() {
			return resolveIndex().getIndexedExpression();
		}

		@Override
		public String getProjectionAttributes() {
			return resolveIndex().getProjectionAttributes();
		}

		@Override
		public Region<?, ?> getRegion() {
			return resolveIndex().getRegion();
		}

		@Override
		public IndexStatistics getStatistics() {
			return resolveIndex().getStatistics();
		}

		@Override
		@SuppressWarnings("deprecation")
		public com.gemstone.gemfire.cache.query.IndexType getType() {
			return resolveIndex().getType();
		}

		@Override
		public boolean equals(Object obj) {

			if (this == obj) {
				return true;
			}

			if (!(obj instanceof IndexWrapper || obj instanceof Index)) {
				return false;
			}

			if (obj instanceof IndexWrapper) {
				return (getIndexName().equals(((IndexWrapper) obj).getIndexName()));
			}

			return resolveIndex().equals(obj);
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
			return (this.index != null ? String.valueOf(this.index) : getIndexName());
		}
	}
}
