/*
 * Copyright 2011-2019 the original author or authors.
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

package org.springframework.data.gemfire;

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexExistsException;
import org.apache.geode.cache.query.IndexNameConflictException;
import org.apache.geode.cache.query.IndexStatistics;
import org.apache.geode.cache.query.QueryService;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.data.gemfire.config.annotation.IndexConfigurer;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
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
 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory
 * @see org.springframework.data.gemfire.IndexFactoryBean.IndexWrapper
 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
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

	//@Autowired(required = false)
	private List<IndexConfigurer> indexConfigurers = Collections.emptyList();

	private final IndexConfigurer compositeIndexConfigurer = new IndexConfigurer() {

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

	@Override
	public void afterPropertiesSet() throws Exception {

		this.indexName = resolveIndexName();

		applyIndexConfigurers(this.indexName);

		this.cache = resolveCache();
		this.queryService = resolveQueryService();

		assertIndexDefinitionConfiguration();

		this.index = createIndex(this.queryService, this.indexName);

		registerAlias(getBeanName(), this.indexName);
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
	private void assertIndexDefinitionConfiguration() {

		Assert.hasText(this.expression, "Index expression is required");
		Assert.hasText(this.from, "Index from clause is required");

		if (IndexType.isKey(this.indexType)) {
			Assert.isTrue(StringUtils.isEmpty(this.imports), "Imports are not supported with a KEY Index");
		}
	}

	/* (non-Javadoc) */
	RegionService resolveCache() {

		return Optional.ofNullable(this.cache)
			.orElseGet(() -> Optional.ofNullable(GemfireUtils.resolveGemFireCache())
				.orElseThrow(() -> newIllegalStateException("Cache is required")));
	}

	/* (non-Javadoc) */
	String resolveIndexName() {

		return Optional.ofNullable(this.name).filter(StringUtils::hasText)
			.orElseGet(() -> Optional.ofNullable(getBeanName()).filter(StringUtils::hasText)
				.orElseThrow(() -> newIllegalArgumentException("Index name is required")));
	}

	/* (non-Javadoc) */
	QueryService resolveQueryService() {

		return Optional.ofNullable(this.queryService)
			.orElseGet(() -> Optional.ofNullable(lookupQueryService())
				.orElseThrow(() -> newIllegalStateException("QueryService is required to create an Index")));
	}

	/* (non-Javadoc) */
	QueryService lookupQueryService() {

		String queryServiceBeanName = GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE;

		return Optional.ofNullable(getBeanFactory())
			.filter(beanFactory -> beanFactory.containsBean(queryServiceBeanName))
			.map(beanFactory -> beanFactory.getBean(queryServiceBeanName, QueryService.class))
			.orElseGet(() -> registerQueryServiceBean(queryServiceBeanName, doLookupQueryService()));
	}

	/* (non-Javadoc) */
	QueryService doLookupQueryService() {

		return Optional.ofNullable(this.queryService).orElseGet(() ->
			(this.cache instanceof ClientCache ? ((ClientCache) this.cache).getLocalQueryService()
				: this.cache.getQueryService()));
	}



	/* (non-Javadoc) */
	QueryService registerQueryServiceBean(String beanName, QueryService queryService) {

		if (isDefine()) {
			((ConfigurableBeanFactory) getBeanFactory()).registerSingleton(beanName, queryService);
		}

		return queryService;
	}

	/* (non-Javadoc) */
	void registerAlias(String beanName, String indexName) {

		Optional.ofNullable(getBeanFactory()).filter(it -> it instanceof ConfigurableBeanFactory)
			.filter(it -> (beanName != null && !beanName.equals(indexName)))
			.ifPresent(it -> ((ConfigurableBeanFactory) it).registerAlias(beanName, indexName));
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

			Optional<Index> existingIndexByDefinition =
				tryToFindExistingIndexByDefinition(queryService, expression, from, indexType);

			return existingIndexByDefinition.filter(existingIndex -> isIgnoreIfExists())
				.map(existingIndex -> {

					logWarning("WARNING! You are choosing to ignore this Index [%1$s] and return the existing"
							+ " Index having the same basic definition [%2$s] but with a different name [%3$s];"
							+ " Make sure no OQL Query Hints refer to this Index by name [%1$s]",
						indexName, toBasicIndexDefinition(), existingIndex.getName());

					return handleIgnore(existingIndex);

				}).orElseGet(() ->

					existingIndexByDefinition.filter(it -> !retryAttempted && isOverride())
						.map(existingIndex -> {

							// Log an informational warning to caution the user about using the override
							logWarning("WARNING! You are attempting to 'override' an existing Index [%1$s]"
									+ " having the same basic definition [%2$s] as the Index that will be created"
									+ " by this IndexFactoryBean [%3$s]; 'Override' effectively 'renames' the existing"
									+ " Index [%1$s] by removing it then recreating it under the new name [%3$s] with"
									+ " the same definition; You should be careful to update any existing OQL Query"
									+ " Hints referring to the old Index name [%1$s] to now use the new name [%3$s]",
								existingIndex.getName(), toBasicIndexDefinition(), indexName);

							return handleOverride(existingIndex, queryService, indexName);

						}).orElseThrow(() -> {

							String existingIndexName = existingIndexByDefinition.map(Index::getName)
								.orElse("unknown");

							return new GemfireIndexException(String.format(
								"An Index with a different name [%1$s] having the same definition [%2$s] already exists;"
									+ " You may attempt to override the existing Index [%1$s] with the new name [%3$s]"
									+ " by setting the 'override' property to 'true'",
								existingIndexName, toBasicIndexDefinition(), indexName), cause);

						})
				);
		}
		catch (IndexNameConflictException cause) {

			// Same name; possibly different definition

 			Optional<Index> existingIndexByName = tryToFindExistingIndexByName(queryService, indexName);

			return existingIndexByName.filter(existingIndex -> isIgnoreIfExists())
				.map(existingIndex ->

					handleIgnore(warnOnIndexDefinitionMismatch(existingIndex, indexName, "Returning"))

				).orElseGet(() ->

					existingIndexByName.filter(it -> !retryAttempted && isOverride())
						.map(existingIndex ->

							handleSmartOverride(warnOnIndexDefinitionMismatch(existingIndex, indexName,
								"Overriding"), queryService, indexName)

						).orElseThrow(() -> {

							String existingIndexDefinition = existingIndexByName
								.map(it -> String.format(DETAILED_INDEX_DEFINITION, it.getName(),
									it.getIndexedExpression(), it.getFromClause(), "unknown", it.getType()))
								.orElse("unknown");

							return new GemfireIndexException(String.format(
								"An Index with the same name [%1$s] having possibly a different definition already exists;"
									+ " you may choose to ignore this Index definition [%2$s] and use the existing Index"
									+ " definition [%3$s] by setting the 'ignoreIfExists' property to 'true'",
								indexName, toDetailedIndexDefinition(), existingIndexDefinition), cause);

						})
				);
		}
		catch (Exception cause) {
			throw new GemfireIndexException(String.format("Failed to create Index [%s]",
				toDetailedIndexDefinition()), cause);
		}
	}

	/* (non-Javadoc) */
	@SuppressWarnings("all")
	private boolean isIndexDefinitionMatch(Index index) {

		return Optional.ofNullable(index)
			.map(it -> {

				IndexType thisIndexType = Optional.ofNullable(this.indexType).orElse(IndexType.FUNCTIONAL);

				boolean result = ObjectUtils.nullSafeEquals(it.getIndexedExpression(), this.expression)
					&& ObjectUtils.nullSafeEquals(it.getFromClause(), this.from)
					&& ObjectUtils.nullSafeEquals(IndexType.valueOf(it.getType()), thisIndexType);

				return result;
			})
			.orElse(false);
	}

	/* (non-Javadoc) */
	private boolean isNotIndexDefinitionMatch(Index index) {
		return !isIndexDefinitionMatch(index);
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

		return Optional.of(existingIndex)
			.filter(it -> it.getName().equalsIgnoreCase(indexName))
			.filter(it -> isIndexDefinitionMatch(existingIndex))
			.orElseGet(() -> handleOverride(existingIndex, queryService, indexName));
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
	Optional<Index> tryToFindExistingIndexByDefinition(QueryService queryService,
			String expression, String fromClause, IndexType indexType) {

		for (Index index : nullSafeCollection(queryService.getIndexes())) {
			if (index.getIndexedExpression().equalsIgnoreCase(expression)
				&& index.getFromClause().equalsIgnoreCase(fromClause)
				&& indexType.equals(IndexType.valueOf(index.getType()))) {

				return Optional.of(index);
			}
		}

		return Optional.empty();
	}

	/* (non-Javadoc) */
	Optional<Index> tryToFindExistingIndexByName(QueryService queryService, String indexName) {

		for (Index index : nullSafeCollection(queryService.getIndexes())) {
			if (index.getName().equalsIgnoreCase(indexName)) {
				return Optional.of(index);
			}
		}

		return Optional.empty();
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
	 * Returns a reference to the {@link Index} created by this {@link IndexFactoryBean}.
	 *
	 * @return a reference to the {@link Index} created by this {@link IndexFactoryBean}.
	 * @see org.apache.geode.cache.query.Index
	 */
	public Index getIndex() {
		return this.index;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Index getObject() {
		return Optional.ofNullable(getIndex()).orElseGet(() ->
			this.index = tryToFindExistingIndexByName(resolveQueryService(), resolveIndexName()).orElse(null));
	}

	/**
	 * @inheritDoc
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Class<?> getObjectType() {
		return Optional.ofNullable(getIndex()).map(Index::getClass).orElse((Class) Index.class);
	}

	/**
	 * Sets a reference to the {@link RegionService}.
	 *
	 * @param cache reference to the {@link RegionService}.
	 * @see org.apache.geode.cache.RegionService
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
	 * @see org.apache.geode.cache.query.QueryService
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

			String indexName = getIndexName();

			return Optional.ofNullable(this.index)
				.orElseGet(() -> {

					AtomicReference<Index> searchResult = new AtomicReference<>();

					nullSafeCollection(getQueryService().getIndexes()).forEach(index -> {
						if (index.getName().equalsIgnoreCase(indexName)) {
							searchResult.set(index);
						}
					});

					return Optional.of(searchResult).map(it -> {
						this.index = it.get();
						return this.index;
					}).orElseThrow(() -> new GemfireIndexException(
						String.format("Index with name [%s] was not found", indexName), (Exception) null));
				});
		}

		/* (non-Javadoc) */
		protected Index getIndex() {
			return this.index;
		}

		/* (non-Javadoc) */
		protected String getIndexName() {
			return Optional.ofNullable(this.indexName).filter(StringUtils::hasText).orElseThrow(() ->
				newIllegalStateException("Index name is required"));
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
		public org.apache.geode.cache.query.IndexType getType() {
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

			return Optional.ofNullable(getIndex()).map(String::valueOf)
				.orElseGet(this::getIndexName);
		}
	}
}
