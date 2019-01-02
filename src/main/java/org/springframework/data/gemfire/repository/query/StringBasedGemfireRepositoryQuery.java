/*
 * Copyright 2012-2019 the original author or authors.
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
package org.springframework.data.gemfire.repository.query;

import java.util.Collection;
import java.util.Collections;

import com.gemstone.gemfire.cache.query.SelectResults;

import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.query.ParametersParameterAccessor;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * {@link GemfireRepositoryQuery} using plain {@link String} based OQL queries.
 * <p>
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 */
public class StringBasedGemfireRepositoryQuery extends GemfireRepositoryQuery {

	private static final String INVALID_QUERY = "Paging and modifying queries are not supported!";

	private boolean userDefinedQuery = false;

	private final GemfireTemplate template;
	private final QueryString query;

	/*
	 * (non-Javadoc)
	 * Constructor used for testing purposes only!
	 */
	StringBasedGemfireRepositoryQuery() {

		this.query = null;
		this.template = null;

		register(LimitQueryPostProcessor.INSTANCE
			.processBefore(ImportQueryPostProcessor.INSTANCE)
			.processBefore(HintQueryPostProcessor.INSTANCE)
			.processBefore(TraceQueryPostProcessor.INSTANCE));
	}

	/**
	 * Creates a new {@link StringBasedGemfireRepositoryQuery} using the given {@link GemfireQueryMethod} and
	 * {@link GemfireTemplate}. The actual query {@link String} will be looked up from the query method.
	 *
	 * @param queryMethod must not be {@literal null}.
	 * @param template must not be {@literal null}.
	 */
	public StringBasedGemfireRepositoryQuery(GemfireQueryMethod queryMethod, GemfireTemplate template) {
		this(queryMethod.getAnnotatedQuery(), queryMethod, template);
	}

	/**
	 * Creates a new {@link StringBasedGemfireRepositoryQuery} using the given query {@link String},
	 * {@link GemfireQueryMethod} and {@link GemfireTemplate}.
	 *
	 * @param query will fall back to the query annotated to the given {@link GemfireQueryMethod} if {@literal null}.
	 * @param queryMethod must not be {@literal null}.
	 * @param template must not be {@literal null}.
	 */
	public StringBasedGemfireRepositoryQuery(String query, GemfireQueryMethod queryMethod, GemfireTemplate template) {

		super(queryMethod);

		Assert.notNull(template, "GemfireTemplate must not be null");
		Assert.state(!(queryMethod.isModifyingQuery() || queryMethod.isPageQuery()), INVALID_QUERY);

		this.userDefinedQuery |= !StringUtils.hasText(query);
		this.query = new QueryString(StringUtils.hasText(query) ? query : queryMethod.getAnnotatedQuery());
		this.template = template;

		register(LimitQueryPostProcessor.INSTANCE
			.processBefore(ImportQueryPostProcessor.INSTANCE)
			.processBefore(HintQueryPostProcessor.INSTANCE)
			.processBefore(TraceQueryPostProcessor.INSTANCE));
	}

	/*
	 * (non-Javadoc)
	 */
	public StringBasedGemfireRepositoryQuery asUserDefinedQuery() {
		this.userDefinedQuery = true;
		return this;
	}

	/*
	 * (non-Javadoc)
	 */
	public boolean isUserDefinedQuery() {
		return userDefinedQuery;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.RepositoryQuery#execute(java.lang.Object[])
	 */
	@Override
	public Object execute(Object[] arguments) {

		QueryMethod queryMethod = getQueryMethod();

		QueryString query = isUserDefinedQuery() ? this.query
			: this.query.forRegion(queryMethod.getEntityInformation().getJavaType(), this.template.getRegion());

		ParametersParameterAccessor parameterAccessor =
			new ParametersParameterAccessor(queryMethod.getParameters(), arguments);

		for (Integer index : query.getInParameterIndexes()) {
			query = query.bindIn(toCollection(parameterAccessor.getBindableValue(index - 1)));
		}

		String queryString = getQueryPostProcessor().postProcess(queryMethod, query.toString(), arguments);

		SelectResults<?> selectResults = this.template.find(queryString, arguments);

		Collection<?> result = toCollection(selectResults);

		if (queryMethod.isCollectionQuery()) {
			return result;
		}
		else if (queryMethod.isQueryForEntity()) {
			if (result.isEmpty()) {
				return null;
			}
			else if (result.size() == 1) {
				return result.iterator().next();
			}
			else {
				throw new IncorrectResultSizeDataAccessException(1, result.size());
			}
		}
		else if (isSingleResultNonEntityQuery(queryMethod, result)) {
			return result.iterator().next();
		}
		else {
			throw new IllegalStateException("Unsupported query: " + query.toString());
		}
	}

	boolean isSingleResultNonEntityQuery(QueryMethod method, Collection<?> result) {

		return (!method.isCollectionQuery() && method.getReturnedObjectType() != null
			&& !Void.TYPE.equals(method.getReturnedObjectType()) && result != null && result.size() == 1);
	}

	/**
	 * Returns the given object as a Collection. Collections will be returned as is, Arrays will be converted into a
	 * Collection and all other objects will be wrapped into a single-element Collection.
	 *
	 * @param source the resulting object from the GemFire Query.
	 * @return the querying resulting object as a Collection.
	 * @see java.util.Arrays#asList(Object[])
	 * @see java.util.Collection
	 * @see org.springframework.util.CollectionUtils#arrayToList(Object)
	 * @see com.gemstone.gemfire.cache.query.SelectResults
	 */
	Collection<?> toCollection(final Object source) {

		if (source instanceof SelectResults) {
			return ((SelectResults) source).asList();
		}

		if (source instanceof Collection) {
			return (Collection<?>) source;
		}

		if (source == null) {
			return Collections.emptyList();
		}

		return (source.getClass().isArray() ? CollectionUtils.arrayToList(source) : Collections.singletonList(source));
	}

	protected static class HintQueryPostProcessor extends AbstractQueryPostProcessor<Repository, String> {

		protected static final HintQueryPostProcessor INSTANCE = new HintQueryPostProcessor();

		@Override
		public String postProcess(QueryMethod queryMethod, String query, Object... arguments) {

			if (queryMethod instanceof GemfireQueryMethod) {

				GemfireQueryMethod gemfireQueryMethod = (GemfireQueryMethod) queryMethod;

				if (gemfireQueryMethod.hasHint() && !QueryString.HINT_PATTERN.matcher(query).find()) {
					query = QueryString.of(query).withHints(gemfireQueryMethod.getHints()).toString();
				}
			}

			return query;
		}
	}

	protected static class ImportQueryPostProcessor extends AbstractQueryPostProcessor<Repository, String> {

		protected static final ImportQueryPostProcessor INSTANCE = new ImportQueryPostProcessor();

		@Override
		public String postProcess(QueryMethod queryMethod, String query, Object... arguments) {

			if (queryMethod instanceof GemfireQueryMethod) {

				GemfireQueryMethod gemfireQueryMethod = (GemfireQueryMethod) queryMethod;

				if (gemfireQueryMethod.hasImport() && !QueryString.IMPORT_PATTERN.matcher(query).find()) {
					query = QueryString.of(query).withImport(gemfireQueryMethod.getImport()).toString();
				}
			}

			return query;
		}
	}

	protected static class LimitQueryPostProcessor extends AbstractQueryPostProcessor<Repository, String> {

		protected static final LimitQueryPostProcessor INSTANCE = new LimitQueryPostProcessor();

		@Override
		public String postProcess(QueryMethod queryMethod, String query, Object... arguments) {

			if (queryMethod instanceof  GemfireQueryMethod) {

				GemfireQueryMethod gemfireQueryMethod = (GemfireQueryMethod) queryMethod;

				if (gemfireQueryMethod.hasLimit() && !QueryString.LIMIT_PATTERN.matcher(query).find()) {
					query = QueryString.of(query).withLimit(gemfireQueryMethod.getLimit()).toString();
				}
			}

			return query;
		}
	}

	protected static class TraceQueryPostProcessor extends AbstractQueryPostProcessor<Repository, String> {

		protected static final TraceQueryPostProcessor INSTANCE = new TraceQueryPostProcessor();

		@Override
		public String postProcess(QueryMethod queryMethod, String query, Object... arguments) {

			if (queryMethod instanceof GemfireQueryMethod) {

				GemfireQueryMethod gemfireQueryMethod = (GemfireQueryMethod) queryMethod;

				if (gemfireQueryMethod.hasTrace() && !QueryString.TRACE_PATTERN.matcher(query).find()) {
					query = QueryString.of(query).withTrace().toString();
				}
			}

			return query;
		}
	}
}
