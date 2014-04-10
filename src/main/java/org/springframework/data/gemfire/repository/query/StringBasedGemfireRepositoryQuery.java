/*
 * Copyright 2012 the original author or authors.
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
import java.util.Iterator;

import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.repository.query.ParametersParameterAccessor;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.query.SelectResults;
import com.gemstone.gemfire.cache.query.internal.ResultsBag;
import com.gemstone.gemfire.cache.query.internal.ResultsCollectionPdxDeserializerWrapper;

/**
 * {@link GemfireRepositoryQuery} using plain {@link String} based OQL queries.
 * 
 * @author Oliver Gierke
 * @author David Turanski
 */
public class StringBasedGemfireRepositoryQuery extends GemfireRepositoryQuery {

	private static final String INVALID_EXECUTION = "Paging and modifying queries are not supported!";

	private final QueryString query;
	private final GemfireQueryMethod method;
	private final GemfireTemplate template;

	/**
	 * Creates a new {@link StringBasedGemfireRepositoryQuery} using the given {@link GemfireQueryMethod} and
	 * {@link GemfireTemplate}. The actual query {@link String} will be looked up from the query method.
	 * 
	 * @param method must not be {@literal null}.
	 * @param template must not be {@literal null}.
	 */
	public StringBasedGemfireRepositoryQuery(GemfireQueryMethod method, GemfireTemplate template) {
		this(method.getAnnotatedQuery(), method, template);
	}

	/**
	 * Creates a new {@link StringBasedGemfireRepositoryQuery} using the given query {@link String},
	 * {@link GemfireQueryMethod} and {@link GemfireTemplate}.
	 * 
	 * @param query will fall back to the query annotated to the given {@link GemfireQueryMethod} if {@literal null} is
	 *          given.
	 * @param method must not be {@literal null}.
	 * @param template must not be {@literal null}.
	 */
	public StringBasedGemfireRepositoryQuery(String query, GemfireQueryMethod method, GemfireTemplate template) {

		super(method);

		Assert.notNull(template);

		this.query = new QueryString(StringUtils.hasText(query) ? query : method.getAnnotatedQuery());
		this.method = method;
		this.template = template;

		if (method.isPageQuery() || method.isModifyingQuery()) {
			throw new IllegalStateException(INVALID_EXECUTION);
		}
	}

	/* 
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.RepositoryQuery#execute(java.lang.Object[])
	 */
	@Override
	public Object execute(Object[] parameters) {

		ParametersParameterAccessor accessor = new ParametersParameterAccessor(method.getParameters(), parameters);
		QueryString query = this.query.forRegion(method.getEntityInformation().getJavaType(), template.getRegion());

		Iterator<Integer> indexes = query.getInParameterIndexes().iterator();
		while (indexes.hasNext()) {
			query = query.bindIn(toCollection(accessor.getBindableValue(indexes.next() - 1)));
		}

		Collection<?> result = toCollection(template.find(query.toString(), parameters));

		if (method.isCollectionQuery()) {
			return result;
		} else if (method.isQueryForEntity()) {
			if (result.isEmpty()) {
				return null;
			} else if (result.size() == 1) {
				return result.iterator().next();
			} else {
				throw new IncorrectResultSizeDataAccessException(1, result.size());
			}
		} else {
			throw new IllegalStateException("Unsupported query: " + query.toString());
		}
	}

	/**
	 * Returns the given object as collection. Collections will be returned as is, Arrays will be converted into a
	 * collection and all other objects will be wrapped into a single-element collection.
	 * 
	 * @param source
	 * @return
	 */
	private Collection<?> toCollection(Object source) {

		if (source instanceof SelectResults) {
			return ((SelectResults) source).asList();
		}

		if (source instanceof ResultsBag) {
			ResultsBag bag = (ResultsBag) source;
			return bag.asList();
		}

		if (source instanceof Collection) {
			return (Collection<?>) source;
		}

		return source.getClass().isArray() ? CollectionUtils.arrayToList(source) : Collections.singleton(source);
	}
}
