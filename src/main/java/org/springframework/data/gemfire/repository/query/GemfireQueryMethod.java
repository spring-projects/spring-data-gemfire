/*
 * Copyright 2012-2015 the original author or authors.
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

import java.lang.reflect.Method;

import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.domain.Pageable;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;
import org.springframework.data.gemfire.repository.Query;
import org.springframework.data.gemfire.repository.query.annotation.Hint;
import org.springframework.data.gemfire.repository.query.annotation.Import;
import org.springframework.data.gemfire.repository.query.annotation.Limit;
import org.springframework.data.gemfire.repository.query.annotation.Trace;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.data.projection.ProjectionFactory;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * GemFire specific {@link QueryMethod}.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.data.repository.query.QueryMethod
 */
public class GemfireQueryMethod extends QueryMethod {

	@SuppressWarnings("all")
	protected static final String[] EMPTY_STRING_ARRAY = new String[0];

	private final GemfirePersistentEntity<?> entity;

	private final Method method;

	/**
	 * Creates a new {@link GemfireQueryMethod} from the given {@link Method} and {@link RepositoryMetadata}.
	 *
	 * @param method must not be {@literal null}.
	 * @param metadata must not be {@literal null}.
	 * @param factory must not be {@literal null}.
	 * @param mappingContext must not be {@literal null}.
	 */
	public GemfireQueryMethod(Method method, RepositoryMetadata metadata, ProjectionFactory factory,
			MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext) {

		super(method, metadata, factory);

		Assert.notNull(mappingContext, "MappingContext must not be null");
		assertNonPagingQueryMethod(method);

		this.method = method;

		this.entity = mappingContext.getPersistentEntity(getDomainClass());
	}

	/**
	 * Asserts that the query method is a non-Paging query method since GemFire does not support pagination
	 * as it has no concept of a Cursor.
	 *
	 * @param method the query method to be evaluated
	 * @throws java.lang.IllegalStateException if the query method contains a parameter of type Pageable.
	 * @see org.springframework.data.domain.Pageable
	 * @see java.lang.reflect.Method#getParameterTypes()
	 */
	private void assertNonPagingQueryMethod(Method method) {
		for (Class<?> type : method.getParameterTypes()) {
			if (Pageable.class.isAssignableFrom(type)) {
				throw new IllegalStateException(String.format("Pagination is not supported by GemFire Repositories;"
					+ " Offending method: %1$s", method.getName()));
			}
		}
	}

	/**
	 * Returns the {@link GemfirePersistentEntity} the method deals with.
	 *
	 * @return the {@link GemfirePersistentEntity} the method deals with.
	 */
	public GemfirePersistentEntity<?> getPersistentEntity() {
		return entity;
	}

	/**
	 * Determines whether this query method specifies an annotated, non-empty query.
	 *
	 * @return a boolean value indicating whether the query method specifies an annotated, non-empty query.
	 * @see org.springframework.util.StringUtils#hasText(String)
	 * @see #getAnnotatedQuery()
	 */
	public boolean hasAnnotatedQuery() {
		return StringUtils.hasText(getAnnotatedQuery());
	}

	/**
	 * Returns the annotated query for the query method if present.
	 *
	 * @return the annotated query or {@literal null} in case it's empty or not present.
	 * @see org.springframework.data.gemfire.repository.Query
	 * @see java.lang.reflect.Method#getAnnotation(Class)
	 */
	String getAnnotatedQuery() {
		Query query = method.getAnnotation(Query.class);
		String queryString = (query != null ? (String) AnnotationUtils.getValue(query) : null);
		return (StringUtils.hasText(queryString) ? queryString : null);
	}

	/**
	 * Determines whether this query method uses a query HINT to tell the GemFire OQL query engine which indexes
	 * to apply to the query execution.
	 *
	 * @return a boolean value to indicate whether this query method uses a query HINT.
	 * @see org.springframework.data.gemfire.repository.query.annotation.Hint
	 * @see java.lang.reflect.Method#isAnnotationPresent(Class)
	 */
	public boolean hasHint() {
		return method.isAnnotationPresent(Hint.class);
	}

	/**
	 * Gets the query HINTs for this query method.
	 *
	 * @return the query HINTs for this query method or an empty array if this query method has no query HINTs.
	 * @see org.springframework.data.gemfire.repository.query.annotation.Hint
	 * @see java.lang.reflect.Method#getAnnotation(Class)
	 */
	public String[] getHints() {
		Hint hint = method.getAnnotation(Hint.class);
		return (hint != null ? hint.value() : EMPTY_STRING_ARRAY);
	}

	/**
	 * Determine whether this query method declares an IMPORT statement to qualify application domain object types
	 * referenced in the query.
	 *
	 * @return a boolean value to indicate whether this query method declares an IMPORT statement.
	 * @see org.springframework.data.gemfire.repository.query.annotation.Import
	 * @see java.lang.reflect.Method#isAnnotationPresent(Class)
	 */
	public boolean hasImport() {
		return method.isAnnotationPresent(Import.class);
	}

	/**
	 * Gets the IMPORT statement for this query method.
	 *
	 * @return the IMPORT statement for this query method or null if this query method does not have an IMPORT statement.
	 * @see org.springframework.data.gemfire.repository.query.annotation.Import
	 * @see java.lang.reflect.Method#getAnnotation(Class)
	 */
	public String getImport() {
		Import importStatement = method.getAnnotation(Import.class);
		return (importStatement != null ? importStatement.value() : null);
	}

	/**
	 * Determines whether this query method defines a LIMIT on the number of results returned by the query.
	 *
	 * @return a boolean value indicating whether this query method defines a LIMIT on the result set
	 * returned by the query.
	 * @see org.springframework.data.gemfire.repository.query.annotation.Limit
	 * @see java.lang.reflect.Method#isAnnotationPresent(Class)
	 */
	public boolean hasLimit() {
		return method.isAnnotationPresent(Limit.class);
	}

	/**
	 * Gets the LIMIT for this query method on the result set returned by the query.
	 *
	 * @return the LIMIT for this query method limiting the number of results returned by the query.
	 * @see org.springframework.data.gemfire.repository.query.annotation.Limit
	 * @see java.lang.reflect.Method#getAnnotation(Class)
	 */
	public int getLimit() {
		Limit limit = method.getAnnotation(Limit.class);
		return (limit != null ? limit.value() : Integer.MAX_VALUE);
	}

	/**
	 * Determines whether this query method has TRACE (i.e. logging) enabled.
	 *
	 * @return a boolean value to indicate whether this query method has TRACE enabled.
	 * @see org.springframework.data.gemfire.repository.query.annotation.Limit
	 * @see java.lang.reflect.Method#isAnnotationPresent(Class)
	 */
	public boolean hasTrace() {
		return method.isAnnotationPresent(Trace.class);
	}

}
