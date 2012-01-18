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

import java.lang.reflect.Method;

import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;
import org.springframework.data.gemfire.repository.Query;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Gemfire specific {@link QueryMethod}.
 * 
 * @author Oliver Gierke
 */
public class GemfireQueryMethod extends QueryMethod {

	private final Method method;
	private final GemfirePersistentEntity<?> entity;

	/**
	 * Creates a new {@link GemfireQueryMethod} from the given {@link Method} and {@link RepositoryMetadata}.
	 * 
	 * @param method must not be {@literal null}.
	 * @param metadata must not be {@literal null}.
	 * @param context must not be {@literal null}.
	 */
	public GemfireQueryMethod(Method method, RepositoryMetadata metadata,
			MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> context) {

		super(method, metadata);

		Assert.notNull(context);

		this.method = method;
		this.entity = context.getPersistentEntity(getDomainClass());
	}

	/**
	 * Returns whether the query method contains an annotated, non-empty query.
	 * 
	 * @return
	 */
	public boolean hasAnnotatedQuery() {
		return StringUtils.hasText(getAnnotatedQuery());
	}

	/**
	 * @return the entity
	 */
	public GemfirePersistentEntity<?> getPersistentEntity() {
		return entity;
	}

	/**
	 * Returns the query annotated to the query method.
	 * 
	 * @return the annotated query or {@literal null} in case it's empty or none available.
	 */
	String getAnnotatedQuery() {

		Query query = method.getAnnotation(Query.class);
		String queryString = query == null ? null : (String) AnnotationUtils.getValue(query);

		return StringUtils.hasText(queryString) ? queryString : null;
	}
}
