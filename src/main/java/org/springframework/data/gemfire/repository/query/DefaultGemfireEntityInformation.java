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

import java.io.Serializable;

import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.repository.core.support.DelegatingEntityInformation;
import org.springframework.data.repository.core.support.ReflectionEntityInformation;

/**
 * Implementation of {@link GemfireEntityInformation} using reflection to lookup region names.
 * 
 * @author Oliver Gierke
 */
public class DefaultGemfireEntityInformation<T, ID extends Serializable> extends DelegatingEntityInformation<T, ID>
implements GemfireEntityInformation<T, ID> {

	private final GemfirePersistentEntity<T> entity;

	/**
	 * Creates a new {@link DefaultGemfireEntityInformation}.
	 * 
	 * @param entity must not be {@literal null}.
	 */
	public DefaultGemfireEntityInformation(GemfirePersistentEntity<T> entity) {
		super(new ReflectionEntityInformation<T, ID>(entity.getType()));
		this.entity = entity;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.repository.query.GemfireEntityInformation#getRegionName()
	 */
	@Override
	public String getRegionName() {
		return entity.getRegionName();
	}
}
