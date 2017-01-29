/*
 * Copyright 2012-2018 the original author or authors.
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

package org.springframework.data.gemfire.mapping;

import org.springframework.data.gemfire.mapping.model.GemfireSimpleTypeHolder;
import org.springframework.data.mapping.context.AbstractMappingContext;
import org.springframework.data.mapping.model.MutablePersistentEntity;
import org.springframework.data.mapping.model.Property;
import org.springframework.data.mapping.model.SimpleTypeHolder;
import org.springframework.data.util.TypeInformation;

/**
 * Spring Data {@link AbstractMappingContext} implementation defining entity mapping meta-data
 * for GemFire persistent entities.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.data.mapping.context.AbstractMappingContext
 */
public class GemfireMappingContext extends AbstractMappingContext<GemfirePersistentEntity<?>, GemfirePersistentProperty> {

	/**
	 * Constructs a GemfireMappingContext with a GemfireSimpleTypeHolder.
	 *
	 * @see org.springframework.data.gemfire.mapping.model.GemfireSimpleTypeHolder
	 */
	public GemfireMappingContext() {
		// Technically, the following call is not Thread-safe (the "this" reference escapes), but then MappingContext
		// makes no Thread-safety guarantees, even though, most likely, and especially in GemFire's case,
		// the MappingContext will be used in a highly concurrent context (modeled after SD MongoDB for consistency)!
		setSimpleTypeHolder(new GemfireSimpleTypeHolder());
	}

	/**
	 * @inheritDoc
	 * @see org.springframework.data.mapping.context.AbstractMappingContext#createPersistentEntity(org.springframework.data.util.TypeInformation)
	 */
	@Override
	protected <T> GemfirePersistentEntity<T> createPersistentEntity(TypeInformation<T> typeInformation) {
		return new GemfirePersistentEntity<>(typeInformation);
	}

	/**
	 * @inheritDoc
	 * @see org.springframework.data.mapping.context.AbstractMappingContext#createPersistentProperty(Property, MutablePersistentEntity, SimpleTypeHolder)
	 */
	@Override
	protected GemfirePersistentProperty createPersistentProperty(Property property, GemfirePersistentEntity<?> owner,
			SimpleTypeHolder simpleTypeHolder) {

		return new GemfirePersistentProperty(property, owner, simpleTypeHolder);
	}
}
