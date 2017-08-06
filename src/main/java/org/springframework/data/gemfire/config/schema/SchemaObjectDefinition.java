/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.schema;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.io.Serializable;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.Index;
import org.springframework.core.Ordered;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.util.StringUtils;

/**
 * {@link SchemaObjectDefinition} is an Abstract Data Type (ADT) encapsulating the definition of a single Apache Geode
 * or Pivotal GemFire schema object (e.g. {@link Region} or {@link Index}).
 *
 * @author John Blum
 * @see java.io.Serializable
 * @see org.springframework.core.Ordered
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefiner
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectType
 * @since 2.0.0
 */
public abstract class SchemaObjectDefinition implements Serializable, Ordered {

	private final String name;

	/**
	 * Constructs a new instance of {@link SchemaObjectDefinition} initialized with the specified {@link String name}.
	 *
	 * @param name {@link String name} given to the GemFire/Geode schema object; must not be {@literal null}.
	 * @throws IllegalArgumentException if name is not specified.
	 */
	public SchemaObjectDefinition(String name) {
		this.name = Optional.ofNullable(name).filter(StringUtils::hasText)
			.orElseThrow(() -> newIllegalArgumentException("Name [%s] is required", name));
	}

	/**
	 * Returns the {@link String name} assigned to the schema object.
	 *
	 * @return the {@link String name} assigned to the schema object; name is never {@literal null}.
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * Returns the {@link SchemaObjectType type} of schema object defined by this {@link SchemaObjectDefinition}.
	 *
	 * @return the {@link SchemaObjectType type} of schema object defined by this {@link SchemaObjectDefinition}.
	 * @see org.springframework.data.gemfire.config.schema.SchemaObjectType
	 */
	public abstract SchemaObjectType getType();

	/**
	 * Creates an actual schema object from this {@link SchemaObjectDefinition}.
	 *
	 * @param gemfireAdminOperations {@link GemfireAdminOperations} used to create an actual schema object from this
	 * {@link SchemaObjectDefinition}.
	 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
	 */
	public void create(GemfireAdminOperations gemfireAdminOperations) {
	}

	@Override
	public boolean equals(Object obj) {

		if (this == obj) {
			return true;
		}

		if (!(obj instanceof SchemaObjectDefinition)) {
			return false;
		}

		SchemaObjectDefinition that = (SchemaObjectDefinition) obj;

		return this.getName().equals(that.getName())
			&& this.getType().equals(that.getType());
	}

	@Override
	public int hashCode() {

		int hashValue = 17;

		hashValue = 37 * hashValue + this.getName().hashCode();
		hashValue = 37 * hashValue + this.getType().hashCode();

		return hashValue;
	}

	@Override
	public String toString() {
		return String.format("%1$s[%2$s]", getType(), getName());
	}
}
