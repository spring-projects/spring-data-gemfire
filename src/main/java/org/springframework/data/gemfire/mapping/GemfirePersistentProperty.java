/*
 * Copyright 2012-2020 the original author or authors.
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

package org.springframework.data.gemfire.mapping;

import static org.springframework.data.gemfire.util.CollectionUtils.*;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Set;

import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mapping.Association;
import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.mapping.PersistentProperty;
import org.springframework.data.mapping.model.AnnotationBasedPersistentProperty;
import org.springframework.data.mapping.model.Property;
import org.springframework.data.mapping.model.SimpleTypeHolder;

/**
 * {@link PersistentProperty} implementation to for Gemfire related metadata.
 *
 * @author Oliver Gierke
 */
public class GemfirePersistentProperty extends AnnotationBasedPersistentProperty<GemfirePersistentProperty> {

	protected static final Set<String> SUPPORTED_IDENTIFIER_NAMES = asSet("id");

	/**
	 * Constructs an instance of {@link GemfirePersistentProperty} initialized with entity persistent property
	 * information (meta-data).
	 *
	 * @param property {@link Property} representing the {@link GemfirePersistentEntity entity's}  persistent property.
	 * @param owner {@link GemfirePersistentEntity entity} owning the persistent property.
	 * @param simpleTypeHolder {@link SimpleTypeHolder} used to handle primitive types.
	 * @see org.springframework.data.mapping.PersistentEntity
	 * @see org.springframework.data.mapping.PersistentProperty
	 * @see org.springframework.data.mapping.model.Property
	 * @see org.springframework.data.mapping.model.SimpleTypeHolder
	 * @see AnnotationBasedPersistentProperty(Property, PersistentEntity, SimpleTypeHolder)
	 */
	public GemfirePersistentProperty(Property property, PersistentEntity<?, GemfirePersistentProperty> owner,
			SimpleTypeHolder simpleTypeHolder) {

		super(property, owner, simpleTypeHolder);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected Association<GemfirePersistentProperty> createAssociation() {
		return new Association<>(this, null);
	}

	/**
	 * Determines whether this {@link GemfirePersistentProperty} explicitly identifies
	 * an {@link GemfirePersistentEntity entity} identifier, one in which the user explicitly annotated
	 * the {@link GemfirePersistentEntity owning entity} class member ({@link Field} or property,
	 * i.e. {@link Method getter/setter}).
	 *
	 * @return a boolean value indicating whether this {@link GemfirePersistentProperty} explicitly identifies
	 * an {@link GemfirePersistentEntity entity} identifier.
	 * @see org.springframework.data.annotation.Id
	 * @see #isAnnotationPresent(Class)
	 */
	public boolean isExplicitIdProperty() {
		return isAnnotationPresent(Id.class);
	}

	/**
	 * @inheritDoc
	 * @see org.springframework.data.mapping.model.AnnotationBasedPersistentProperty#isIdProperty()
	 */
	@Override
	public boolean isIdProperty() {
		return super.isIdProperty() || SUPPORTED_IDENTIFIER_NAMES.contains(getName());
	}

	/**
	 * Determines whether this {@link GemfirePersistentProperty persistent property} is {@literal transient}
	 * and thus impervious to persistent operations.
	 *
	 * A {@link GemfirePersistentProperty persistent property} is considered {@literal transient}
	 * if the {@link GemfirePersistentEntity owning entity's} field/property is annotated with
	 * {@link Transient} or the field/property is modified with {@link Modifier#TRANSIENT transient}.
	 *
	 * @return a boolean value indicating whether this {@link GemfirePersistentProperty persistent property}
	 * is {@literal transient} and thus impervious to persistent operations.
	 */
	@Override
	public boolean isTransient() {
		return super.isTransient()
			|| getProperty().getField().filter(field -> Modifier.isTransient(field.getModifiers())).isPresent();
	}

	/**
	 * Returns the {@link String name} of this {@link GemfirePersistentProperty's} {@link Class type}.
	 *
	 * @return the {@link String name} of this {@link GemfirePersistentProperty's} {@link Class type}.
	 * @see java.lang.Class#getName()
	 * @see #getType()
	 */
	public String getTypeName() {
		return getType().getName();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean usePropertyAccess() {
		return super.usePropertyAccess() || !getProperty().isFieldBacked();
	}
}
