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
package org.springframework.data.gemfire.mapping;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;

import org.springframework.data.gemfire.mapping.model.GemfireSimpleTypeHolder;
import org.springframework.data.mapping.Association;
import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.mapping.PersistentProperty;
import org.springframework.data.mapping.model.AnnotationBasedPersistentProperty;
import org.springframework.data.mapping.model.SimpleTypeHolder;

/**
 * {@link PersistentProperty} implementation to for Gemfire related metadata.
 * 
 * @author Oliver Gierke
 */
public class GemfirePersistentProperty extends AnnotationBasedPersistentProperty<GemfirePersistentProperty> {

	/* (non-Javadoc) */
	private static SimpleTypeHolder resolveSimpleTypeHolder(SimpleTypeHolder source) {
		return (source instanceof GemfireSimpleTypeHolder ? source
			: (source != null ? new GemfireSimpleTypeHolder(source) : new GemfireSimpleTypeHolder()));
	}

	/**
	 * Constructs an instance of the GemfirePersistentProperty with entity information.
	 *
	 * @param field the entity field corresponding to the persistent property.
	 * @param propertyDescriptor PropertyDescriptor for the entity's persistent property.
	 * @param owner the entity owning the persistent property.
	 * @param simpleTypeHolder type holder for primitive types.
	 */
	public GemfirePersistentProperty(Field field, PropertyDescriptor propertyDescriptor,
			PersistentEntity<?, GemfirePersistentProperty> owner, SimpleTypeHolder simpleTypeHolder) {
		super(field, propertyDescriptor, owner, resolveSimpleTypeHolder(simpleTypeHolder));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.springframework.data.mapping.model.AbstractPersistentProperty#
	 * createAssociation()
	 */
	@Override
	protected Association<GemfirePersistentProperty> createAssociation() {
		return new Association<GemfirePersistentProperty>(this, null);
	}
}
