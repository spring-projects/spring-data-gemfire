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

import static org.springframework.data.gemfire.util.SpringUtils.defaultIfEmpty;

import java.lang.annotation.Annotation;

import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.mapping.model.BasicPersistentEntity;
import org.springframework.data.util.TypeInformation;

/**
 * {@link PersistentEntity} implementation adding custom GemFire persistent entity related metadata, such as the
 * {@link com.gemstone.gemfire.cache.Region} to which the entity is mapped, etc.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.data.gemfire.mapping.GemfirePersistentProperty
 * @see org.springframework.data.mapping.model.BasicPersistentEntity
 */
@SuppressWarnings("unused")
public class GemfirePersistentEntity<T> extends BasicPersistentEntity<T, GemfirePersistentProperty> {

	private final Annotation regionAnnotation;

	private final String regionName;

	/* (non-Javadoc) */
	protected static Annotation resolveRegionAnnotation(Class<?> persistentEntityType) {

		for (Class<? extends Annotation> regionAnnotationType : Region.REGION_ANNOTATION_TYPES) {
			Annotation regionAnnotation = AnnotatedElementUtils.getMergedAnnotation(
				persistentEntityType, regionAnnotationType);

			if (regionAnnotation != null) {
				return regionAnnotation;
			}
		}

		return null;
	}

	/* (non-Javadoc) */
	protected static String resolveRegionName(Class<?> persistentEntityType, Annotation regionAnnotation) {

		String regionName = (regionAnnotation != null ? AnnotationAttributes.fromMap(
			AnnotationUtils.getAnnotationAttributes(regionAnnotation)).getString("value") : null);

		return defaultIfEmpty(regionName, persistentEntityType.getSimpleName());
	}

	/**
	 * Creates a new {@link GemfirePersistentEntity} for the given {@link TypeInformation}.
	 *
	 * @param information must not be {@literal null}.
	 */
	public GemfirePersistentEntity(TypeInformation<T> information) {
		super(information);

		Class<T> rawType = information.getType();

		this.regionAnnotation = resolveRegionAnnotation(rawType);
		this.regionName = resolveRegionName(rawType, this.regionAnnotation);
	}

	/**
	 * Returns the {@link Region} annotation used to annotate this {@link PersistentEntity} or {@literal null}
	 * if this {@link PersistentEntity} was not annotated with a {@link Region} annotation.
	 *
	 * @param <T> concrete {@link Class} type of the Region {@link Annotation}.
	 * @return the {@link Region} annotation used to annotate this {@link PersistentEntity} or {@literal null}
	 * if this {@link PersistentEntity} was not annotated with a {@link Region} annotation.
	 * @see org.springframework.data.gemfire.mapping.ClientRegion
	 * @see org.springframework.data.gemfire.mapping.LocalRegion
	 * @see org.springframework.data.gemfire.mapping.PartitionRegion
	 * @see org.springframework.data.gemfire.mapping.ReplicateRegion
	 * @see org.springframework.data.gemfire.mapping.Region
	 * @see java.lang.annotation.Annotation
	 */
	@SuppressWarnings("unchecked")
	public <T extends Annotation> T getRegionAnnotation() {
		return (T) this.regionAnnotation;
	}

	/**
	 * Returns the {@link Class} type of the Region {@link Annotation} or {@literal null}
	 * if this {@link PersistentEntity} was not annotated with a Region {@link Annotation}.
	 *
	 * @return the {@link Class} type of the Region {@link Annotation} or {@literal null}
	 * if this {@link PersistentEntity} was not annotated with a Region {@link Annotation}.
	 * @see java.lang.annotation.Annotation#annotationType()
	 * @see #getRegionAnnotation()
	 */
	public Class<? extends Annotation> getRegionAnnotationType() {
		Annotation regionAnnotation = getRegionAnnotation();
		return (regionAnnotation != null ? regionAnnotation.annotationType() : null);
	}

	/**
	 * Returns the name of the {@link Region} in which this {@link PersistentEntity} will be stored.
	 *
	 * @return the name of the {@link Region} in which this {@link PersistentEntity} will be stored.
	 * @see com.gemstone.gemfire.cache.Region#getName()
	 */
	public String getRegionName() {
		return this.regionName;
	}
}
