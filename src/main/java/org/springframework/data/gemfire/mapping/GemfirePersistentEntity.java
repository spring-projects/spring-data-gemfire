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

import java.lang.annotation.Annotation;
import java.util.Optional;

import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.mapping.MappingException;
import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.mapping.PersistentProperty;
import org.springframework.data.mapping.model.BasicPersistentEntity;
import org.springframework.data.util.TypeInformation;
import org.springframework.lang.NonNull;
import org.springframework.util.StringUtils;

/**
 * {@link PersistentEntity} implementation adding custom GemFire persistent entity related metadata, such as the
 * {@link org.apache.geode.cache.Region} to which the entity is mapped, etc.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @author Gregory Green
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

			Annotation regionAnnotation =
				AnnotatedElementUtils.getMergedAnnotation(persistentEntityType, regionAnnotationType);

			if (regionAnnotation != null) {
				return regionAnnotation;
			}
		}

		return null;
	}

	/* (non-Javadoc) */
	protected static String resolveRegionName(Class<?> persistentEntityType, Annotation regionAnnotation) {

		Optional<String> regionName = Optional.ofNullable(regionAnnotation)
			.map(annotation ->  getAnnotationAttributeStringValue(annotation, "value"))
			.filter(StringUtils::hasText);

		return regionName.orElse(persistentEntityType.getSimpleName());
	}

	/* (non-Javadoc) */
	protected static String getAnnotationAttributeStringValue(Annotation annotation, String attributeName) {
		return AnnotationAttributes.fromMap(AnnotationUtils.getAnnotationAttributes(annotation))
			.getString(attributeName);
	}

	/**
	 * Constructs a new instance of {@link GemfirePersistentEntity} initialized with the given {@link TypeInformation}
	 * describing the domain object (entity) {@link Class} type.
	 *
	 * @param information {@link TypeInformation} meta-data describing the domain object (entity) {@link Class} type.
	 * @throws IllegalArgumentException if the given {@link TypeInformation} is {@literal null}.
	 * @see org.springframework.data.util.TypeInformation
	 */
	public GemfirePersistentEntity(TypeInformation<T> information) {

		super(information);

		Class<T> rawType = information.getType();

		this.regionAnnotation = resolveRegionAnnotation(rawType);
		this.regionName = resolveRegionName(rawType, this.regionAnnotation);
	}

	/**
	 * Returns the {@link Region} {@link Annotation} used to annotate this {@link PersistentEntity} or {@literal null}
	 * if this {@link PersistentEntity} was not annotated with a {@link Region} {@link Annotation}.
	 *
	 * @param <T> concrete {@link Class} type of the {@link Region} {@link Annotation}.
	 * @return the {@link Region} {@link Annotation} used to annotate this {@link PersistentEntity} or {@literal null}
	 * if this {@link PersistentEntity} was not annotated with a {@link Region} {@link Annotation}.
	 * @see org.springframework.data.gemfire.mapping.annotation.ClientRegion
	 * @see org.springframework.data.gemfire.mapping.annotation.LocalRegion
	 * @see org.springframework.data.gemfire.mapping.annotation.PartitionRegion
	 * @see org.springframework.data.gemfire.mapping.annotation.ReplicateRegion
	 * @see org.springframework.data.gemfire.mapping.annotation.Region
	 * @see java.lang.annotation.Annotation
	 */
	@SuppressWarnings("unchecked")
	public <T extends Annotation> T getRegionAnnotation() {
		return (T) this.regionAnnotation;
	}

	/**
	 * Returns the {@link Class} type of the {@link Region} {@link Annotation} used to annotate this entity
	 * or {@literal null} if this entity was not annotated with a {@link Region} {@link Annotation}.
	 *
	 * @return the {@link Class} type of the {@link Region} {@link Annotation} used to annotate this entity
	 * or {@literal null} if this entity was not annotated with a {@link Region} {@link Annotation}.
	 * @see java.lang.annotation.Annotation#annotationType()
	 * @see #getRegionAnnotation()
	 */
	public Class<? extends Annotation> getRegionAnnotationType() {
		return Optional.ofNullable(getRegionAnnotation())
			.map((annotation) -> ((Annotation) annotation).annotationType())
				.orElse(null);
	}

	/**
	 * Returns the {@link String name} of the {@link org.apache.geode.cache.Region}
	 * in which this {@link PersistentEntity} will be stored.
	 *
	 * @return the {@link String name} of the {@link org.apache.geode.cache.Region}
	 * in which this {@link PersistentEntity} will be stored.
	 * @see org.apache.geode.cache.Region#getName()
	 */
	@NonNull
	public String getRegionName() {
		return this.regionName;
	}

	/**
	 * @inheritDoc
	 * @see org.springframework.data.mapping.model.BasicPersistentEntity#returnPropertyIfBetterIdPropertyCandidateOrNull(PersistentProperty)
	 */
	@Override
	protected GemfirePersistentProperty returnPropertyIfBetterIdPropertyCandidateOrNull(
			GemfirePersistentProperty property) {

		if (property.isIdProperty()) {

			GemfirePersistentProperty idProperty = getIdProperty();

			if (idProperty != null) {
				if (idProperty.isExplicitIdProperty()) {
					if (property.isExplicitIdProperty()) {
						throw new MappingException(String.format(
							"Attempt to add explicit id property [%1$s] but already have id property [%2$s] registered as explicit;"
								+ " Please check your object [%3$s] mapping configuration",
									property.getName(), idProperty.getName(), getType().getName()));
					}

					return null;
				}

				return property.isExplicitIdProperty() ? property : null;
			}
			else  {
				return property;
			}
		}

		return null;
	}
}
