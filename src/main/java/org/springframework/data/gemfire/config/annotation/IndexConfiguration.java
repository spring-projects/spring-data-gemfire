/*
 * Copyright 2016 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Annotation;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.query.Index;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.IndexFactoryBean;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;
import org.springframework.data.gemfire.mapping.annotation.Indexed;
import org.springframework.data.gemfire.mapping.annotation.LuceneIndexed;
import org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean;
import org.springframework.data.mapping.PropertyHandler;
import org.springframework.util.StringUtils;

/**
 * The {@link IndexConfiguration} class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * and extension of {@link EntityDefinedRegionsConfiguration} used in the {@link EnableIndexes} annotation
 * to dynamically create GemFire/Geode {@link org.apache.geode.cache.Region} {@link Index Indexes} based on
 * {@link GemfirePersistentEntity} {@link GemfirePersistentProperty} annotations.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.data.annotation.Id
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.springframework.data.gemfire.IndexType
 * @see org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EntityDefinedRegionsConfiguration
 * @see org.springframework.data.gemfire.mapping.GemfirePersistentEntity
 * @see org.springframework.data.gemfire.mapping.GemfirePersistentProperty
 * @see org.springframework.data.gemfire.mapping.annotation.Indexed
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.query.Index
 * @since 1.9.0
 */
public class IndexConfiguration extends EntityDefinedRegionsConfiguration {

	/**
	 * Returns the {@link Annotation} {@link Class type} that configures and creates {@link Region} Indexes
	 * from application persistent entity properties.
	 *
	 * @return the {@link Annotation} {@link Class type} that configures and creates {@link Region Region} Indexes
	 * from application persistent entity properties.
	 * @see org.springframework.data.gemfire.config.annotation.EnableIndexes
	 * @see java.lang.annotation.Annotation
	 * @see java.lang.Class
	 */
	protected Class<? extends Annotation> getEnableIndexesAnnotationType() {
		return EnableIndexes.class;
	}

	/**
	 * Returns the name of the {@link Annotation} {@link Class type} that configures and creates {@link Region} Indexes
	 * from application persistent entity properties.
	 *
	 * @return the name of the {@link Annotation} {@link Class type} that configures and creates {@link Region Region}
	 * Indexes from application persistent entity properties.
	 * @see #getEnableIndexesAnnotationType()
	 * @see java.lang.Class#getName()
	 */
	protected String getEnableIndexesAnnotationTypeName() {
		return getEnableIndexesAnnotationType().getName();
	}

	/**
	 * Returns the simple name of the {@link Annotation} {@link Class type} that configures and creates {@link Region}
	 * Indexes from application persistent entity properties.
	 *
	 * @return the simple name of the {@link Annotation} {@link Class type} that configures and creates
	 * {@link Region Region} Indexes from application persistent entity properties.
	 * @see #getEnableIndexesAnnotationType()
	 * @see java.lang.Class#getSimpleName()
	 */
	@SuppressWarnings("unused")
	protected String getEnableIndexesAnnotationTypeSimpleName() {
		return getEnableIndexesAnnotationType().getSimpleName();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected GemfirePersistentEntity<?> postProcess(AnnotationMetadata importingClassMetadata,
			BeanDefinitionRegistry registry, GemfirePersistentEntity<?> persistentEntity) {

		GemfirePersistentEntity<?> localPersistentEntity =
			super.postProcess(importingClassMetadata, registry, persistentEntity);

		if (isAnnotationPresent(importingClassMetadata, getEnableIndexesAnnotationTypeName())) {
			AnnotationAttributes enableIndexesAttributes =
				getAnnotationAttributes(importingClassMetadata, getEnableIndexesAnnotationTypeName());

			localPersistentEntity.doWithProperties((PropertyHandler<GemfirePersistentProperty>) persistentProperty -> {
				Optional.ofNullable(persistentProperty.findAnnotation(Id.class)).ifPresent(idAnnotation ->
					registerIndexBeanDefinition(enableIndexesAttributes, localPersistentEntity, persistentProperty,
						IndexType.KEY, idAnnotation, registry));

				Optional.ofNullable(persistentProperty.findAnnotation(Indexed.class)).ifPresent(indexedAnnotation ->
					registerIndexBeanDefinition(enableIndexesAttributes, localPersistentEntity, persistentProperty,
						indexedAnnotation.type(), indexedAnnotation, registry));

				Optional.ofNullable(persistentProperty.findAnnotation(LuceneIndexed.class)).ifPresent(
					luceneIndexAnnotation -> registerLuceneIndexBeanDefinition(enableIndexesAttributes,
						localPersistentEntity, persistentProperty, luceneIndexAnnotation, registry));
			});
		}

		return persistentEntity;
	}

	/**
	 * Registers an Index of the given {@link IndexType} for the {@link GemfirePersistentProperty}
	 * on the {@link GemfirePersistentEntity} using the {@link Annotation} meta-data to define the Index.
	 *
	 * @param enableIndexesAttributes {@link AnnotationAttributes} containing meta-data
	 * for the {@link EnableIndexes} annotation.
	 * @param persistentEntity {@link GemfirePersistentEntity} containing the {@link GemfirePersistentProperty}
	 * to be indexed.
	 * @param persistentProperty {@link GemfirePersistentProperty} for which the Index will be created.
	 * @param indexType {@link IndexType} enum specifying the Index type (e.g. KEY, HASH, etc).
	 * @param indexAnnotation Index {@link Annotation}.
	 * @param registry {@link BeanDefinitionRegistry} used to register the Index bean definition.
	 * @see java.lang.annotation.Annotation
	 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
	 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
	 * @see org.springframework.data.gemfire.IndexType
	 * @see org.springframework.data.gemfire.mapping.GemfirePersistentEntity
	 * @see org.springframework.data.gemfire.mapping.GemfirePersistentProperty
	 */
	protected void registerIndexBeanDefinition(AnnotationAttributes enableIndexesAttributes,
			GemfirePersistentEntity<?> persistentEntity, GemfirePersistentProperty persistentProperty,
			IndexType indexType, Annotation indexAnnotation, BeanDefinitionRegistry registry) {

		Optional.ofNullable(indexAnnotation).ifPresent(localIndexAnnotation -> {
			AnnotationAttributes indexedAttributes = getAnnotationAttributes(localIndexAnnotation);

			BeanDefinitionBuilder indexFactoryBeanBuilder =
				BeanDefinitionBuilder.genericBeanDefinition(IndexFactoryBean.class);

			String indexName = resolveName(persistentEntity, persistentProperty, indexedAttributes, indexType);

			indexFactoryBeanBuilder.addPropertyReference("cache", GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

			indexFactoryBeanBuilder.addPropertyValue("define", resolveDefine(enableIndexesAttributes));

			indexFactoryBeanBuilder.addPropertyValue("expression",
				resolveExpression(persistentEntity, persistentProperty, indexedAttributes));

			indexFactoryBeanBuilder.addPropertyValue("from",
				resolveFrom(persistentEntity, persistentProperty, indexedAttributes));

			indexFactoryBeanBuilder.addPropertyValue("name", indexName);

			/*
			indexFactoryBeanBuilder.addPropertyValue("override",
				resolveOverride(persistentEntity, persistentProperty, indexedAttributes));
			*/

			indexFactoryBeanBuilder.addPropertyValue("type",
				resolveType(persistentEntity, persistentProperty, indexedAttributes, indexType).toString());

			registry.registerBeanDefinition(indexName, indexFactoryBeanBuilder.getBeanDefinition());
		});
	}

	/**
	 * Registers a {@link LuceneIndex} for the {@link GemfirePersistentProperty} on the {@link GemfirePersistentEntity}
	 * using the {@link Annotation} meta-data to define the Index.
	 *
	 * @param enableIndexesAttributes {@link AnnotationAttributes} containing meta-data
	 * for the {@link EnableIndexes} annotation.
	 * @param persistentEntity {@link GemfirePersistentEntity} containing the {@link GemfirePersistentProperty}
	 * to be indexed.
	 * @param persistentProperty {@link GemfirePersistentProperty} for which the {@link LuceneIndex} will be created.
	 * @param luceneIndexAnnotation {@link LuceneIndexed} {@link Annotation}.
	 * @param registry {@link BeanDefinitionRegistry} used to register the {@link LuceneIndex} bean definition.
	 * @see java.lang.annotation.Annotation
	 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
	 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
	 * @see org.springframework.data.gemfire.mapping.GemfirePersistentEntity
	 * @see org.springframework.data.gemfire.mapping.GemfirePersistentProperty
	 * @see org.springframework.data.gemfire.mapping.annotation.LuceneIndexed
	 */
	@SuppressWarnings("unused")
	protected void registerLuceneIndexBeanDefinition(AnnotationAttributes enableIndexesAttributes,
			GemfirePersistentEntity<?> persistentEntity, GemfirePersistentProperty persistentProperty,
			Annotation luceneIndexAnnotation, BeanDefinitionRegistry registry) {

		Optional.ofNullable(luceneIndexAnnotation).ifPresent(localLuceneIndexAnnotation -> {
			AnnotationAttributes luceneIndexAttributes =
				AnnotationAttributes.fromMap(AnnotationUtils.getAnnotationAttributes(localLuceneIndexAnnotation));

			BeanDefinitionBuilder luceneIndexFactoryBeanBuilder =
				BeanDefinitionBuilder.genericBeanDefinition(LuceneIndexFactoryBean.class);

			String indexName = luceneIndexAttributes.getString("name");

			boolean destroy = (luceneIndexAttributes.containsKey("destroy")
				&& luceneIndexAttributes.getBoolean("destroy"));

			luceneIndexFactoryBeanBuilder.addPropertyValue("destroy", destroy);

			luceneIndexFactoryBeanBuilder.addPropertyValue("fields", persistentProperty.getName());

			luceneIndexFactoryBeanBuilder.addPropertyValue("indexName", indexName);

			luceneIndexFactoryBeanBuilder.addPropertyValue("regionPath", persistentEntity.getRegionName());

			registry.registerBeanDefinition(indexName, luceneIndexFactoryBeanBuilder.getBeanDefinition());
		});
	}

	/* (non-Javadoc) */
	private boolean resolveDefine(AnnotationAttributes enableIndexesAnnotationAttributes) {
		return (enableIndexesAnnotationAttributes.containsKey("define")
			&& enableIndexesAnnotationAttributes.getBoolean("define"));
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unused")
	private String resolveExpression(GemfirePersistentEntity<?> persistentEntity,
			GemfirePersistentProperty persistentProperty, AnnotationAttributes indexedAttributes) {

		String expression = (indexedAttributes.containsKey("expression")
			? indexedAttributes.getString("expression") : null);

		return (StringUtils.hasText(expression) ? expression : persistentProperty.getName());
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unused")
	private String resolveFrom(GemfirePersistentEntity<?> persistentEntity,
			GemfirePersistentProperty persistentProperty, AnnotationAttributes indexedAttributes) {

		String from = (indexedAttributes.containsKey("from")
			? indexedAttributes.getString("from") : null);

		return (StringUtils.hasText(from) ? from : persistentEntity.getRegionName());
	}

	/* (non-Javadoc) */
	private String resolveName(GemfirePersistentEntity<?> persistentEntity,
			GemfirePersistentProperty persistentProperty, AnnotationAttributes indexedAttributes, IndexType indexType) {

		String indexName = (indexedAttributes.containsKey("name")
			? indexedAttributes.getString("name") : null);

		return (StringUtils.hasText(indexName) ? indexName
			: generateIndexName(persistentEntity, persistentProperty, indexType));
	}

	/* (non-Javadoc) */
	private String generateIndexName(GemfirePersistentEntity persistentEntity,
			GemfirePersistentProperty persistentProperty, IndexType indexType) {

		return String.format("%1$s%2$s%3$sIdx", persistentEntity.getRegionName(),
			StringUtils.capitalize(persistentProperty.getName()),
				StringUtils.capitalize(indexType.name().toLowerCase()));
	}

	/* (non-Javadoc) */
	/*
	@SuppressWarnings("unused")
	private boolean resolveOverride(GemfirePersistentEntity persistentEntity,
			GemfirePersistentProperty persistentProperty, AnnotationAttributes indexedAttributes) {

		return (indexedAttributes.containsKey("override")
			&& indexedAttributes.getBoolean("override"));
	}
	*/

	/* (non-Javadoc) */
	@SuppressWarnings("unused")
	private IndexType resolveType(GemfirePersistentEntity<?> persistentEntity,
			GemfirePersistentProperty persistentProperty, AnnotationAttributes indexedAttributes, IndexType indexType) {

		IndexType resolvedIndexType = (indexedAttributes.containsKey("type")
			? indexedAttributes.getEnum("type") : null);

		return Optional.ofNullable(resolvedIndexType).orElse(indexType);
	}
}
