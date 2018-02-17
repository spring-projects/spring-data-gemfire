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

package org.springframework.data.gemfire.repository.support;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;
import org.springframework.data.gemfire.mapping.Regions;
import org.springframework.data.gemfire.repository.query.DefaultGemfireEntityInformation;
import org.springframework.data.gemfire.repository.query.GemfireEntityInformation;
import org.springframework.data.gemfire.repository.query.GemfireQueryMethod;
import org.springframework.data.gemfire.repository.query.PartTreeGemfireRepositoryQuery;
import org.springframework.data.gemfire.repository.query.StringBasedGemfireRepositoryQuery;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.data.projection.ProjectionFactory;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.core.NamedQueries;
import org.springframework.data.repository.core.RepositoryInformation;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;
import org.springframework.data.repository.query.EvaluationContextProvider;
import org.springframework.data.repository.query.QueryLookupStrategy;
import org.springframework.data.repository.query.QueryLookupStrategy.Key;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * {@link RepositoryFactorySupport} implementation creating repository proxies
 * for Gemfire.
 *
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 */
public class GemfireRepositoryFactory extends RepositoryFactorySupport {

	private static final Class<org.springframework.data.gemfire.mapping.annotation.Region> REGION_ANNOTATION =
		org.springframework.data.gemfire.mapping.annotation.Region.class;

	static final String REGION_NOT_FOUND = "Region [%1$s] for Domain Type [%2$s] using Repository [%3$s] was not found;"
		+ " You must configure a Region with name [%1$s] in the application context";

	static final String REGION_REPOSITORY_ID_TYPE_MISMATCH =
		"Region [%1$s] requires keys of type [%2$s], but Repository [%3$s] declared an id of type [%4$s]";

	static final String REPOSITORY_ENTITY_ID_TYPE_MISMATCH =
		"Repository [%1$s] declared an id of type [%2$s], but entity [%3$s] has an id of type [%4$s]";

	private final MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext;

	private final Regions regions;

	/**
	 * Constructs a new instance of {@link GemfireRepositoryFactory} initialized with the given collection
	 * of configured {@link Region Regions} and the {@link MappingContext}.
	 *
	 * @param regions {@link Iterable} collection of configured {@link Region Regions} used by this application;
	 * must not be {@literal null}.
	 * @param mappingContext {@link MappingContext} used to map entities to the underlying data store,
	 * must not be {@literal null}.
	 * @throws IllegalArgumentException if either {@link Regions} or the {@link MappingContext} is {@literal null}.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see org.springframework.data.gemfire.mapping.Regions
	 * @see org.springframework.data.mapping.context.MappingContext
	 */
	public GemfireRepositoryFactory(@NonNull Iterable<Region<?, ?>> regions,
			@NonNull MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext) {

		Assert.notNull(regions, "Regions are required");
		Assert.notNull(mappingContext, "MappingContext is required");

		this.regions = new Regions(regions, mappingContext);
		this.mappingContext = mappingContext;
	}

	/**
	 * Returns a reference to the GemFire {@link MappingContext} used to provide mapping meta-data
	 * between {@link Class entity types} and the data store.
	 *
	 * @return a reference to the GemFire {@link MappingContext}.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see org.springframework.data.mapping.context.MappingContext
	 */
	protected MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> getMappingContext() {
		return this.mappingContext;
	}

	/**
	 * Returns a reference to the configured, application-defined {@link Region Regions}.
	 *
	 * @return a reference to the configured, application-defined {@link Region Regions}.
	 * @see org.springframework.data.gemfire.mapping.Regions
	 */
	protected Regions getRegions() {
		return this.regions;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport#getEntityInformation(java.lang.Class)
	 */
	@Override
	public <T, ID> GemfireEntityInformation<T, ID> getEntityInformation(Class<T> domainClass) {
		return new DefaultGemfireEntityInformation<>(resolvePersistentEntity(domainClass));
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
	 * 	#getRepositoryBaseClass(org.springframework.data.repository.core.RepositoryMetadata)
	 */
	@Override
	protected Class<?> getRepositoryBaseClass(RepositoryMetadata metadata) {
		return SimpleGemfireRepository.class;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport#getTargetRepository(org.springframework.data.repository.core.RepositoryInformation)
	 */
	@Override
	protected Object getTargetRepository(RepositoryInformation repositoryInformation) {

		GemfireEntityInformation<?, Serializable> entityInformation =
			getEntityInformation(repositoryInformation.getDomainType());

		GemfireTemplate gemfireTemplate = newTemplate(repositoryInformation);

		return getTargetRepositoryViaReflection(repositoryInformation, gemfireTemplate, entityInformation);
	}

	/**
	 * Constructs a new instance of {@link GemfireTemplate} initialized with the identified {@link Region}
	 * used to back all persistent, data access operations defined by the {@link Repository}.
	 *
	 * @param repositoryMetadata {@link RepositoryMetadata} containing meta-data about the {@link Repository}.
	 * @return a new instance of {@link GemfireTemplate} initialized with the identified {@link Region}.
	 * @see #resolvePersistentEntity(Class)
	 * @see #resolveRegion(RepositoryMetadata, GemfirePersistentEntity)
	 * @see #validate(RepositoryMetadata, GemfirePersistentEntity, Region)
	 * @see org.springframework.data.repository.core.RepositoryMetadata
	 * @see org.springframework.data.gemfire.GemfireTemplate
	 * @see org.apache.geode.cache.Region
	 */
	protected GemfireTemplate newTemplate(RepositoryMetadata repositoryMetadata) {

		GemfirePersistentEntity<?> entity = resolvePersistentEntity(repositoryMetadata.getDomainType());

		return new GemfireTemplate(validate(repositoryMetadata, entity, resolveRegion(repositoryMetadata, entity)));
	}

	@Nullable
	@SuppressWarnings("unchecked")
	private <T> GemfirePersistentEntity<T> resolvePersistentEntity(Class<?> domainType) {
		return (GemfirePersistentEntity<T>) getMappingContext().getPersistentEntity(domainType);
	}

	private Region<?, ?> resolveRegion(RepositoryMetadata repositoryMetadata, GemfirePersistentEntity entity) {

		String resolvedRegionName = getRepositoryRegionName(repositoryMetadata)
			.orElseGet(() -> getEntityRegionName(repositoryMetadata, entity));

		return resolveRegion(repositoryMetadata, resolvedRegionName);
	}

	private Region<?, ?> validate(RepositoryMetadata repositoryMetadata, GemfirePersistentEntity<?> entity,
			Region<?, ?> region) {

		Assert.notNull(region, "Region is required");

		Class<?> repositoryIdType = repositoryMetadata.getIdType();

		Optional.ofNullable(region.getAttributes().getKeyConstraint())
			.ifPresent(regionKeyType -> Assert.isTrue(regionKeyType.isAssignableFrom(repositoryIdType),
				() -> String.format(REGION_REPOSITORY_ID_TYPE_MISMATCH, region.getFullPath(), regionKeyType.getName(),
					repositoryMetadata.getRepositoryInterface().getName(), repositoryIdType.getName())));

		Optional.ofNullable(entity)
			.map(GemfirePersistentEntity::getIdProperty)
			.ifPresent(entityIdProperty -> Assert.isTrue(repositoryIdType.isAssignableFrom(entityIdProperty.getType()),
				() -> String.format(REPOSITORY_ENTITY_ID_TYPE_MISMATCH, repositoryMetadata.getRepositoryInterface().getName(),
					repositoryIdType.getName(), entityIdProperty.getOwner().getName(), entityIdProperty.getTypeName())));

		return region;
	}

	String getEntityRegionName(@NonNull RepositoryMetadata repositoryMetadata,
			@Nullable GemfirePersistentEntity entity) {

		Optional<GemfirePersistentEntity> optionalEntity = Optional.ofNullable(entity);

		return optionalEntity
			.map(GemfirePersistentEntity::getRegionName)
			.filter(StringUtils::hasText)
			.orElseGet(() -> optionalEntity
				.map(GemfirePersistentEntity::getType)
				.map(Class::getSimpleName)
				.orElseGet(() -> repositoryMetadata.getDomainType().getSimpleName()));
	}

	Optional<String> getRepositoryRegionName(@NonNull RepositoryMetadata repositoryMetadata) {

		return Optional.ofNullable(repositoryMetadata)
			.map(RepositoryMetadata::getRepositoryInterface)
			.filter(repositoryInterface -> repositoryInterface.isAnnotationPresent(REGION_ANNOTATION))
			.map(repositoryInterface -> repositoryInterface.getAnnotation(REGION_ANNOTATION))
			.map(regionAnnotation -> regionAnnotation.value())
			.filter(StringUtils::hasText);
	}

	Region<?, ?> resolveRegion(@NonNull RepositoryMetadata repositoryMetadata, String regionNamePath) {

		return Optional.ofNullable(getRegions().getRegion(regionNamePath))
			.orElseThrow(() -> newIllegalStateException(REGION_NOT_FOUND,
				regionNamePath, repositoryMetadata.getDomainType().getName(),
					repositoryMetadata.getRepositoryInterface().getName()));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
	 * 	#getQueryLookupStrategy(Key, EvaluationContextProvider)
	 */
	@Override
	protected Optional<QueryLookupStrategy> getQueryLookupStrategy(Key key,
			EvaluationContextProvider evaluationContextProvider) {

		return Optional.of((Method method, RepositoryMetadata repositoryMetadata, ProjectionFactory projectionFactory,
			NamedQueries namedQueries) -> {

				GemfireQueryMethod queryMethod =
					newQueryMethod(method, repositoryMetadata, projectionFactory, evaluationContextProvider);

				GemfireTemplate template = newTemplate(repositoryMetadata);

				if (queryMethod.hasAnnotatedQuery()) {
					return new StringBasedGemfireRepositoryQuery(queryMethod, template).asUserDefinedQuery();
				}

				String namedQueryName = queryMethod.getNamedQueryName();

				if (namedQueries.hasQuery(namedQueryName)) {
					return new StringBasedGemfireRepositoryQuery(namedQueries.getQuery(namedQueryName),
						queryMethod, template).asUserDefinedQuery();
				}

				return new PartTreeGemfireRepositoryQuery(queryMethod, template);
			});
	}

	@SuppressWarnings({ "unchecked", "unused" })
	protected <T extends QueryMethod> T newQueryMethod(Method method, RepositoryMetadata repositoryMetadata,
			ProjectionFactory projectionFactory, EvaluationContextProvider evaluationContextProvider) {

		return (T) new GemfireQueryMethod(method, repositoryMetadata, projectionFactory, getMappingContext());
	}
}
