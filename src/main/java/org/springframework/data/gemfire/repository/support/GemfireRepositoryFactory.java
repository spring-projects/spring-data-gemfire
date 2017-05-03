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

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.Optional;

import org.apache.geode.cache.Region;
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
import org.springframework.data.repository.core.NamedQueries;
import org.springframework.data.repository.core.RepositoryInformation;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;
import org.springframework.data.repository.query.EvaluationContextProvider;
import org.springframework.data.repository.query.QueryLookupStrategy;
import org.springframework.data.repository.query.QueryLookupStrategy.Key;
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

	private final MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext;

	private final Regions regions;

	/**
	 * Creates a new {@link GemfireRepositoryFactory}.
	 *
	 * @param regions must not be {@literal null}.
	 * @param mappingContext the {@link MappingContext} used by the constructed Repository for mapping entities
	 * to the underlying data store, must not be {@literal null}.
	 */
	public GemfireRepositoryFactory(Iterable<Region<?, ?>> regions,
			MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext) {

		Assert.notNull(regions, "Regions must not be null");
		Assert.notNull(mappingContext, "MappingContext must not be null");

		this.mappingContext = mappingContext;
		this.regions = new Regions(regions, this.mappingContext);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport#getEntityInformation(java.lang.Class)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <T, ID> GemfireEntityInformation<T, ID> getEntityInformation(Class<T> domainClass) {

		GemfirePersistentEntity<T> entity = (GemfirePersistentEntity<T>) mappingContext.getPersistentEntity(domainClass)
			.orElseThrow(() -> newIllegalArgumentException("Unable to resolve PersistentEntity for type [%s]",
				domainClass));

		return new DefaultGemfireEntityInformation<>(entity);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport#getTargetRepository(org.springframework.data.repository.core.RepositoryInformation)
	 */
	@Override
	protected Object getTargetRepository(RepositoryInformation repositoryInformation) {

		GemfireEntityInformation<?, Serializable> entityInformation =
			getEntityInformation(repositoryInformation.getDomainType());

		GemfireTemplate gemfireTemplate = getTemplate(repositoryInformation);

		return getTargetRepositoryViaReflection(repositoryInformation, gemfireTemplate, entityInformation);
	}

	GemfireTemplate getTemplate(RepositoryMetadata metadata) {

		GemfirePersistentEntity<?> entity = mappingContext.getPersistentEntity(metadata.getDomainType())
			.orElseThrow(() -> newIllegalArgumentException("Unable to resolve PersistentEntity for type [%s]",
				metadata.getDomainType()));

		String entityRegionName = entity.getRegionName();
		String repositoryRegionName = getRepositoryRegionName(metadata.getRepositoryInterface());
		String resolvedRegionName = StringUtils.hasText(repositoryRegionName) ? repositoryRegionName : entityRegionName;

		Region<?, ?> region = regions.getRegion(resolvedRegionName);

		if (region == null) {
			throw newIllegalStateException("No Region [%1$s] was found for domain class [%2$s];"
				+ " Make sure you have configured a GemFire Region of that name in your application context",
					resolvedRegionName, metadata.getDomainType().getName());
		}

		Class<?> regionKeyType = region.getAttributes().getKeyConstraint();
		Class<?> entityIdType = metadata.getIdType();

		if (regionKeyType != null && entity.getIdProperty() != null) {
			Assert.isTrue(regionKeyType.isAssignableFrom(entityIdType), String.format(
				"The Region referenced only supports keys of type [%1$s], but the entity to be stored has an id of type [%2$s]",
					regionKeyType.getName(), entityIdType.getName()));
		}

		return new GemfireTemplate(region);
	}

	String getRepositoryRegionName(Class<?> repositoryInterface) {
		return (repositoryInterface.isAnnotationPresent(org.springframework.data.gemfire.mapping.annotation.Region.class) ?
			repositoryInterface.getAnnotation(org.springframework.data.gemfire.mapping.annotation.Region.class).value() : null);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
	 * 	#getRepositoryBaseClass(org.springframework.data.repository.core.RepositoryMetadata)
	 */
	@Override
	protected Class<?> getRepositoryBaseClass(RepositoryMetadata metadata) {
		return SimpleGemfireRepository.class;
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

		return Optional.of(
			(Method method, RepositoryMetadata metadata, ProjectionFactory factory, NamedQueries namedQueries) -> {

				GemfireQueryMethod queryMethod = new GemfireQueryMethod(method, metadata, factory, mappingContext);
				GemfireTemplate template = getTemplate(metadata);

				if (queryMethod.hasAnnotatedQuery()) {
					return new StringBasedGemfireRepositoryQuery(queryMethod, template).asUserDefinedQuery();
				}

				if (namedQueries.hasQuery(queryMethod.getNamedQueryName())) {
					return new StringBasedGemfireRepositoryQuery(namedQueries.getQuery(queryMethod.getNamedQueryName()),
						queryMethod, template).asUserDefinedQuery();
				}

				return new PartTreeGemfireRepositoryQuery(queryMethod, template);
			});
	}
}
