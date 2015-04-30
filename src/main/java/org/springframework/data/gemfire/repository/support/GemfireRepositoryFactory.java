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
package org.springframework.data.gemfire.repository.support;

import java.io.Serializable;
import java.lang.reflect.Method;

import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;
import org.springframework.data.gemfire.mapping.Regions;
import org.springframework.data.gemfire.repository.query.DefaultGemfireEntityInformation;
import org.springframework.data.gemfire.repository.query.GemfireEntityInformation;
import org.springframework.data.gemfire.repository.query.GemfireQueryMethod;
import org.springframework.data.gemfire.repository.query.PartTreeGemfireRepositoryQuery;
import org.springframework.data.gemfire.repository.query.StringBasedGemfireRepositoryQuery;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.data.repository.core.NamedQueries;
import org.springframework.data.repository.core.RepositoryInformation;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;
import org.springframework.data.repository.query.QueryLookupStrategy;
import org.springframework.data.repository.query.QueryLookupStrategy.Key;
import org.springframework.data.repository.query.RepositoryQuery;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.Region;

/**
 * {@link RepositoryFactorySupport} implementation creating repository proxies
 * for Gemfire.
 * 
 * @author Oliver Gierke
 * @author David Turanski
 */
public class GemfireRepositoryFactory extends RepositoryFactorySupport {

	private final MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> context;

	private final Regions regions;

	/**
	 * Creates a new {@link GemfireRepositoryFactory}.
	 * 
	 * @param regions must not be {@literal null}.
	 * @param context the {@link MappingContext} used by the constructed Repository for mapping entities
	 * to the underlying data store.
	 */
	public GemfireRepositoryFactory(Iterable<Region<?, ?>> regions, MappingContext<? extends GemfirePersistentEntity<?>,
			GemfirePersistentProperty> context) {
		Assert.notNull(regions);
		this.context = context == null ? new GemfireMappingContext() : context;
		this.regions = new Regions(regions, this.context);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
	 * 	#getEntityInformation(java.lang.Class)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <T, ID extends Serializable> GemfireEntityInformation<T, ID> getEntityInformation(Class<T> domainClass) {
		GemfirePersistentEntity<T> entity = (GemfirePersistentEntity<T>) context.getPersistentEntity(domainClass);
		return new DefaultGemfireEntityInformation<T, ID>(entity);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
	 * 	#getTargetRepository(org.springframework.data.repository.core.RepositoryMetadata)
	 */
	@Override
	@SuppressWarnings({ "rawtypes", "unchecked" })
	protected Object getTargetRepository(RepositoryInformation repositoryInformation) {
		GemfireEntityInformation<?, Serializable> entityInformation = getEntityInformation(
			repositoryInformation.getDomainType());

		GemfireTemplate gemfireTemplate = getTemplate(repositoryInformation);

		return getTargetRepositoryViaReflection(repositoryInformation, gemfireTemplate, entityInformation);
	}

	private GemfireTemplate getTemplate(RepositoryMetadata metadata) {
		GemfirePersistentEntity<?> entity = context.getPersistentEntity(metadata.getDomainType());

		String entityRegionName = entity.getRegionName();
		String repositoryRegionName = getRepositoryRegionName(metadata.getRepositoryInterface());
		String regionName = (StringUtils.hasText(repositoryRegionName) ? repositoryRegionName : entityRegionName);

		Region<?, ?> region = regions.getRegion(regionName);

		if (region == null) {
			throw new IllegalStateException(String.format("No region '%s' found for domain class %s!"
				+ " Make sure you have configured a Gemfire region of that name in your application context!",
					regionName, metadata.getDomainType()));
		}

		Class<?> regionKeyType = region.getAttributes().getKeyConstraint();
		Class<?> entityIdType = metadata.getIdType();

		if (regionKeyType != null && entity.getIdProperty() != null) {
			Assert.isTrue(regionKeyType.isAssignableFrom(entityIdType), String.format(
				"The region referenced only supports keys of type %s but the entity to be stored has an id of type %s!",
					regionKeyType, entityIdType));
		}

		return new GemfireTemplate(region);
	}

	private String getRepositoryRegionName(final Class<?> repositoryClass) {
		return (repositoryClass.isAnnotationPresent(org.springframework.data.gemfire.mapping.Region.class) ?
			repositoryClass.getAnnotation(org.springframework.data.gemfire.mapping.Region.class).value() : null);
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
	 * @see springframework.data.repository.core.support.RepositoryFactorySupport
	 * 	#getQueryLookupStrategy(org.springframework.data.repository.query.QueryLookupStrategy.Key)
	 */
	@Override
	protected QueryLookupStrategy getQueryLookupStrategy(Key key) {
		return new QueryLookupStrategy() {
			@Override
			public RepositoryQuery resolveQuery(Method method, RepositoryMetadata metadata, NamedQueries namedQueries) {
				GemfireQueryMethod queryMethod = new GemfireQueryMethod(method, metadata, context);
				GemfireTemplate template = getTemplate(metadata);

				if (queryMethod.hasAnnotatedQuery()) {
					return new StringBasedGemfireRepositoryQuery(queryMethod, template).asUserDefinedQuery();
				}

				String namedQueryName = queryMethod.getNamedQueryName();

				if (namedQueries.hasQuery(namedQueryName)) {
					return new StringBasedGemfireRepositoryQuery(namedQueries.getQuery(namedQueryName), queryMethod,
						template).asUserDefinedQuery();
				}

				return new PartTreeGemfireRepositoryQuery(queryMethod, template);
			}
		};
	}

}
