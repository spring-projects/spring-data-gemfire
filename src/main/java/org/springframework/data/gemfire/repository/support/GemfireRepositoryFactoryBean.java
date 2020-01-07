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
package org.springframework.data.gemfire.repository.support;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.OrderComparator;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;
import org.springframework.data.gemfire.repository.query.GemfireRepositoryQuery;
import org.springframework.data.gemfire.repository.query.QueryPostProcessor;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.RepositoryDefinition;
import org.springframework.data.repository.core.support.QueryCreationListener;
import org.springframework.data.repository.core.support.RepositoryFactoryBeanSupport;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;
import org.springframework.data.util.ClassTypeInformation;
import org.springframework.data.util.TypeInformation;
import org.springframework.lang.NonNull;
import org.springframework.util.Assert;

/**
 * {@link FactoryBean} adapter for {@link GemfireRepositoryFactory}.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationContextAware
 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
 * @see org.springframework.data.gemfire.mapping.GemfirePersistentEntity
 * @see org.springframework.data.gemfire.mapping.GemfirePersistentProperty
 * @see org.springframework.data.mapping.context.MappingContext
 * @see org.springframework.data.repository.Repository
 * @see org.springframework.data.repository.core.support.RepositoryFactoryBeanSupport
 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
 * @see org.apache.geode.cache.Region
 */
public class GemfireRepositoryFactoryBean<T extends Repository<S, ID>, S, ID>
		extends RepositoryFactoryBeanSupport<T, S, ID> implements ApplicationContextAware {

	private ApplicationContext applicationContext;

	private GemFireCache cache;

	private Iterable<Region<?, ?>> regions;

	private MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext;

	/**
	 * Creates a new {@link GemfireRepositoryFactoryBean} for the given repository interface.
	 *
	 * @param repositoryInterface must not be {@literal null}.
	 */
	public GemfireRepositoryFactoryBean(Class<? extends T> repositoryInterface) {
		super(repositoryInterface);
	}

	/**
	 * Sets a reference to the Spring {@link ApplicationContext}.
	 *
	 * @param applicationContext reference to the Spring {@link ApplicationContext}.
	 * @see org.springframework.context.ApplicationContextAware#setApplicationContext(ApplicationContext)
	 * @see org.springframework.context.ApplicationContext
	 */
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {

		this.applicationContext = applicationContext;

		Map<String, Region> regionBeans = applicationContext.getBeansOfType(Region.class);

		Set<Region<?, ?>> regions = new HashSet<>(Collections.unmodifiableSet(regionBeans.entrySet().stream()
			.<Region<?, ?>>map(Map.Entry::getValue)
			.collect(Collectors.toSet())));

		getCache().map(GemFireCache::rootRegions).ifPresent(regions::addAll);

		this.regions = regions;
	}

	/**
	 * Returns an {@link Optional} reference to the configured Spring {@link ApplicationContext}.
	 *
	 * @return an {@link Optional} reference to the configured Spring {@link ApplicationContext}.
	 * @see org.springframework.context.ApplicationContext
	 * @see java.util.Optional
	 */
	protected Optional<ApplicationContext> getApplicationContext() {
		return Optional.ofNullable(this.applicationContext);
	}

	/**
	 * Set a reference to the {@link GemFireCache}.
	 *
	 * @param cache reference to the {@link GemFireCache}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	public void setCache(GemFireCache cache) {
		this.cache = cache;
	}

	/**
	 * Returns an {@link Optional} reference to the configured {@link GemFireCache}.
	 *
	 * @return an {@link Optional} reference to the configured {@link GemFireCache}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	protected Optional<GemFireCache> getCache() {
		return Optional.ofNullable(this.cache);
	}

	/**
	 * Configures the {@link MappingContext} used to perform application domain object type to data store mappings.
	 *
	 * @param mappingContext {@link MappingContext} to configure.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see org.springframework.data.mapping.context.MappingContext
	 */
	@Autowired(required = false)
	public void setGemfireMappingContext(MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext) {
		setMappingContext(mappingContext);
		this.mappingContext = mappingContext;
	}

	/**
	 * Returns a reference to the Spring Data {@link MappingContext} used to perform application domain object type
	 * to data store mappings.
	 *
	 * @return a reference to the {@link MappingContext}.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see org.springframework.data.mapping.context.MappingContext
	 * @see #setGemfireMappingContext(MappingContext)
	 */
	protected MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> getGemfireMappingContext() {
		return this.mappingContext;
	}

	/**
	 * Attempts to resolve the {@link MappingContext} used to map {@link GemfirePersistentEntity entities}
	 * to Pivotal GemFire.
	 *
	 * @return a reference to the resolved {@link MappingContext}.
	 * @throws IllegalStateException if the {@link MappingContext} cannot be resolved.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see org.springframework.data.mapping.context.MappingContext
	 */
	private MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> resolveGemfireMappingContext() {

		MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext =
			getGemfireMappingContext();

		Assert.state(mappingContext != null, "GemfireMappingContext must not be null");

		return mappingContext;
	}

	/**
	 * Returns an {@link Iterable} reference to the {@link Region Regions}
	 * defined in the Spring {@link ApplicationContext}.
	 *
	 * @return a reference to all {@link Region Regions} defined in the Spring {@link ApplicationContext}.
	 * @see org.apache.geode.cache.Region
	 * @see java.lang.Iterable
	 */
	protected Iterable<Region<?, ?>> getRegions() {
		return this.regions;
	}

	/**
	 * Creates an instance of {@link RepositoryFactorySupport} that interfaces with GemFire.
	 *
	 * @see org.springframework.data.repository.core.support.RepositoryFactoryBeanSupport#createRepositoryFactory()
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
	 */
	@Override
	protected RepositoryFactorySupport createRepositoryFactory() {

		GemfireRepositoryFactory repositoryFactory =
			new GemfireRepositoryFactory(getRegions(), getGemfireMappingContext());

		getApplicationContext()
			.map(applicationContext -> new QueryPostProcessorRegistrationOnQueryCreationListener(applicationContext))
			.ifPresent(repositoryFactory::addQueryCreationListener);

		return repositoryFactory;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.core.support.RepositoryFactoryBeanSupport#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() {
		resolveGemfireMappingContext();
		super.afterPropertiesSet();
	}

	protected class QueryPostProcessorRegistrationOnQueryCreationListener
			implements QueryCreationListener<GemfireRepositoryQuery> {

		private Iterable<QueryPostProcessorMetadata> queryPostProcessorsMetadata;

		public QueryPostProcessorRegistrationOnQueryCreationListener(ApplicationContext applicationContext) {

			Assert.notNull(applicationContext, "ApplicationContext must not be null");

			List<QueryPostProcessor> queryPostProcessors =
				new ArrayList<>(applicationContext.getBeansOfType(QueryPostProcessor.class).values());

			queryPostProcessors.sort(OrderComparator.INSTANCE);

			this.queryPostProcessorsMetadata = queryPostProcessors.stream()
				.map(QueryPostProcessorMetadata::from)
				.collect(Collectors.toList());
		}

		protected Iterable<QueryPostProcessorMetadata> getQueryPostProcessorsMetadata() {
			return this.queryPostProcessorsMetadata;
		}

		@Override
		public void onCreation(GemfireRepositoryQuery repositoryQuery) {

			Class<?> repositoryInterface = getRepositoryInformation().getRepositoryInterface();

			StreamSupport.stream(getQueryPostProcessorsMetadata().spliterator(), false)
				.filter(queryPostProcessorMetadata -> queryPostProcessorMetadata.isMatch(repositoryInterface))
				.forEach(queryPostProcessorMetadata -> queryPostProcessorMetadata.register(repositoryQuery));
		}
	}

	static class QueryPostProcessorMetadata {

		private static final Map<QueryPostProcessorKey, QueryPostProcessorMetadata> cache = new WeakHashMap<>();

		private final Class<?> declaredRepositoryType;

		private final QueryPostProcessor<?, ?> queryPostProcessor;

		static QueryPostProcessorMetadata from(@NonNull QueryPostProcessor<?, ?> queryPostProcessor) {

			return cache.computeIfAbsent(QueryPostProcessorKey.of(queryPostProcessor),
				key -> new QueryPostProcessorMetadata(key.getQueryPostProcessor()));
		}

		@SuppressWarnings("unchecked")
		QueryPostProcessorMetadata(@NonNull QueryPostProcessor<?, ?> queryPostProcessor) {

			Assert.notNull(queryPostProcessor, "QueryPostProcessor must not be null");

			this.queryPostProcessor = queryPostProcessor;

			List<TypeInformation<?>> typeArguments = ClassTypeInformation.from(queryPostProcessor.getClass())
				.getRequiredSuperTypeInformation(QueryPostProcessor.class)
				.getTypeArguments();

			this.declaredRepositoryType = Optional.of(typeArguments)
				.filter(list -> !list.isEmpty())
				.map(list -> list.get(0))
				.map(typeInfo -> typeInfo.getType())
				.orElse((Class) Repository.class);
		}

		@NonNull
		Class<?> getDeclaredRepositoryType() {
			return this.declaredRepositoryType;
		}

		@NonNull
		@SuppressWarnings("unchecked")
		QueryPostProcessor<?, String> getQueryPostProcessor() {
			return (QueryPostProcessor<?, String>) this.queryPostProcessor;
		}

		boolean isMatch(Class<?> repositoryInterface) {

			return repositoryInterface != null
				&& (getDeclaredRepositoryType().isAssignableFrom(repositoryInterface)
					|| repositoryInterface.isAnnotationPresent(RepositoryDefinition.class));
		}

		GemfireRepositoryQuery register(GemfireRepositoryQuery repositoryQuery) {

			repositoryQuery.register(getQueryPostProcessor());

			return repositoryQuery;
		}

		@EqualsAndHashCode
		@RequiredArgsConstructor(staticName = "of")
		private static class QueryPostProcessorKey {

			@lombok.NonNull @Getter
			QueryPostProcessor<?, ?> queryPostProcessor;

		}
	}
}
