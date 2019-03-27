/*
 * Copyright 2012-2019 the original author or authors.
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

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeList;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import com.gemstone.gemfire.cache.Region;

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
 * @see com.gemstone.gemfire.cache.Region
 */
public class GemfireRepositoryFactoryBean<T extends Repository<S, ID>, S, ID extends Serializable>
		extends RepositoryFactoryBeanSupport<T, S, ID> implements ApplicationContextAware {

	private ApplicationContext applicationContext;

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
	 * Sets a reference to the Spring {@link ApplicationContext} in which this object runs.
	 *
	 * @param applicationContext the Spring {@link ApplicationContext} reference.
	 * @see org.springframework.context.ApplicationContextAware#setApplicationContext(ApplicationContext)
	 * @see org.springframework.context.ApplicationContext
	 */
	@Override
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {

		this.applicationContext = applicationContext;

		Collection<Region> regions = applicationContext.getBeansOfType(Region.class).values();

		this.regions = (Iterable) Collections.unmodifiableCollection(regions);
	}

	/**
	 * Configures the {@link MappingContext} used to perform domain object type to store mappings.
	 *
	 * @param mappingContext the {@link MappingContext} to set.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see org.springframework.data.mapping.context.MappingContext
	 */
	@Autowired(required = false)
	public void setGemfireMappingContext(MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext) {
		setMappingContext(mappingContext);
		this.mappingContext = mappingContext;
	}

	/**
	 * Returns a reference to the Spring Data {@link MappingContext} used to perform domain object type
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
	 * Returns an {@link Iterable} reference to the GemFire {@link Region}s defined
	 * in the Spring {@link ApplicationContext}.
	 *
	 * @return a reference to all GemFire {@link Region}s defined in the Spring {@link ApplicationContext}.
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

		if (this.applicationContext != null) {

			QueryCreationListener<GemfireRepositoryQuery> listener =
				new QueryPostProcessorRegistrationOnQueryCreationListener(this.applicationContext);

			repositoryFactory.addQueryCreationListener(listener);
		}

		return repositoryFactory;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.core.support.RepositoryFactoryBeanSupport#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() {
		Assert.state(getGemfireMappingContext() != null, "GemfireMappingContext must not be null");
		super.afterPropertiesSet();
	}

	protected class QueryPostProcessorRegistrationOnQueryCreationListener
		implements QueryCreationListener<GemfireRepositoryQuery> {

		private Iterable<QueryPostProcessorMetadata> queryPostProcessorsMetadata;

		protected QueryPostProcessorRegistrationOnQueryCreationListener(ApplicationContext applicationContext) {

			Assert.notNull(applicationContext, "ApplicationContext must not be null");

			List<QueryPostProcessor> queryPostProcessors =
				new ArrayList<QueryPostProcessor>(applicationContext.getBeansOfType(QueryPostProcessor.class).values());

			Collections.sort(queryPostProcessors, OrderComparator.INSTANCE);

			List<QueryPostProcessorMetadata> queryPostProcessorsMetadata =
				new ArrayList<QueryPostProcessorMetadata>();

			for (QueryPostProcessor queryPostProcessor : queryPostProcessors) {
				queryPostProcessorsMetadata.add(QueryPostProcessorMetadata.from(queryPostProcessor));
			}

			this.queryPostProcessorsMetadata = queryPostProcessorsMetadata;
		}

		protected Iterable<QueryPostProcessorMetadata> getQueryPostProcessorsMetadata() {
			return this.queryPostProcessorsMetadata;
		}

		@Override
		public void onCreation(GemfireRepositoryQuery repositoryQuery) {

			Class<?> repositoryInterface = getRepositoryInformation().getRepositoryInterface();

			for (QueryPostProcessorMetadata metadata : getQueryPostProcessorsMetadata()) {
				if (metadata.isMatch(repositoryInterface)) {
					metadata.register(repositoryQuery);
				}
			}
		}
	}

	static class QueryPostProcessorMetadata {

		private static final Map<QueryPostProcessorKey, QueryPostProcessorMetadata> cache =
			new WeakHashMap<QueryPostProcessorKey, QueryPostProcessorMetadata>();

		private final Class<?> declaredRepositoryType;

		private final QueryPostProcessor<?, ?> queryPostProcessor;

		static synchronized QueryPostProcessorMetadata from(QueryPostProcessor<?, ?> queryPostProcessor) {

			QueryPostProcessorKey key = QueryPostProcessorKey.of(queryPostProcessor);

			QueryPostProcessorMetadata metadata = cache.get(key);

			if (metadata == null) {
				metadata = new QueryPostProcessorMetadata(key.getQueryPostProcessor());
				cache.put(key, metadata);
			}

			return metadata;
		}

		@SuppressWarnings("unchecked")
		QueryPostProcessorMetadata(QueryPostProcessor<?, ?> queryPostProcessor) {

			Assert.notNull(queryPostProcessor, "QueryPostProcessor must not be null");

			this.queryPostProcessor = queryPostProcessor;

			List<TypeInformation<?>> typeArguments = ClassTypeInformation.from(queryPostProcessor.getClass())
				.getSuperTypeInformation(QueryPostProcessor.class)
				.getTypeArguments();

			this.declaredRepositoryType = resolveDeclaredRepositoryType(typeArguments);
		}

		Class<?> resolveDeclaredRepositoryType(List<TypeInformation<?>> typeArguments) {
			return !nullSafeList(typeArguments).isEmpty() ? typeArguments.get(0).getType() : Repository.class;
		}

		Class<?> getDeclaredRepositoryType() {
			return this.declaredRepositoryType;
		}

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

		protected static class QueryPostProcessorKey {

			private QueryPostProcessor<?, ?> queryPostProcessor;

			public static QueryPostProcessorKey of(QueryPostProcessor queryPostProcessor) {

				Assert.notNull(queryPostProcessor, "QueryPostProcessor must not be null");

				QueryPostProcessorKey key = new QueryPostProcessorKey();

				key.queryPostProcessor = queryPostProcessor;

				return key;
			}

			protected QueryPostProcessor<?, ?> getQueryPostProcessor() {
				return queryPostProcessor;
			}

			@Override
			public boolean equals(Object obj) {

				if (this == obj) {
					return true;
				}

				if (!(obj instanceof QueryPostProcessorKey)) {
					return false;
				}

				QueryPostProcessorKey that = (QueryPostProcessorKey) obj;

				return this.getQueryPostProcessor().equals(that.getQueryPostProcessor());
			}

			@Override
			public int hashCode() {

				int hashValue = 17;

				hashValue = 37 * hashValue + getQueryPostProcessor().hashCode();

				return hashValue;
			}
		}
	}
}
