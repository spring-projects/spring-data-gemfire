/*
 * Copyright 2012-2019 the original author or authors.
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

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheTransactionManager;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.SelectResults;
import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.GemfireCallback;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.Wrapper;
import org.springframework.data.gemfire.repository.query.QueryString;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.data.repository.core.EntityInformation;
import org.springframework.data.util.StreamUtils;
import org.springframework.data.util.Streamable;
import org.springframework.util.Assert;

/**
 * Basic Repository implementation for GemFire.
 *
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 * @see java.io.Serializable
 * @see org.springframework.data.gemfire.GemfireTemplate
 * @see org.springframework.data.gemfire.repository.GemfireRepository
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.Region
 */
public class SimpleGemfireRepository<T, ID> implements GemfireRepository<T, ID> {

	private final EntityInformation<T, ID> entityInformation;

	private final GemfireTemplate template;

	/**
	 * Creates a new {@link SimpleGemfireRepository}.
	 *
	 * @param template must not be {@literal null}.
	 * @param entityInformation must not be {@literal null}.
	 */
	public SimpleGemfireRepository(GemfireTemplate template, EntityInformation<T, ID> entityInformation) {

		Assert.notNull(template, "Template must not be null");
		Assert.notNull(entityInformation, "EntityInformation must not be null");

		this.template = template;
		this.entityInformation = entityInformation;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#save(S)
	 */
	@Override
	public <U extends T> U save(U entity) {

		ID id = this.entityInformation.getRequiredId(entity);

		this.template.put(id, entity);

		return entity;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#save(java.lang.Iterable)
	 */
	@Override
	public <U extends T> Iterable<U> saveAll(Iterable<U> entities) {

		Map<ID, U> entitiesToSave = new HashMap<>();

		entities.forEach(entity -> entitiesToSave.put(this.entityInformation.getRequiredId(entity), entity));

		this.template.putAll(entitiesToSave);

		return entitiesToSave.values();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.repository.GemfireRepository#save(org.springframework.data.gemfire.repository.Wrapper)
	 */
	@Override
	public T save(Wrapper<T, ID> wrapper) {

		T entity = wrapper.getEntity();

		this.template.put(wrapper.getKey(), entity);

		return entity;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#count()
	 */
	@Override
	public long count() {

		SelectResults<Integer> results =
			this.template.find(String.format("SELECT count(*) FROM %s", this.template.getRegion().getFullPath()));

		return Long.valueOf(results.iterator().next());
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#existsById(java.lang.Object)
	 */
	@Override
	public boolean existsById(ID id) {
		return findById(id).isPresent();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#findById(java.lang.Object)
	 */
	@Override
	public Optional<T> findById(ID id) {
		return Optional.ofNullable(this.template.get(id));
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#findAll()
	 */
	@Override
	public Collection<T> findAll() {

		SelectResults<T> results =
			this.template.find(String.format("SELECT * FROM %s", this.template.getRegion().getFullPath()));

		return results.asList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.repository.GemfireRepository.sort(:org.springframework.data.domain.Sort)
	 */
	@Override
	public Iterable<T> findAll(Sort sort) {

		QueryString query = QueryString.of("SELECT * FROM /RegionPlaceholder")
			.fromRegion(this.entityInformation.getJavaType(), this.template.getRegion())
			.orderBy(sort);

		SelectResults<T> selectResults = this.template.find(query.toString());

		return selectResults.asList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#findAllById(java.lang.Iterable)
	 */
	@Override
	public Collection<T> findAllById(Iterable<ID> ids) {

		List<ID> keys = Streamable.of(ids).stream().collect(StreamUtils.toUnmodifiableList());

		return CollectionUtils.<ID, T>nullSafeMap(this.template.getAll(keys)).values().stream()
			.filter(Objects::nonNull).collect(Collectors.toList());
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#deleteById(java.lang.Object)
	 */
	@Override
	public void deleteById(ID id) {
		this.template.remove(id);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#delete(java.lang.Object)
	 */
	@Override
	public void delete(T entity) {
		deleteById(this.entityInformation.getRequiredId(entity));
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#delete(java.lang.Iterable)
	 */
	@Override
	public void deleteAll(Iterable<? extends T> entities) {
		entities.forEach(this::delete);
	}

	/*
	 * (non-Javadoc)
	 * @see org.apache.geode.cache.Region#getAttributes()
	 * @see org.apache.geode.cache.RegionAttributes#getDataPolicy()
	 */
	boolean isPartitioned(Region<?, ?> region) {

		return region != null && region.getAttributes() != null
			&& isPartitioned(region.getAttributes().getDataPolicy());
	}

	/*
	 * (non-Javadoc)
	 * @see org.apache.geode.cache.DataPolicy#withPartitioning()
	 */
	boolean isPartitioned(DataPolicy dataPolicy) {
		return dataPolicy != null && dataPolicy.withPartitioning();
	}

	/*
	 * (non-Javadoc)
	 * @see org.apache.geode.cache.Region#getRegionService()
	 * @see org.apache.geode.cache.Cache#getCacheTransactionManager()
	 */
	boolean isTransactionPresent(Region<?, ?> region) {

		return region.getRegionService() instanceof Cache
			&& isTransactionPresent(((Cache) region.getRegionService()).getCacheTransactionManager());
	}

	/*
	 * (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#exists()
	 */
	boolean isTransactionPresent(CacheTransactionManager cacheTransactionManager) {
		return cacheTransactionManager != null && cacheTransactionManager.exists();
	}

	/* (non-Javadoc) */
	<K> void  doRegionClear(Region<K, ?> region) {
		region.removeAll(region.keySet());
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#deleteAll()
	 */
	@Override
	public void deleteAll() {
		this.template.execute((GemfireCallback<Void>) region -> {

			if (isPartitioned(region) || isTransactionPresent(region)) {
				doRegionClear(region);
			}
			else {
				try {
					region.clear();
				}
				catch (UnsupportedOperationException ignore) {
					doRegionClear(region);
				}
			}

			return null;
		});
	}
}
