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

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
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
	 * Constructs a new instance of {@link SimpleGemfireRepository} initialized with the {@link GemfireTemplate}
	 * and {@link EntityInformation}.
	 *
	 * @param template {@link GemfireTemplate} used to perform basic data access operations and simple OQL queries;
	 * must not be {@literal null}.
	 * @param entityInformation {@link EntityInformation} used to describe the entity; must not be {@literal null}.
	 * @throws IllegalArgumentException if {@link GemfireTemplate} or {@link EntityInformation} is {@literal null}.
	 * @see org.springframework.data.gemfire.GemfireTemplate
	 * @see org.springframework.data.repository.core.EntityInformation
	 */
	public SimpleGemfireRepository(GemfireTemplate template, EntityInformation<T, ID> entityInformation) {

		Assert.notNull(template, "GemfireTemplate must not be null");
		Assert.notNull(entityInformation, "EntityInformation must not be null");

		this.template = template;
		this.entityInformation = entityInformation;
	}

	@Override
	public <U extends T> U save(U entity) {

		ID id = this.entityInformation.getRequiredId(entity);

		this.template.put(id, entity);

		return entity;
	}

	@Override
	public T save(Wrapper<T, ID> wrapper) {

		T entity = wrapper.getEntity();

		this.template.put(wrapper.getKey(), entity);

		return entity;
	}

	@Override
	public <U extends T> Iterable<U> saveAll(Iterable<U> entities) {

		Map<ID, U> entitiesToSave = new HashMap<>();

		entities.forEach(entity -> entitiesToSave.put(this.entityInformation.getRequiredId(entity), entity));

		this.template.putAll(entitiesToSave);

		return entitiesToSave.values();
	}

	@Override
	public long count() {

		String countQuery = String.format("SELECT count(*) FROM %s", this.template.getRegion().getFullPath());

		SelectResults<Integer> results = this.template.find(countQuery);

		return Optional.ofNullable(results)
			.map(SelectResults::iterator)
			.filter(Iterator::hasNext)
			.map(Iterator::next)
			.map(Long::valueOf)
			.orElse(0L);
	}

	@Override
	public boolean existsById(ID id) {
		return findById(id).isPresent();
	}

	@Override
	public Optional<T> findById(ID id) {
		return Optional.ofNullable(this.template.get(id));
	}

	@Override
	public Collection<T> findAll() {

		SelectResults<T> results =
			this.template.find(String.format("SELECT * FROM %s", this.template.getRegion().getFullPath()));

		return results.asList();
	}

	@Override
	public Iterable<T> findAll(Sort sort) {

		QueryString query = QueryString.of("SELECT * FROM /RegionPlaceholder")
			.fromRegion(this.entityInformation.getJavaType(), this.template.getRegion())
			.orderBy(sort);

		SelectResults<T> selectResults = this.template.find(query.toString());

		return selectResults.asList();
	}

	@Override
	public Collection<T> findAllById(Iterable<ID> ids) {

		List<ID> keys = Streamable.of(ids).stream().collect(StreamUtils.toUnmodifiableList());

		return CollectionUtils.<ID, T>nullSafeMap(this.template.getAll(keys)).values().stream()
			.filter(Objects::nonNull).collect(Collectors.toList());
	}

	@Override
	public void deleteById(ID id) {
		this.template.remove(id);
	}

	@Override
	public void delete(T entity) {
		deleteById(this.entityInformation.getRequiredId(entity));
	}

	@Override
	public void deleteAll(Iterable<? extends T> entities) {
		entities.forEach(this::delete);
	}

	boolean isPartitioned(Region<?, ?> region) {

		return region != null && region.getAttributes() != null
			&& isPartitioned(region.getAttributes().getDataPolicy());
	}

	boolean isPartitioned(DataPolicy dataPolicy) {
		return dataPolicy != null && dataPolicy.withPartitioning();
	}

	boolean isTransactionPresent(Region<?, ?> region) {

		return region.getRegionService() instanceof Cache
			&& isTransactionPresent(((Cache) region.getRegionService()).getCacheTransactionManager());
	}

	boolean isTransactionPresent(CacheTransactionManager cacheTransactionManager) {
		return cacheTransactionManager != null && cacheTransactionManager.exists();
	}

	<K> void  doRegionClear(Region<K, ?> region) {
		region.removeAll(region.keySet());
	}

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
