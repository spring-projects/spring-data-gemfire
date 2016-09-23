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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.GemfireCallback;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.Wrapper;
import org.springframework.data.gemfire.repository.query.QueryString;
import org.springframework.data.repository.core.EntityInformation;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheTransactionManager;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.query.SelectResults;

/**
 * Basic Repository implementation for GemFire.
 *
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 * @see java.io.Serializable
 * @see org.springframework.data.gemfire.GemfireTemplate
 * @see org.springframework.data.gemfire.repository.GemfireRepository
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.Region
 */
public class SimpleGemfireRepository<T, ID extends Serializable> implements GemfireRepository<T, ID> {

	private final GemfireTemplate template;
	private final EntityInformation<T, ID> entityInformation;

	/**
	 * Creates a new {@link SimpleGemfireRepository}.
	 *
	 * @param template must not be {@literal null}.
	 * @param entityInformation must not be {@literal null}.
	 */
	public SimpleGemfireRepository(GemfireTemplate template, EntityInformation<T, ID> entityInformation) {

		Assert.notNull(template);
		Assert.notNull(entityInformation);

		this.template = template;
		this.entityInformation = entityInformation;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.CrudRepository#save(S)
	 */
	@Override
	public <U extends T> U save(U entity) {
		template.put(entityInformation.getId(entity), entity);
		return entity;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.CrudRepository#save(java.lang.Iterable)
	 */
	@Override
	public <U extends T> Iterable<U> save(Iterable<U> entities) {
		Map<ID, U> result = new HashMap<ID, U>();

		for (U entity : entities) {
			result.put(entityInformation.getId(entity), entity);
		}

		template.putAll(result);

		return result.values();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.springframework.data.gemfire.repository.GemfireRepository#save(
	 *   org.springframework.data.gemfire.repository.Wrapper)
	 */
	@Override
	public T save(Wrapper<T, ID> wrapper) {
		return template.put(wrapper.getKey(), wrapper.getEntity());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.CrudRepository#count()
	 */
	@Override
	public long count() {
		SelectResults<Integer> results = template.find("SELECT count(*) FROM " + template.getRegion().getFullPath());
		return (long) results.iterator().next();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.CrudRepository#exists(java.io.Serializable)
	 */
	@Override
	public boolean exists(ID id) {
		return (findOne(id) != null);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.CrudRepository#findOne(java.io.Serializable)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public T findOne(ID id) {
		return (T) template.get(id);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.CrudRepository#findAll()
	 */
	@Override
	public Collection<T> findAll() {
		SelectResults<T> results = template.find("SELECT * FROM " + template.getRegion().getFullPath());
		return results.asList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.repository.GemfireRepository.sort(:org.springframework.data.domain.Sort)
	 */
	@Override
	public Iterable<T> findAll(Sort sort) {
		QueryString query = new QueryString("SELECT * FROM /RegionPlaceholder")
			.forRegion(entityInformation.getJavaType(), template.getRegion())
			.orderBy(sort);

		SelectResults<T> selectResults = template.find(query.toString());

		return selectResults.asList();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.CrudRepository#findAll(java.lang.Iterable)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Collection<T> findAll(Iterable<ID> ids) {
		List<ID> parameters = new ArrayList<ID>();

		for (ID id : ids) {
			parameters.add(id);
		}

		return (Collection<T>) template.getAll(parameters).values();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.CrudRepository#delete(java.io.Serializable)
	 */
	@Override
	public void delete(ID id) {
		template.remove(id);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.springframework.data.repository.CrudRepository#delete(java.lang.Object)
	 */
	@Override
	public void delete(T entity) {
		delete(entityInformation.getId(entity));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.springframework.data.repository.CrudRepository#delete(java.lang.Iterable)
	 */
	@Override
	public void delete(Iterable<? extends T> entities) {
		for (T entity : entities) {
			delete(entity);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.gemstone.gemfire.cache.Region#getAttributes()
	 * @see com.gemstone.gemfire.cache.RegionAttributes#getDataPolicy()
	 */
	boolean isPartitioned(final Region region) {
		return (region != null && region.getAttributes() != null
			&& isPartitioned(region.getAttributes().getDataPolicy()));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.gemstone.gemfire.cache.DataPolicy#withPartitioning()
	 */
	boolean isPartitioned(final DataPolicy dataPolicy) {
		return (dataPolicy != null && dataPolicy.withPartitioning());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.gemstone.gemfire.cache.Region#getRegionService()
	 * @see com.gemstone.gemfire.cache.Cache#getCacheTransactionManager()
	 */
	boolean isTransactionPresent(final Region region) {
		return (region.getRegionService() instanceof Cache
			&& isTransactionPresent(((Cache) region.getRegionService()).getCacheTransactionManager()));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#exists()
	 */
	boolean isTransactionPresent(final CacheTransactionManager cacheTransactionManager) {
		return (cacheTransactionManager != null && cacheTransactionManager.exists());
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	void doRegionClear(final Region region) {
		region.removeAll(region.keySet());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.CrudRepository#deleteAll()
	 */
	@Override
	public void deleteAll() {
		template.execute(new GemfireCallback<Void>() {
			@Override
			@SuppressWarnings("rawtypes")
			public Void doInGemfire(final Region region) {
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
			}
		});
	}

}
