package org.springframework.data.gemfire.repository.support;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.springframework.data.gemfire.GemfireCallback;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.Wrapper;
import org.springframework.data.repository.core.EntityInformation;
import org.springframework.util.Assert;

import com.gemstone.gemfire.GemFireCheckedException;
import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Region;

/**
 * Basic repository implementation.
 * 
 * @author Oliver Gierke
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
	 * @see org.springframework.data.repository.Repository#save(java.lang.Object)
	 */
	public T save(T entity) {
		template.put(entityInformation.getId(entity), entity);
		return entity;
	}

	/* 
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.Repository#save(java.lang.Iterable)
	 */
	public Iterable<T> save(Iterable<? extends T> entities) {
		List<T> result = new ArrayList<T>();
		for (T entity : entities) {
			result.add(save(entity));
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.Repository#findOne(java.io.Serializable)
	 */
	@SuppressWarnings("unchecked")
	public T findOne(ID id) {
		Object object = template.get(id);
		return (T) object;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.Repository#exists(java.io.Serializable)
	 */
	public boolean exists(ID id) {
		return findOne(id) != null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.Repository#findAll()
	 */
	public Collection<T> findAll() {
		return template.execute(new GemfireCallback<Collection<T>>() {
			@SuppressWarnings({ "rawtypes", "unchecked" })
			public Collection<T> doInGemfire(Region region) {
				return region.values();
			}
		});
	}

	/* 
	 * (non-Javadoc)
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
	 * @see org.springframework.data.repository.Repository#count()
	 */
	public long count() {
		return template.execute(new GemfireCallback<Long>() {
			@SuppressWarnings("rawtypes")
			public Long doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return Long.valueOf(region.keySet().size());
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.Repository#delete(java.lang.Object)
	 */
	public void delete(T entity) {
		template.remove(entityInformation.getId(entity));
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.Repository#delete(java.lang.Iterable)
	 */
	public void delete(Iterable<? extends T> entities) {
		for (T entity : entities) {
			delete(entity);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.Repository#deleteAll()
	 */
	public void deleteAll() {

		template.execute(new GemfireCallback<Void>() {
			@SuppressWarnings("rawtypes")
			public Void doInGemfire(Region region) {
				region.clear();
				return null;
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.CrudRepository#delete(java.io.Serializable)
	 */
	public void delete(ID id) {
		template.remove(id);
	}

	/* 
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.repository.GemfireRepository#save(org.springframework.data.gemfire.repository.Wrapper)
	 */
	@Override
	public T save(Wrapper<T, ID> wrapper) {
		return template.put(wrapper.getKey(), wrapper.getEntity());
	}
}
