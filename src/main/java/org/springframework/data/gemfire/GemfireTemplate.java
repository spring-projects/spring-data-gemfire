/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Collection;
import java.util.Map;

import org.springframework.dao.DataAccessException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

import com.gemstone.gemfire.GemFireCheckedException;
import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.query.IndexInvalidException;
import com.gemstone.gemfire.cache.query.Query;
import com.gemstone.gemfire.cache.query.QueryInvalidException;
import com.gemstone.gemfire.cache.query.QueryService;
import com.gemstone.gemfire.cache.query.SelectResults;
import com.gemstone.gemfire.internal.cache.LocalRegion;

/**
 * Helper class that simplifies GemFire data access code and converts {@link GemFireCheckedException} and
 * {@link GemFireException} into Spring {@link DataAccessException}, following the <tt>org.springframework.dao</tt>
 * exception hierarchy.
 *
 * The central method is <tt>execute</tt>, supporting GemFire access code implementing the GemfireCallback interface.
 * It provides dedicated handling such that neither the GemfireCallback implementation nor the calling code needs to
 * explicitly care about handling {@link Region} life-cycle exceptions.
 * Typically used to implement data access or business logic services that use GemFire within their implementation but
 * are GemFire-agnostic in their interface. The latter or code calling the latter only have to deal with business
 * objects, query objects, and <tt>org.springframework.dao</tt> exceptions. 
 *
 * @author Costin Leau
 * @author John Blum
 * @see java.util.Map
 * @see org.springframework.data.gemfire.GemfireAccessor
 * @see org.springframework.data.gemfire.GemfireOperations
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.query.Query
 * @see com.gemstone.gemfire.cache.query.QueryService
 * @see com.gemstone.gemfire.cache.query.SelectResults
 */
@SuppressWarnings("unused")
public class GemfireTemplate extends GemfireAccessor implements GemfireOperations {

	private boolean exposeNativeRegion = false;

	private Region<?, ?> regionProxy;

	public GemfireTemplate() {
	}

	public <K, V> GemfireTemplate(Region<K, V> region) {
		setRegion(region);
		afterPropertiesSet();
	}

	@Override
	public void afterPropertiesSet() {
		super.afterPropertiesSet();
		regionProxy = createRegionProxy(getRegion());
	}

	/**
	 * Sets whether to expose the native Gemfire Region to GemfireCallback code. Default is "false": a Region proxy
	 * will be returned, suppressing <code>close</code> calls.
	 * <p>As there is often a need to cast to a interface, the exposed proxy implements all interfaces
	 * implemented by the original {@link Region}. If this is not sufficient, turn this flag to "true".
	 *
	 * @param exposeNativeRegion a boolean value to indicate whether the native GemFire Cache Region should be exposed
	 * to the GemfireCallback.
	 * @see org.springframework.data.gemfire.GemfireCallback
	 */
	public void setExposeNativeRegion(boolean exposeNativeRegion) {
		this.exposeNativeRegion = exposeNativeRegion;
	}

	/**
	 * Returns whether to expose the native GemFire Cache Region or a Region proxy to the GemfireCallback code.
	 *
	 * @return a boolean value indicating whether the native GemFire Cache Region or Region proxy is exposed
	 * to the GemfireCallback code.
	 */
	public boolean isExposeNativeRegion() {
		return this.exposeNativeRegion;
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#containsKey(java.lang.Object)
	 */
	@Override
	public boolean containsKey(final Object key) {
		return getRegion().containsKey(key);
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#containsKeyOnServer(java.lang.Object)
	 */
	@Override
	public boolean containsKeyOnServer(final Object key) {
		return getRegion().containsKeyOnServer(key);
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#containsValue(java.lang.Object)
	 */
	@Override
	public boolean containsValue(final Object value) {
		return getRegion().containsValue(value);
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#containsValueForKey(java.lang.Object)
	 */
	@Override
	public boolean containsValueForKey(final Object key) {
		return getRegion().containsValueForKey(key);
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#create(K, V)
	 */
	@Override
	public <K, V> void create(final K key, final V value) {
		try {
			getRegion().create(key, value);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#get(K)
	 */
	@Override
	public <K, V> V get(final K key) {
		try {
			return this.<K, V>getRegion().get(key);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#getAll(java.util.Collection)
	 */
	@Override
	public <K, V> Map<K, V> getAll(final Collection<?> keys) {
		try {
			return this.<K, V>getRegion().getAll(keys);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#put(K, V)
	 */
	@Override
	public <K, V> V put(final K key, final V value) {
		try {
			return this.<K, V>getRegion().put(key, value);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#putAll(java.util.Map)
	 */
	@Override
	public <K, V> void putAll(final Map<? extends K, ? extends V> map) {
		try {
			this.<K, V>getRegion().putAll(map);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#putIfAbsent(K, V)
	 */
	@Override
	public <K, V> V putIfAbsent(final K key, final V value) {
		try {
			return this.<K, V>getRegion().putIfAbsent(key, value);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#remove(K)
	 */
	@Override
	public <K, V> V remove(final K key) {
		try {
			return this.<K, V>getRegion().remove(key);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#replace(K, V)
	 */
	@Override
	public <K, V> V replace(final K key, final V value) {
		try {
			return this.<K, V>getRegion().replace(key, value);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#replace(K, V, V)
	 */
	@Override
	public <K, V> boolean replace(final K key, final V oldValue, final V newValue) {
		try {
			return this.<K, V>getRegion().replace(key, oldValue, newValue);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#query(java.lang.String)
	 */
	@Override
	public <E> SelectResults<E> query(final String query) {
		try {
			return this.getRegion().query(query);
		}
		catch (IndexInvalidException ex) {
			throw convertGemFireQueryException(ex);
		}
		catch (QueryInvalidException ex) {
			throw convertGemFireQueryException(ex);
		}
		catch (GemFireCheckedException e) {
			throw convertGemFireAccessException(e);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
		catch (RuntimeException ex) {
			// test for CqInvalidException (removed in 6.5)
			if (GemfireCacheUtils.isCqInvalidException(ex)) {
				throw GemfireCacheUtils.convertCqInvalidException(ex);
			}

			// callback code threw application exception
			throw ex;
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#find(java.lang.String, java.lang.Object)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <E> SelectResults<E> find(final String queryString, final Object... params) throws InvalidDataAccessApiUsageException {
		try {
			QueryService queryService = lookupQueryService(getRegion());
			Query query = queryService.newQuery(queryString);
			Object result = query.execute(params);

			if (result instanceof SelectResults) {
				return (SelectResults<E>) result;
			}
			else {
				throw new InvalidDataAccessApiUsageException(
					"Result object returned from GemfireCallback isn't a SelectResult: [" + result + "]");
			}
		}
		catch (IndexInvalidException ex) {
			throw convertGemFireQueryException(ex);
		}
		catch (QueryInvalidException ex) {
			throw convertGemFireQueryException(ex);
		}
		catch (GemFireCheckedException e) {
			throw convertGemFireAccessException(e);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
		catch (RuntimeException ex) {
			// test for CqInvalidException (removed in 6.5)
			if (GemfireCacheUtils.isCqInvalidException(ex)) {
				throw GemfireCacheUtils.convertCqInvalidException(ex);
			}

			// callback code threw application exception
			throw ex;
		}
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#findUnique(java.lang.String, java.lang.Object)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <T> T findUnique(final String queryString, final Object... params) throws InvalidDataAccessApiUsageException {
		try {
			QueryService queryService = lookupQueryService(getRegion());
			Query query = queryService.newQuery(queryString);
			Object result = query.execute(params);

			if (result instanceof SelectResults) {
				SelectResults<T> selectResults = (SelectResults<T>) result;

				if (selectResults.asList().size() == 1) {
					result = selectResults.iterator().next();
				}
				else {
					throw new InvalidDataAccessApiUsageException(String.format(
						"The result returned from query (%1$s) is not unique: (%2$s).", queryString, result));
				}
			}

			return (T) result;
		}
		catch (IndexInvalidException ex) {
			throw convertGemFireQueryException(ex);
		}
		catch (QueryInvalidException ex) {
			throw convertGemFireQueryException(ex);
		}
		catch (GemFireCheckedException e) {
			throw convertGemFireAccessException(e);
		}
		catch (GemFireException e) {
			throw convertGemFireAccessException(e);
		}
		catch (RuntimeException ex) {
			// test for CqInvalidException (removed in 6.5)
			if (GemfireCacheUtils.isCqInvalidException(ex)) {
				throw GemfireCacheUtils.convertCqInvalidException(ex);
			}

			// callback code threw application exception
			throw ex;
		}
	}

	/**
	 * Returns the query service used by the template in its find methods.
	 * 
	 * @param region region to find the local query service from
	 * @return query service to use, local or generic
	 * @see com.gemstone.gemfire.cache.Region
	 * @see com.gemstone.gemfire.cache.Region#getRegionService()
	 * @see com.gemstone.gemfire.cache.RegionService#getQueryService()
	 * @see com.gemstone.gemfire.cache.client.ClientCache#getLocalQueryService()
	 */
	protected QueryService lookupQueryService(final Region<?, ?> region) {
		return (requiresLocalQueryService(region) ? ((ClientCache) region.getRegionService()).getLocalQueryService()
			: region.getRegionService().getQueryService());
	}

	/*
	 * (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Region
	 * @see com.gemstone.gemfire.internal.cache.LocalRegion
	 */
	/* package-private */ boolean isLocalWithNoServerProxy(final Region<?, ?> region) {
		return (region instanceof LocalRegion && !((LocalRegion) region).hasServerProxy());
	}

	/*
	 * (non-Javadoc)
	 * @see #isLocalWithNoServerProxy(:Region)
	 * @see com.gemstone.gemfire.cache.Region
	 * @see com.gemstone.gemfire.cache.RegionAttributes
	 * @see com.gemstone.gemfire.cache.RegionAttributes#getScope()
	 * @see com.gemstone.gemfire.cache.Scope
	 * @see com.gemstone.gemfire.cache.client.ClientCache
	 * @see com.gemstone.gemfire.internal.cache.LocalRegion
	 * @see com.gemstone.gemfire.internal.cache.LocalRegion#hasServerProxy()
	 */
	private boolean requiresLocalQueryService(final Region<?, ?> region) {
		return (region.getRegionService() instanceof ClientCache && isLocalWithNoServerProxy(region)
			&& Scope.LOCAL.equals(region.getAttributes().getScope()));
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#execute(org.springframework.data.gemfire.GemfireCallback)
	 */
	@Override
	public <T> T execute(GemfireCallback<T> action) throws DataAccessException {
		return execute(action, isExposeNativeRegion());
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.GemfireOperations#execute(org.springframework.data.gemfire.GemfireCallback, boolean)
	 */
	@Override
	public <T> T execute(GemfireCallback<T> action, boolean exposeNativeRegion) throws DataAccessException {
		Assert.notNull(action, "Callback object must not be null");
		try {
			Region<?, ?> regionToExpose = (exposeNativeRegion ? getRegion() : regionProxy);
			return action.doInGemfire(regionToExpose);
		}
		catch (IndexInvalidException ex) {
			throw convertGemFireQueryException(ex);
		}
		catch (QueryInvalidException ex) {
			throw convertGemFireQueryException(ex);
		}
		catch (GemFireCheckedException ex) {
			throw convertGemFireAccessException(ex);
		}
		catch (GemFireException ex) {
			throw convertGemFireAccessException(ex);
		}
		catch (RuntimeException ex) {
			// test for CqInvalidException (removed in 6.5)
			if (GemfireCacheUtils.isCqInvalidException(ex)) {
				throw GemfireCacheUtils.convertCqInvalidException(ex);
			}

			// callback code threw application exception
			throw ex;
		}
	}

	/**
	 * Create a close-suppressing proxy for the given GemFire Cache {@link Region}.
	 * Called by the <code>execute</code> method.
	 *
	 * @param <K> the Region key class type.
	 * @param <V> the Region value class type.
	 * @param region the GemFire Cache Region to create a proxy for.
	 * @return the Region proxy implementing all interfaces implemented by the passed-in Region object.
	 * @see com.gemstone.gemfire.cache.Region#close()
	 * @see #execute(GemfireCallback, boolean)
	 */
	@SuppressWarnings("unchecked")
	protected <K, V> Region<K, V> createRegionProxy(final Region<K, V> region) {
		return (Region<K, V>) Proxy.newProxyInstance(region.getClass().getClassLoader(),
			ClassUtils.getAllInterfacesForClass(region.getClass(), getClass().getClassLoader()),
				new RegionCloseSuppressingInvocationHandler(region));
	}

	/**
	 * InvocationHandler that suppresses close calls on GemFire Cache Regions.
	 *
	 * @see com.gemstone.gemfire.cache.Region#close()
	 * @see java.lang.reflect.InvocationHandler
	 */
	private static class RegionCloseSuppressingInvocationHandler implements InvocationHandler {

		private final Region<?, ?> target;

		public RegionCloseSuppressingInvocationHandler(final Region<?, ?> target) {
			Assert.notNull(target, "The Region to target must not be null.");
			this.target = target;
		}

		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if (method.getName().equals("equals")) {
				// only consider equal when proxies are identical
				return (proxy == args[0]);
			}
			else if (method.getName().equals("hashCode")) {
				// use hashCode of Region proxy
				return System.identityHashCode(proxy);
			}
			else if (method.getName().equals("close")) {
				// suppress Region.close() method call
				return null;
			}
			else {
				try {
					return method.invoke(this.target, args);
				}
				catch (InvocationTargetException ex) {
					throw ex.getTargetException();
				}
			}
		}
	}

}
