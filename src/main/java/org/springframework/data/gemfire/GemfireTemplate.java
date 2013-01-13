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
 * <p/>
 * The central method is <tt>execute</tt>, supporting GemFire access code implementing the GemfireCallback interface.
 * It provides dedicated handling such that neither the GemfireCallback implementation nor the calling code needs to
 * explicitly care about handling {@link Region} life-cycle exceptions.
 * Typically used to implement data access or business logic services that use GemFire within their implementation but
 * are GemFire-agnostic in their interface. The latter or code calling the latter only have to deal with business
 * objects, query objects, and <tt>org.springframework.dao</tt> exceptions. 
 * 
 * @author Costin Leau
 */
public class GemfireTemplate extends GemfireAccessor {

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
	 * Sets whether to expose the native Gemfire Region to GemfireCallback
	 * code. Default is "false": a Region proxy will be returned,
	 * suppressing <code>close</code> calls.
	 * <p>As there is often a need to cast to a interface, the exposed proxy
	 * implements all interfaces implemented by the original {@link Region}.
	 * If this is not sufficient, turn this flag to "true".
	 * @see GemfireCallback
	 */
	public void setExposeNativeRegion(boolean exposeNativeRegion) {
		this.exposeNativeRegion = exposeNativeRegion;
	}

	/**
	 * Returns whether to expose the native GemFire Region to GemfireCallback
	 * code, or rather a Region proxy.
	 */
	public boolean isExposeNativeRegion() {
		return this.exposeNativeRegion;
	}

	public boolean containsKey(final Object key) {
		return execute(new GemfireCallback<Boolean>() {
 			public Boolean doInGemfire(Region<?,?> region) throws GemFireCheckedException, GemFireException {
				return region.containsKey(key);
			}
		});
	}

	public boolean containsKeyOnServer(final Object key) {
		return execute(new GemfireCallback<Boolean>() {
			public Boolean doInGemfire(Region<?,?> region) throws GemFireCheckedException, GemFireException {
				return region.containsKeyOnServer(key);
			}
		});
	}

	public boolean containsValue(final Object value) {
		return execute(new GemfireCallback<Boolean>() {
			public Boolean doInGemfire(Region<?,?> region) throws GemFireCheckedException, GemFireException {
				return region.containsValue(value);
			}
		});
	}

	public boolean containsValueForKey(final Object key) {
		return execute(new GemfireCallback<Boolean>() {
			public Boolean doInGemfire(Region<?,?> region) throws GemFireCheckedException, GemFireException {
				return region.containsValueForKey(key);
			}
		});
	}

	public <K, V> void create(final K key, final V value) {
		execute(new GemfireCallback<Object>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public Object doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				region.create(key, value);
				return null;
			}
		});
	}

	public <K, V> V get(final K key) {
		return execute(new GemfireCallback<V>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public V doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return (V) region.get(key);
			}
		});
	}

	public <K, V> V put(final K key, final V value) {
		return execute(new GemfireCallback<V>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public V doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return (V) region.put(key, value);
			}
		});
	}

	public <K, V> V putIfAbsent(final K key, final V value) {
		return execute(new GemfireCallback<V>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public V doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return (V) region.putIfAbsent(key, value);
			}
		});
	}

	public <K, V> V remove(final K key) {
		return execute(new GemfireCallback<V>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public V doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return (V) region.remove(key);
			}
		});
	}

	public <K, V> V replace(final K key, final V value) {
		return execute(new GemfireCallback<V>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public V doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return (V) region.replace(key, value);
			}
		});
	}

	public <K, V> boolean replace(final K key, final V oldValue, final V newValue) {
		return execute(new GemfireCallback<Boolean>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public Boolean doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return region.replace(key, oldValue, newValue);
			}
		});
	}

	public <K, V> Map<K, V> getAll(final Collection<?> keys) {
		return execute(new GemfireCallback<Map<K, V>>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public Map<K, V> doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return (Map<K, V>) region.getAll(keys);
			}
		});
	}

	public <K, V> void putAll(final Map<? extends K, ? extends V> map) {
		execute(new GemfireCallback<Object>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public Object doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				region.putAll(map);
				return null;
			}
		});
	}

	/**
	 * Shortcut for {@link Region#query(String)} method. Filters the values of this region using the predicate given as a string with the syntax of the WHERE clause of the query language. 
	 * The predefined variable this may be used inside the predicate to denote the current element being filtered. 
	 * This method evaluates the passed in where clause and returns results. It is supported on servers as well as clients. 
	 * When executed on a client, this method always runs on the server and returns results. 
	 * When invoking this method from the client, applications can pass in a where clause or a complete query. 

	 * @see Region#query(String)
	 * @param query A query language boolean query predicate. 
	 * @return A SelectResults containing the values of this Region that match the predicate. 
	 */
	public <E> SelectResults<E> query(final String query) {
		return execute(new GemfireCallback<SelectResults<E>>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public SelectResults<E> doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return region.query(query);
			}
		});
	}

	/**
	 * Executes a GemFire query with the given (optional) parameters and returns the result. Note this method expects the query to return multiple results; for queries that return only one
	 * element use {@link #findUnique(String, Object...)}.
	 * <p/>
	 * As oppose, to the {@link #query(String)} method, this method allows for more generic queries (against multiple regions even) to be executed.
	 * 
	 * <p/>Note that the local query service is used if the region is configured as a client without any pool configuration or server connectivity - otherwise the query service on the default pool
	 * is being used.
	 * 
	 * @see QueryService#newQuery(String)
	 * @see Query#execute(Object[])
	 * @see SelectResults
	 * @param query GemFire query
	 * @param params Values that are bound to parameters (such as $1) in this query. 
	 * @return A {@link SelectResults} instance holding the objects matching the query
	 * @throws InvalidDataAccessApiUsageException in case the query returns a single result (not a {@link SelectResults}).
	 */
	public <E> SelectResults<E> find(final String query, final Object... params)
			throws InvalidDataAccessApiUsageException {
		return execute(new GemfireCallback<SelectResults<E>>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public SelectResults<E> doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				QueryService queryService = lookupQueryService(region);
				Query q = queryService.newQuery(query);
 				Object result = q.execute(params);
 				if (result instanceof SelectResults) {
					return (SelectResults<E>) result;
				}
				throw new InvalidDataAccessApiUsageException(
						"Result object returned from GemfireCallback isn't a SelectResult: [" + result + "]");
			}
		});
	}

	/**
	 * Executes a GemFire query with the given (optional) parameters and returns the result. Note this method expects the query to return a single result; for queries that return multiple
	 * elements use {@link #find(String, Object...)}.
	 * <p/>
	 * As oppose, to the {@link #query(String)} method, this method allows for more generic queries (against multiple regions even) to be executed.
	 * 
	 * <p/>Note that the local query service is used if the region is configured as a client without any pool configuration or server connectivity - otherwise the query service on the default pool
	 * is being used.
	 * 
	 * @see QueryService#newQuery(String)
	 * @see Query#execute(Object[])
	 * @param query GemFire query
	 * @param params Values that are bound to parameters (such as $1) in this query.
	 * @return The (single) object that represents the result of the query.
	 * @throws InvalidDataAccessApiUsageException in case the query returns multiple objects (through {@link SelectResults}). 
	 */
	public <T> T findUnique(final String query, final Object... params) throws InvalidDataAccessApiUsageException {
		return execute(new GemfireCallback<T>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public T doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				QueryService queryService = lookupQueryService(region);
				Query q = queryService.newQuery(query);
				Object result = q.execute(params);
				if (result instanceof SelectResults) {
					throw new InvalidDataAccessApiUsageException(
							"Result object returned from GemfireCallback isn't unique: [" + result + "]");
				}
				return (T) result;
			}
		});
	}


	/**
	 * Returns the query service used by the template in its find methods.
	 * 
	 * @param region region to find the local query service from
	 * @return query service to use, local or generic
	 */
	protected QueryService lookupQueryService(Region<?, ?> region) {
		if (region.getRegionService() instanceof ClientCache
				&& (region instanceof LocalRegion && !((LocalRegion) region).hasServerProxy())
				&& Scope.LOCAL.equals(region.getAttributes().getScope())) {
			return ((ClientCache) region.getRegionService()).getLocalQueryService();
		}
		return region.getRegionService().getQueryService();
	}


	public <T> T execute(GemfireCallback<T> action) throws DataAccessException {
		return execute(action, isExposeNativeRegion());
	}

	/**
	 * Execute the action specified by the given action object within a
	 * Region.
	 * @param action callback object that specifies the Gemfire action
	 * @param exposeNativeRegion whether to expose the native
	 * GemFire region to callback code
	 * @return a result object returned by the action, or <code>null</code>
	 * @throws org.springframework.dao.DataAccessException in case of GemFire errors
	 */
	public <T> T execute(GemfireCallback<T> action, boolean exposeNativeRegion) throws DataAccessException {
		Assert.notNull(action, "Callback object must not be null");
		try {
			Region<?, ?> regionToExpose = (exposeNativeRegion ? getRegion() : regionProxy);
			T result = action.doInGemfire(regionToExpose);
			return result;
		} catch (IndexInvalidException ex) {
			throw convertGemFireQueryException(ex);
		} catch (QueryInvalidException ex) {
			throw convertGemFireQueryException(ex);
		} catch (GemFireCheckedException ex) {
			throw convertGemFireAccessException(ex);
		} catch (GemFireException ex) {
			throw convertGemFireAccessException(ex);
		} catch (RuntimeException ex) {
			// try first the CqInvalidException (removed in 6.5)
			if (GemfireCacheUtils.isCqInvalidException(ex)) {
				throw GemfireCacheUtils.convertCqInvalidException(ex);
			}
			// callback code threw application exception
			throw ex;
		}
	}

	/**
	 * Create a close-suppressing proxy for the given GemFire {@link Region}.
	 * Called by the <code>execute</code> method.
	 * 
	 * @param region the GemFire Region to create a proxy for
	 * @return the Region proxy, implementing all interfaces
	 * implemented by the passed-in Region object 
	 * @see Region#close()
	 * @see #execute(GemfireCallback, boolean)
	 */
	@SuppressWarnings("unchecked")
	protected <K, V> Region<K, V> createRegionProxy(Region<K, V> region) {
		Class<?>[] ifcs = ClassUtils.getAllInterfacesForClass(region.getClass(), getClass().getClassLoader());
		return (Region<K, V>) Proxy.newProxyInstance(region.getClass().getClassLoader(), ifcs,
				new CloseSuppressingInvocationHandler(region));
	}

	//-------------------------------------------------------------------------
	// Convenience methods for load, save, delete
	//-------------------------------------------------------------------------

	/**
	 * Invocation handler that suppresses close calls on GemFire Regions.
	 * @see Region#close()
	 */
	private static class CloseSuppressingInvocationHandler implements InvocationHandler {

		private final Region<?, ?> target;

		public CloseSuppressingInvocationHandler(Region<?, ?> target) {
			this.target = target;
		}

		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			// Invocation on Region interface coming in...

			if (method.getName().equals("equals")) {
				// Only consider equal when proxies are identical.
				return (proxy == args[0]);
			}
			else if (method.getName().equals("hashCode")) {
				// Use hashCode of region proxy.
				return System.identityHashCode(proxy);
			}
			else if (method.getName().equals("close")) {
				// Handle close method: suppress, not valid.
				return null;
			}

			// Invoke method on target Region
			try {
				Object retVal = method.invoke(this.target, args);
				return retVal;
			} catch (InvocationTargetException ex) {
				throw ex.getTargetException();
			}
		}
	}
}