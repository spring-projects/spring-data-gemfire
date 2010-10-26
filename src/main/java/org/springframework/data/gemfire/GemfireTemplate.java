/*
 * Copyright 2010 the original author or authors.
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

import org.springframework.dao.DataAccessException;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

import com.gemstone.gemfire.GemFireCheckedException;
import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.query.IndexInvalidException;
import com.gemstone.gemfire.cache.query.QueryInvalidException;
import com.gemstone.gemfire.cache.query.SelectResults;

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

	public <K,V> GemfireTemplate(Region<K, V> region) {
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

	public <K, V> V get(final K key) {
		return execute(new GemfireCallback<V>() {
			@SuppressWarnings("unchecked")
			public V doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return (V) region.get(key);
			}
		});
	}
	
	public <K, V> V put(final K key, final V value) {
		return execute(new GemfireCallback<V>() {
			@SuppressWarnings("unchecked")
			public V doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return (V) region.put(key, value);
			}
		});
	}

	public <K, V> V remove(final K key) {
		return execute(new GemfireCallback<V>() {
			@SuppressWarnings("unchecked")
			public V doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return (V) region.remove(key);
			}
		});
	}

	public <E> SelectResults<E> query(final String query) {
		return execute(new GemfireCallback<SelectResults<E>>() {
			@SuppressWarnings("unchecked")
			public SelectResults<E> doInGemfire(Region region) throws GemFireCheckedException, GemFireException {
				return region.query(query);
			}
		});
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