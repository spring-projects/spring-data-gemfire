/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire;

import java.util.Collection;
import java.util.Map;

import org.springframework.dao.DataAccessException;
import org.springframework.dao.InvalidDataAccessApiUsageException;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.query.Query;
import com.gemstone.gemfire.cache.query.QueryService;
import com.gemstone.gemfire.cache.query.SelectResults;

/**
 * @author David Turanski
 *
 */
public interface GemfireOperations {

	public abstract boolean containsKey(Object key);

	public abstract boolean containsKeyOnServer(Object key);

	public abstract boolean containsValue(Object value);

	public abstract boolean containsValueForKey(Object key);

	public abstract <K, V> void create(K key, V value);

	public abstract <K, V> V get(K key);

	public abstract <K, V> V put(K key, V value);

	public abstract <K, V> V putIfAbsent(K key, V value);

	public abstract <K, V> V remove(K key);

	public abstract <K, V> V replace(K key, V value);

	public abstract <K, V> boolean replace(K key, V oldValue, V newValue);

	public abstract <K, V> Map<K, V> getAll(Collection<?> keys);

	public abstract <K, V> void putAll(Map<? extends K, ? extends V> map);

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
	public abstract <E> SelectResults<E> query(String query);

	/**
	 * Executes a GemFire query with the given (optional) parameters and returns the result. Note this method expects the query to return multiple results; for queries that return only one
	 * element use {@link #findUnique(String, Object...)}.
	 *
	 * As oppose, to the {@link #query(String)} method, this method allows for more generic queries (against multiple regions even) to be executed.
	 * 
	 * Note that the local query service is used if the region is configured as a client without any pool configuration or server connectivity - otherwise the query service on the default pool
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
	public abstract <E> SelectResults<E> find(String query, Object... params) throws InvalidDataAccessApiUsageException;

	/**
	 * Executes a GemFire query with the given (optional) parameters and returns the result. Note this method expects the query to return a single result; for queries that return multiple
	 * elements use {@link #find(String, Object...)}.
	 *
	 * As oppose, to the {@link #query(String)} method, this method allows for more generic queries (against multiple regions even) to be executed.
	 * 
	 * Note that the local query service is used if the region is configured as a client without any pool configuration or server connectivity - otherwise the query service on the default pool
	 * is being used.
	 * 
	 * @see QueryService#newQuery(String)
	 * @see Query#execute(Object[])
	 * @param query GemFire query
	 * @param params Values that are bound to parameters (such as $1) in this query.
	 * @return The (single) object that represents the result of the query.
	 * @throws InvalidDataAccessApiUsageException in case the query returns multiple objects (through {@link SelectResults}). 
	 */
	public abstract <T> T findUnique(String query, Object... params) throws InvalidDataAccessApiUsageException;

	public abstract <T> T execute(GemfireCallback<T> action) throws DataAccessException;

	/**
	 * Execute the action specified by the given action object within a
	 * Region.
	 * @param action callback object that specifies the Gemfire action
	 * @param exposeNativeRegion whether to expose the native
	 * GemFire region to callback code
	 * @return a result object returned by the action, or <code>null</code>
	 * @throws org.springframework.dao.DataAccessException in case of GemFire errors
	 */
	public abstract <T> T execute(GemfireCallback<T> action, boolean exposeNativeRegion) throws DataAccessException;

}
