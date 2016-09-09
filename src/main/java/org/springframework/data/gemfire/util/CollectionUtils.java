/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.util;

import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * The CollectionUtils class is a utility class for working with Java Collections Framework and classes.
 *
 * @author John Blum
 * @see java.util.Collection
 * @see java.util.Collections
 * @see java.util.List
 * @see java.util.Map
 * @see java.util.Set
 * @see org.springframework.util.CollectionUtils
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public abstract class CollectionUtils extends org.springframework.util.CollectionUtils {

	/**
	 * Returns an unmodifiable {@link Set} containing the elements from the given object array.
	 *
	 * @param <T> Class type of the elements.
	 * @param elements array of objects to add to the {@link Set}.
	 * @return an unmodifiable {@link Set} containing the elements from the given object array.
	 */
	public static <T> Set<T> asSet(T... elements) {
		Set<T> set = new HashSet<T>(elements.length);
		Collections.addAll(set, elements);
		return Collections.unmodifiableSet(set);
	}

	/**
	 * Adapts the given Enumeration as an Iterable object for use within a for each loop.
	 *
	 * @param <T> the class type of the Enumeration elements.
	 * @param enumeration the Enumeration to adapt as an Iterable object.
	 * @return an Iterable instance of the Enumeration.
	 * @see java.lang.Iterable
	 * @see java.util.Enumeration
	 */
	public static <T> Iterable<T> iterable(final Enumeration<T> enumeration) {
		return new Iterable<T>() {
			@Override public Iterator<T> iterator() {
				return org.springframework.util.CollectionUtils.toIterator(enumeration);
			}
		};
	}

	/**
	 * Adapts the given Iterator as an Iterable object for use within a for each loop.
	 *
	 * @param <T> the class type of the Iterator elements.
	 * @param iterator the Iterator to adapt as an Iterable object.
	 * @return an Iterable instance of the Iterator.
	 * @see java.lang.Iterable
	 * @see java.util.Iterator
	 */
	public static <T> Iterable<T> iterable(final Iterator<T> iterator) {
		return new Iterable<T>() {
			@Override public Iterator<T> iterator() {
				return iterator;
			}
		};
	}

	/**
	 * Null-safe operation returning the given {@link Collection} if not {@literal null}
	 * or an empty {@link Collection} (implemented with {@link List}) if {@literal null}.
	 *
	 * @param <T> Class type of the {@link Collection} elements.
	 * @param collection {@link Collection} to evaluate.
	 * @return the given {@link Collection} if not null or return an empty {@link Collection}
	 * (implemented with {@link List}).
	 * @see java.util.Collections#emptyList()
	 */
	public static <T> Collection<T> nullSafeCollection(Collection<T> collection) {
		return (collection != null ? collection : Collections.<T>emptyList());
	}

	/**
	 * A null-safe operation returning the original Iterable object if non-null or a default, empty Iterable
	 * implementation if null.
	 *
	 * @param <T> the class type of the iterable elements.
	 * @param iterable the Iterable object evaluated for a null reference.
	 * @return the Iterable object if not null or a default, empty Iterable implementation otherwise.
	 * @see java.lang.Iterable
	 * @see java.util.Iterator
	 */
	public static <T> Iterable<T> nullSafeIterable(Iterable<T> iterable) {
		return (iterable != null ? iterable : new Iterable<T>() {
			@Override public Iterator<T> iterator() {
				return new Iterator<T>() {
					@Override public boolean hasNext() {
						return false;
					}

					@Override public T next() {
						throw new NoSuchElementException("No more elements");
					}

					@Override  public void remove() {
						throw new UnsupportedOperationException("Operation not supported");
					}
				};
			}
		});
	}

	/**
	 * Null-safe operation returning the given {@link List} if not {@literal null}
	 * or an empty {@link List} if {@literal null}.
	 *
	 * @param <T> Class type of the {@link List} elements.
	 * @param list {@link List} to evaluate.
	 * @return the given {@link List} if not null or an empty {@link List}.
	 * @see java.util.Collections#emptyList()
	 */
	public static <T> List<T> nullSafeList(List<T> list) {
		return (list != null ? list : Collections.<T>emptyList());
	}

	/**
	 * Null-safe operation returning the given {@link Map} if not {@literal null}
	 * or an empty {@link Map} if {@literal null}.
	 *
	 * @param <K> Class type of the {@link Map Map's} keys.
	 * @param <V> Class type of the {@link Map Map's} values.
	 * @param map {@link Map} to evaluate.
	 * @return the given {@link Map} if not null or an empty {@link Map}.
	 * @see java.util.Collections#emptyMap()
	 */
	public static <K, V> Map<K, V> nullSafeMap(Map<K, V> map) {
		return (map != null ? map : Collections.<K, V>emptyMap());
	}

	/**
	 * Null-safe operation returning the given {@link Set} if not {@literal null}
	 * or an empty {@link Set} if {@literal null}.
	 *
	 * @param <T> Class type of the {@link Set} elements.
	 * @param set {@link Set} to evaluate.
	 * @return the given {@link Set} if not null or an empty {@link Set}.
	 * @see java.util.Collections#emptySet()
	 */
	public static <T> Set<T> nullSafeSet(Set<T> set) {
		return (set != null ? set : Collections.<T>emptySet());
	}
}
