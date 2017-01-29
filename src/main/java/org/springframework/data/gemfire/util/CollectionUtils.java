/*
 * Copyright 2010-2018 the original author or authors.
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

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;

import org.springframework.util.Assert;

/**
 * The CollectionUtils class is a utility class for working with Java Collections Framework and classes.
 *
 * @author John Blum
 * @see java.util.Collection
 * @see java.util.Collections
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.List
 * @see java.util.Map
 * @see java.util.Set
 * @see org.springframework.util.CollectionUtils
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public abstract class CollectionUtils extends org.springframework.util.CollectionUtils {

	/**
	 * Adds all elements from the given {@link Iterable} to the {@link Collection}.
	 *
	 * @param <E> {@link Class} type of the elements in the {@link Collection} and {@link Iterable}.
	 * @param <T> concrete {@link Class} type of the {@link Collection}.
	 * @param collection {@link Collection} in which to add the elements from the {@link Iterable}.
	 * @param iterable {@link Iterable} containing the elements to add to the {@link Collection}.
	 * @return the given {@link Collection}.
	 * @throws IllegalArgumentException if {@link Collection} is {@literal null}.
	 * @see java.lang.Iterable
	 * @see java.util.Collection
	 */
	public static <E, T extends Collection<E>> T addAll(T collection, Iterable<E> iterable) {

		Assert.notNull(collection, "Collection is required");

		stream(nullSafeIterable(iterable).spliterator(), false).forEach(collection::add);

		return collection;
	}

	/**
	 * Returns an unmodifiable {@link Set} containing the elements from the given object array.
	 *
	 * @param <T> Class type of the elements.
	 * @param elements array of objects to add to the {@link Set}.
	 * @return an unmodifiable {@link Set} containing the elements from the given object array.
	 */
	@SafeVarargs
	public static <T> Set<T> asSet(T... elements) {

		Set<T> set = new HashSet<>(elements.length);

		Collections.addAll(set, elements);

		return Collections.unmodifiableSet(set);
	}

	/**
	 * Null-safe method to determines whether the given {@link Collection} contains any elements from the given array.
	 *
	 * @param collection {@link Collection} to evaluate
	 * @param elements array of elements to evaluate.
	 * @return a boolean value indicating whether the collection contains at least 1 element from the given array.
	 * @see java.util.Collection#contains(Object)
	 */
	public static boolean containsAny(Collection<?> collection, Object... elements) {

		if (collection != null) {
			for (Object element : nullSafeArray(elements, Object.class)) {
				if (collection.contains(element)) {
					return true;
				}
			}
		}

		return false;
	}

	/**
	 * Returns an empty {@link Iterable} object.
	 *
	 * @param <T> {@link Class} type of the elements in the {@link Iterable}.
	 * @return an empty {@link Iterable}.
	 * @see java.lang.Iterable
	 * @see #nullSafeIterable(Iterable)
	 */
	public static <T> Iterable<T> emptyIterable() {
		return nullSafeIterable(null);
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
	public static <T> Iterable<T> iterable(Enumeration<T> enumeration) {
		return () -> toIterator(enumeration);
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
	public static <T> Iterable<T> iterable(Iterator<T> iterator) {
		return () -> iterator;
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
	 * @see java.util.Collection
	 */
	public static <T> Collection<T> nullSafeCollection(Collection<T> collection) {
		return collection != null ? collection : Collections.emptyList();
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
		return iterable != null ? iterable : Collections::emptyIterator;
	}

	/**
	 * Returns the given {@link Iterable} if not {@literal null} or empty, otherwise returns the {@code defaultIterable}.
	 *
	 * @param <T> concrete {@link Class} type of the {@link Iterable}.
	 * @param <E> {@link Class} type of the elements in the {@link Iterable Iterables}.
	 * @param iterable {@link Iterable} to evaluate.
	 * @param defaultIterable {@link Iterable} to return if the given {@code iterable} is {@literal null} or empty.
	 * @return {@code iterable} if not {@literal null} or empty otherwise return {@code defaultIterable}.
	 * @see java.lang.Iterable
	 */
	public static <E, T extends Iterable<E>> T nullSafeIterable(T iterable, T defaultIterable) {
		return Optional.ofNullable(iterable).filter(it -> it.iterator().hasNext()).orElse(defaultIterable);
	}

	/**
	 * Null-safe operation returning the given {@link List} if not {@literal null}
	 * or an empty {@link List} if {@literal null}.
	 *
	 * @param <T> Class type of the {@link List} elements.
	 * @param list {@link List} to evaluate.
	 * @return the given {@link List} if not null or an empty {@link List}.
	 * @see java.util.Collections#emptyList()
	 * @see java.util.List
	 */
	public static <T> List<T> nullSafeList(List<T> list) {
		return list != null ? list : Collections.emptyList();
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
	 * @see java.util.Map
	 */
	@SuppressWarnings("all")
	public static <K, V> Map<K, V> nullSafeMap(Map<K, V> map) {
		return map != null ? map : Collections.<K, V>emptyMap();
	}

	/**
	 * Null-safe operation returning the given {@link Set} if not {@literal null}
	 * or an empty {@link Set} if {@literal null}.
	 *
	 * @param <T> Class type of the {@link Set} elements.
	 * @param set {@link Set} to evaluate.
	 * @return the given {@link Set} if not null or an empty {@link Set}.
	 * @see java.util.Collections#emptySet()
	 * @see java.util.Set
	 */
	public static <T> Set<T> nullSafeSet(Set<T> set) {
		return set != null ? set : Collections.emptySet();
	}

	/**
	 * Sors the elements of the given {@link List} by their natural, {@link Comparable} ordering.
	 *
	 * @param <T> {@link Comparable} class type of the collection elements.
	 * @param list {@link List} of elements to sort.
	 * @return the {@link List} sorted.
	 * @see java.util.Collections#sort(List)
	 * @see java.util.List
	 */
	public static <T extends Comparable<T>> List<T> sort(List<T> list) {

		Collections.sort(list);

		return list;
	}

	/**
	 * Returns a sub-list of elements from the given {@link List} based on the provided {@code indices}.
	 *
	 * @param <T> Class type of the elements in the list.
	 * @param source {@link List} from which the elements of the sub-list is constructed.
	 * @param indices array of indexes in the {@code source} {@link List} to the elements
	 * used to construct the sub-list.
	 * @return a sub-list of elements from the given {@link List} based on the provided {@code indices}.
	 * @throws IndexOutOfBoundsException if the array of indexes contains an index that is not within
	 * the bounds of the list.
	 * @throws NullPointerException if either the list or indexes are null.
	 * @see java.util.List
	 */
	public static <T> List<T> subList(List<T> source, int... indices) {

		Assert.notNull(source, "List is required");

		List<T> result = new ArrayList<>(indices.length);

		for (int index : indices) {
			result.add(source.get(index));
		}

		return result;
	}

	/* (non-Javadoc) */
	public static String toString(Map<?, ?> map) {

		StringBuilder builder = new StringBuilder("{\n");

		int count = 0;

		for (Map.Entry<?, ?> entry : new TreeMap<>(map).entrySet()) {
			builder.append(++count > 1 ? ",\n" : "");
			builder.append("\t");
			builder.append(entry.getKey());
			builder.append(" = ");
			builder.append(entry.getValue());
		}

		builder.append("\n}");

		return builder.toString();
	}
}
