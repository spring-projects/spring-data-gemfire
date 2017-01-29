/*
 * Copyright 2002-2018 the original author or authors.
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

package org.springframework.data.gemfire.util;

import java.lang.reflect.Array;
import java.util.Arrays;

import org.springframework.util.ObjectUtils;

/**
 * The ArrayUtils class is a utility class for working with Object arrays.
 *
 * @author David Turanski
 * @author John Blum
 * @see java.lang.reflect.Array
 * @see java.util.Arrays
 */
public abstract class ArrayUtils {

	/**
	 * Returns the given varargs {@code element} as an array.
	 *
	 * @param <T> Class type of the elements.
	 * @param elements variable list of arguments to return as an array.
	 * @return an arry for the given varargs {@code elements}.
	 */
	@SafeVarargs
	public static <T> T[] asArray(T... elements) {
		return elements;
	}

	/**
	 * Returns the given {@code array} if not {@literal null} or empty, otherwise returns the {@code defaultArray}.
	 *
	 * @param <T> {@link Class} type of the array elements.
	 * @param array array to evaluate.
	 * @param defaultArray array to return if the given {@code array} is {@literal null} or empty.
	 * @return the given {@code array} if not {@literal null} or empty otherwise return the {@code defaultArray}.
	 */
	public static <T> T[] defaultIfEmpty(T[] array, T[] defaultArray) {
		return !ObjectUtils.isEmpty(array) ? array : defaultArray;
	}

	/**
	 * Null-safe method to return the first element in the array or {@literal null}
	 * if the array is {@literal null} or empty.
	 *
	 * @param <T> Class type of the array elements.
	 * @param array the array from which to extract the first element.
	 * @return the first element in the array or {@literal null} if the array is null or empty.
	 * @see #getFirst(Object[], Object)
	 */
	public static <T> T getFirst(T[] array) {
		return getFirst(array, null);
	}

	/**
	 * Null-safe method to return the first element in the array or the {@code defaultValue}
	 * if the array is {@literal null} or empty.
	 *
	 * @param <T> Class type of the array elements.
	 * @param array the array from which to extract the first element.
	 * @param defaultValue value to return if the array is {@literal null} or empty.
	 * @return the first element in the array or {@code defaultValue} if the array is {@literal null} or empty.
	 * @see #getFirst(Object[], Object)
	 */
	public static <T> T getFirst(T[] array, T defaultValue) {
		return isEmpty(array) ? defaultValue : array[0];
	}

	/**
	 * Insert an element into the given array at position (index).  The element is inserted at the given position
	 * and all elements afterwards are moved to the right.
	 *
	 * @param originalArray the array in which to insert the element.
	 * @param position an integer index (position) at which to insert the element in the array.
	 * @param element the element to insert into the array.
	 * @return a new array with the element inserted at position.
	 * @see java.lang.System#arraycopy(Object, int, Object, int, int)
	 * @see java.lang.reflect.Array#newInstance(Class, int)
	 */
	public static Object[] insert(Object[] originalArray, int position, Object element) {

		Object[] newArray =
			(Object[]) Array.newInstance(originalArray.getClass().getComponentType(), originalArray.length + 1);


		// copy all elements before the given position (here, position refers to the length, or number of elements
		// to be copied, excluding the element at originalArray[position]
		if (position > 0) {
			System.arraycopy(originalArray, 0, newArray, 0, position);
		}

		// insert
		newArray[position] = element;

		// copy remaining elements from originalArray, starting at position, to new array
		if (position < originalArray.length) {
			System.arraycopy(originalArray, position, newArray, position + 1,
				originalArray.length - position);
		}

		return newArray;
	}

	/**
	 * Determines whether the given array is empty or not.
	 *
	 * @param array the array to evaluate for emptiness.
	 * @return a boolean value indicating whether the given array is empty.
	 * @see #length(Object...)
	 */
	public static boolean isEmpty(Object[] array) {
		return length(array) == 0;
	}

	/**
	 * Null-safe operation to determine an array's length.
	 *
	 * @param array the array to determine it's length.
	 * @return the length of the given array or 0 if the array reference is null.
	 */
	public static int length(Object[] array) {
		return array != null ? array.length : 0;
	}

	/**
	 * Null-safe, empty array operation returning the given object array if not null or an empty object array
	 * if the array argument is null.
	 *
	 * @param <T> Class type of the array elements.
	 * @param array array of objects on which a null check is performed.
	 * @param componentType Class type of the array elements.
	 * @return the given object array if not null, otherwise return an empty object array.
	 * @see java.lang.reflect.Array#newInstance(Class, int)
	 */
	@SuppressWarnings("unchecked")
	public static <T> T[] nullSafeArray(T[] array, Class<T> componentType) {
		return array != null ? array : (T[]) Array.newInstance(componentType, 0);
	}

	/**
	 * Remove an element from the given array at position (index).  The element is removed at the specified position
	 * and all remaining elements are shifted to the left.
	 *
	 * @param originalArray the array from which to remove the element.
	 * @param position the integer index (position) indicating the element to remove from the array.
	 * @return a new array with the element at position in the originalArray removed.
	 * @see java.lang.System#arraycopy(Object, int, Object, int, int)
	 * @see java.lang.reflect.Array#newInstance(Class, int)
	 */
	public static Object[] remove(Object[] originalArray, int position) {

		Object[] newArray =
			(Object[]) Array.newInstance(originalArray.getClass().getComponentType(), originalArray.length - 1);

		// copy all elements before position (here, position refers to the length, or number of elements to be copied
		if (position > 0) {
			System.arraycopy(originalArray, 0, newArray, 0, position);
		}

		// copy remaining elements after position from the originalArray
		if (position < originalArray.length - 1) {
			System.arraycopy(originalArray, position + 1, newArray, position,
				originalArray.length - 1 - position);
		}

		return newArray;
	}

	/**
	 * Sort the array of elements according to the elements natural ordering.
	 *
	 * @param <T> {@link Comparable} class type of the array elements.
	 * @param array array of elements to sort.
	 * @return the sorted array of elements.
	 * @see java.util.Arrays#sort(Object[])
	 */
	public static <T extends Comparable<T>> T[] sort(T[] array) {

		Arrays.sort(array);

		return array;
	}
}
