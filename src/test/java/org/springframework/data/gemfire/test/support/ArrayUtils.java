/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.test.support;

/**
 * ArrayUtils is a utility class for working with Java arrays.
 *
 * @author John Blum
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class ArrayUtils {

	public static <T> T getFirst(T... array) {
		return getFirst(null, array);
	}

	public static <T> T getFirst(T defaultValue, T... array) {
		return (isEmpty(array) ? defaultValue : array[0]);
	}

	public static boolean isEmpty(final Object... array) {
		return (array == null || array.length == 0);
	}

	public static int length(final Object... array) {
		return (array != null ? array.length : 0);
	}

}
