/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import java.lang.reflect.Field;

/**
 * Utility class containing common functionality used when writing tests.
 *
 * @author Costin Leau
 * @author John Blum
 */
public abstract class TestUtils {

	@SuppressWarnings("unchecked")
	public static <T> T readField(String name, Object target) throws Exception {
		Field field = findField(name, target);

		if (field == null) {
			throw new IllegalArgumentException(String.format("Cannot find field [%1$s] in class [%2$s]",
				name, target.getClass().getName()));
		}

		field.setAccessible(true);

		return (T) field.get(target);
	}

	/* (non-Javadoc) */
	private static Field findField(String fieldName, Object target) {
		return findField(fieldName, target.getClass());
	}

	/* (non-Javadoc) */
	private static Field findField(String fieldName, Class<?> type) {
		Field field = null;

		while (field == null && !type.equals(Object.class)) {
			try {
				field = type.getDeclaredField(fieldName);
			}
			catch (Throwable ignore) {
			}
			finally {
				type = type.getSuperclass();
			}
		}

		return field;
	}
}
