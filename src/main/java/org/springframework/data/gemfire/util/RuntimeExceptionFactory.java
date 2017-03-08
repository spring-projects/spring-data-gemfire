/*
 * Copyright 2016 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.util;

import java.text.MessageFormat;

/**
 * The {@link RuntimeExceptionFactory} class is a factory for creating common {@link RuntimeException RuntimeExceptions}
 * with the added convenience of message formatting and optional {@link Throwable causes}.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class RuntimeExceptionFactory {

	public static final String NOT_IMPLEMENTED = "Not Implemented";

	/**
	 * Constructs and initializes an {@link IllegalArgumentException} with the given {@link String message}
	 * and {@link Object[] arguments} used to format the message.
	 *
	 * @param message {@link String} describing the {@link IllegalArgumentException exception}.
	 * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
	 * @return a new {@link IllegalArgumentException} with the given {@link String message}.
	 * @see #newIllegalArgumentException(Throwable, String, Object...)
	 * @see java.lang.IllegalArgumentException
	 */
	public static IllegalArgumentException newIllegalArgumentException(String message, Object... args) {
		return newIllegalArgumentException(null, message, args);
	}

	/**
	 * Constructs and initializes an {@link IllegalArgumentException} with the given {@link Throwable cause},
	 * {@link String message} and {@link Object[] arguments} used to format the message.
	 *
	 * @param cause {@link Throwable} identifying the reason the {@link IllegalArgumentException} was thrown.
	 * @param message {@link String} describing the {@link IllegalArgumentException exception}.
	 * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
	 * @return a new {@link IllegalArgumentException} with the given {@link Throwable cause} and {@link String message}.
	 * @see java.lang.IllegalArgumentException
	 */
	public static IllegalArgumentException newIllegalArgumentException(Throwable cause,
		String message, Object... args) {

		return new IllegalArgumentException(format(message, args), cause);
	}

	/**
	 * Constructs and initializes an {@link IllegalStateException} with the given {@link String message}
	 * and {@link Object[] arguments} used to format the message.
	 *
	 * @param message {@link String} describing the {@link IllegalStateException exception}.
	 * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
	 * @return a new {@link IllegalStateException} with the given {@link String message}.
	 * @see #newIllegalStateException(Throwable, String, Object...)
	 * @see java.lang.IllegalStateException
	 */
	public static IllegalStateException newIllegalStateException(String message, Object... args) {
		return newIllegalStateException(null, message, args);
	}

	/**
	 * Constructs and initializes an {@link IllegalStateException} with the given {@link Throwable cause},
	 * {@link String message} and {@link Object[] arguments} used to format the message.
	 *
	 * @param cause {@link Throwable} identifying the reason the {@link IllegalStateException} was thrown.
	 * @param message {@link String} describing the {@link IllegalStateException exception}.
	 * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
	 * @return a new {@link IllegalStateException} with the given {@link Throwable cause} and {@link String message}.
	 * @see java.lang.IllegalStateException
	 */
	public static IllegalStateException newIllegalStateException(Throwable cause, String message, Object... args) {
		return new IllegalStateException(format(message, args), cause);
	}

	/**
	 * Constructs and initializes an {@link RuntimeException} with the given {@link String message}
	 * and {@link Object[] arguments} used to format the message.
	 *
	 * @param message {@link String} describing the {@link RuntimeException exception}.
	 * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
	 * @return a new {@link RuntimeException} with the given {@link String message}.
	 * @see #newRuntimeException(Throwable, String, Object...)
	 * @see java.lang.RuntimeException
	 */
	public static RuntimeException newRuntimeException(String message, Object... args) {
		return newRuntimeException(null, message, args);
	}

	/**
	 * Constructs and initializes an {@link RuntimeException} with the given {@link Throwable cause},
	 * {@link String message} and {@link Object[] arguments} used to format the message.
	 *
	 * @param cause {@link Throwable} identifying the reason the {@link RuntimeException} was thrown.
	 * @param message {@link String} describing the {@link RuntimeException exception}.
	 * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
	 * @return a new {@link RuntimeException} with the given {@link Throwable cause} and {@link String message}.
	 * @see java.lang.RuntimeException
	 */
	public static RuntimeException newRuntimeException(Throwable cause, String message, Object... args) {
		return new RuntimeException(format(message, args), cause);
	}

	/**
	 * Constructs and initializes an {@link UnsupportedOperationException} with the given {@link String message}
	 * and {@link Object[] arguments} used to format the message.
	 *
	 * @param message {@link String} describing the {@link UnsupportedOperationException exception}.
	 * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}
	 * @return a new {@link UnsupportedOperationException} with the given {@link String message}.
	 * @see #newUnsupportedOperationException(Throwable, String, Object...)
	 * @see java.lang.UnsupportedOperationException
	 */
	public static UnsupportedOperationException newUnsupportedOperationException(String message, Object... args) {
		return newUnsupportedOperationException(null, message, args);
	}

	/**
	 * Constructs and initializes an {@link UnsupportedOperationException} with the given {@link Throwable cause},
	 * {@link String message} and {@link Object[] arguments} used to format the message.
	 *
	 * @param cause {@link Throwable} identifying the reason the {@link UnsupportedOperationException} was thrown.
	 * @param message {@link String} describing the {@link UnsupportedOperationException exception}.
	 * @param args {@link Object[] arguments} used to replace format placeholders in the {@link String message}.
	 * @return a new {@link UnsupportedOperationException} with the given {@link Throwable cause}
	 * and {@link String message}.
	 * @see java.lang.UnsupportedOperationException
	 */
	public static UnsupportedOperationException newUnsupportedOperationException(Throwable cause,
			String message, Object... args) {

		return new UnsupportedOperationException(format(message, args), cause);
	}

	/**
	 * Formats the given {@link String message} using the given {@link Object[] arguments}.
	 *
	 * @param message {@link String} containing the message pattern to format.
	 * @param args {@link Object[] arguments} used in the message to replace format placeholders.
	 * @return the formatted {@link String message}.
	 * @see java.lang.String#format(String, Object...)
	 * @see java.text.MessageFormat#format(String, Object...)
	 */
	protected static String format(String message, Object... args) {
		return MessageFormat.format(String.format(message, args), args);
	}
}
