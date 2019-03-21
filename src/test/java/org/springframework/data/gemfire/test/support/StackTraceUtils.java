/*
 * Copyright 2010-2019 the original author or authors.
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
 * The StackTraceUtils class is a utility class for working with stack trace frames (elements) of the current Thread.
 *
 * @author John Blum
 * @see java.lang.StackTraceElement
 * @see java.lang.Thread
 * @see org.springframework.data.gemfire.test.support.ThreadUtils
 * @since 1.5.3
 */
@SuppressWarnings("unused")
public abstract class StackTraceUtils extends ThreadUtils {

	public static StackTraceElement getCaller() {
		return getCaller(Thread.currentThread());
	}

	public static StackTraceElement getCaller(final Thread thread) {
		return thread.getStackTrace()[2];
	}

	public static String getCallerName(final StackTraceElement element) {
		return String.format("%1$%s.%2$s", element.getClass().getName(), element.getMethodName());
	}

	public static String getCallerSimpleName(final StackTraceElement element) {
		return String.format("%1$%s.%2$s", element.getClass().getSimpleName(), element.getMethodName());
	}

	public static StackTraceElement getTestCaller() {
		return getTestCaller(Thread.currentThread());
	}

	public static StackTraceElement getTestCaller(final Thread thread) {
		for (StackTraceElement stackTraceElement : thread.getStackTrace()) {
			if (isTestSuiteClass(stackTraceElement) && isTestCaseMethod(stackTraceElement)) {
				return stackTraceElement;
			}
		}

		return null;
	}

	private static boolean isTestCaseMethod(final StackTraceElement element) {
		boolean result = element.getMethodName().toLowerCase().startsWith("test");

		try {
			result |= element.getClass().getMethod(element.getMethodName()).isAnnotationPresent(org.junit.Test.class);
		}
		catch (NoSuchMethodException ignore) {
		}

		return result;
	}

	private static boolean isTestSuiteClass(final StackTraceElement element) {
		boolean result = element.getClass().getSimpleName().toLowerCase().endsWith("test");
		result |= element.getClass().isAssignableFrom(junit.framework.TestCase.class);
		return result;
	}

}
