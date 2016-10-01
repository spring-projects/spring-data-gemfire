/*
 * Copyright 2012 the original author or authors.
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

package org.springframework.data.gemfire.test.support;

import java.util.Arrays;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.mockito.internal.matchers.VarargMatcher;
import org.springframework.data.gemfire.util.ArrayUtils;

/**
 * MockitoMatchers is a utility class encapsulating customer Hamcrest {@link Matcher Matchers} used by Mockito
 * in the Spring Data GemFire project test suite.
 *
 * @author John Blum
 * @see org.hamcrest.Matcher
 * @since 1.9.0
 */
public abstract class MockitoMatchers {

	/* (non-Javadoc) */
	public static Matcher<String> stringArrayMatcher(String... expected) {
		return new ArrayMatcher<String>(expected);
	}

	/* (non-Javadoc) */
	protected static final class ArrayMatcher<T> extends BaseMatcher<T> implements VarargMatcher {

		private final T[] expected;

		protected ArrayMatcher(T... expected) {
			this.expected = expected;
		}

		@Override
		public boolean matches(Object item) {
			Object[] actual = (item instanceof Object[] ? (Object[]) item : ArrayUtils.asArray(item));

			return Arrays.equals(actual, expected);
		}

		@Override
		public void describeTo(Description description) {
			description.appendText(String.format("expected [%1$s]", this.expected));
		}
	}
}
