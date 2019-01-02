/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.test.support;

import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.util.StringUtils;

/**
 * {@link AbstractUnitAndIntegrationTestsWithMockSupport} is an abstract base class for test classes
 * having support for the creation and use of mock objects in test cases, but that may also be ran
 * as integration tests using actual GemFire components (e.g. Regions, etc).
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @since 1.5.3
 */
@SuppressWarnings("unused")
public abstract class AbstractUnitAndIntegrationTestsWithMockSupport {

	protected static final String GEMFIRE_TEST_RUNNER_NO_MOCK =
		System.getProperty(GemfireTestApplicationContextInitializer.GEMFIRE_TEST_RUNNER_DISABLED, "false");

	protected static final String NOT_IMPLEMENTED = "Not Implemented";

	/* (non-Javadoc) */
	protected boolean isMocking() {
		String gemfireTestRunnerNoMock = StringUtils.trimWhitespace(GEMFIRE_TEST_RUNNER_NO_MOCK);

		return !(Boolean.parseBoolean(gemfireTestRunnerNoMock)
			|| "yes".equalsIgnoreCase(gemfireTestRunnerNoMock)
			|| "y".equalsIgnoreCase(gemfireTestRunnerNoMock));
	}

	/* (non-Javadoc) */
	protected boolean isNotMocking() {
		return !isMocking();
	}

	/* (non-Javadoc) */
	protected Object getMockId() {
		StackTraceElement element = StackTraceUtils.getTestCaller();
		return (element != null ? StackTraceUtils.getCallerSimpleName(element) : IdentifierSequence.nextId());
	}
}
