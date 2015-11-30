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

package org.springframework.data.gemfire.test;

import org.springframework.data.gemfire.test.support.IdentifierSequence;
import org.springframework.data.gemfire.test.support.StackTraceUtils;
import org.springframework.util.StringUtils;

/**
 * The AbstractMockery class is an abstract base class supporting the creation and use of mock objects in unit tests.
 *
 * @author John Blum
 * @since 1.5.3
 */
@SuppressWarnings("unused")
public abstract class AbstractMockerySupport {

	protected static final String NOT_IMPLEMENTED = "Not Implemented";

	protected boolean isMocking() {
		String gemfireTestRunnerNoMock = StringUtils.trimWhitespace(System.getProperty(
			GemfireTestApplicationContextInitializer.GEMFIRE_TEST_RUNNER_DISABLED, "false"));

		return !(Boolean.parseBoolean(gemfireTestRunnerNoMock)
			|| "yes".equalsIgnoreCase(gemfireTestRunnerNoMock)
			|| "y".equalsIgnoreCase(gemfireTestRunnerNoMock));
	}

	protected boolean isNotMocking() {
		return !isMocking();
	}

	protected Object getMockId() {
		StackTraceElement element = StackTraceUtils.getTestCaller();
		return (element != null ? StackTraceUtils.getCallerSimpleName(element): IdentifierSequence.nextId());
	}

}
