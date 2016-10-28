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

package org.springframework.data.gemfire.function.execution;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.isA;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.apache.geode.cache.execute.Execution;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionException;
import org.apache.geode.cache.execute.ResultCollector;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * The AbstractFunctionExecutionTest class is a test suite of test cases testing the contract and functionality
 * of the AbstractFunctionExecution class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.function.execution.AbstractFunctionExecution
 * @see org.apache.geode.cache.execute.Execution
 * @since 1.7.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractFunctionExecutionTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Mock
	private Execution mockExecution;

	// TODO add more tests!!!

	@Test
	@SuppressWarnings("unchecked")
	public void executeWithResults() throws Exception {
		Object[] args = { "one", "two", "three" };
		List<Object> results = Arrays.asList(args);

		Function mockFunction = mock(Function.class, "MockFunction");
		ResultCollector mockResultCollector = mock(ResultCollector.class, "MockResultCollector");

		when(mockExecution.withArgs(eq(args))).thenReturn(mockExecution);
		when(mockExecution.execute(eq(mockFunction))).thenReturn(mockResultCollector);
		when(mockFunction.hasResult()).thenReturn(true);
		when(mockResultCollector.getResult(500, TimeUnit.MILLISECONDS)).thenReturn(results);

		AbstractFunctionExecution functionExecution = new AbstractFunctionExecution() {
			@Override protected Execution getExecution() {
				return mockExecution;
			}
		};

		Iterable<Object> actualResults = functionExecution.setFunction(mockFunction)
			.setArgs(args).setTimeout(500).execute();

		assertThat(actualResults).isNotNull();
		assertThat(actualResults).isEqualTo((Iterable<Object>) results);

		verify(mockExecution, times(1)).withArgs(eq(args));
		verify(mockExecution, never()).withCollector(any(ResultCollector.class));
		verify(mockExecution, never()).withFilter(any(Set.class));
		verify(mockExecution, times(1)).execute(eq(mockFunction));
		verify(mockExecution, never()).execute(any(String.class));
		verify(mockResultCollector, times(1)).getResult(500, TimeUnit.MILLISECONDS);
		verify(mockResultCollector, never()).getResult();
	}

	@Test
	public void executeAndExtractWithSingleResult() {
		final List<String> results = Collections.singletonList("test");

		AbstractFunctionExecution functionExecution = new AbstractFunctionExecution() {
			@Override protected Execution getExecution() {
				return mockExecution;
			}

			@SuppressWarnings("unchecked")
			@Override <T> Iterable<T> execute() {
				return (Iterable<T>) results;
			}
		};

		assertThat(functionExecution.<String>executeAndExtract()).isEqualTo("test");
	}

	@Test
	public void executeAndExtractWithMultipleResults() {
		final List<String> results = Arrays.asList("one", "two", "three");

		AbstractFunctionExecution functionExecution = new AbstractFunctionExecution() {
			@Override protected Execution getExecution() {
				return mockExecution;
			}

			@SuppressWarnings("unchecked")
			@Override <T> Iterable<T> execute() {
				return (Iterable<T>) results;
			}
		};

		assertThat(functionExecution.<String>executeAndExtract()).isEqualTo("one");
	}

	@Test
	public void executeAndExtractWithNullResults() {
		AbstractFunctionExecution functionExecution = new AbstractFunctionExecution() {
			@Override protected Execution getExecution() {
				return mockExecution;
			}

			@SuppressWarnings("unchecked")
			@Override <T> Iterable<T> execute() {
				return null;
			}
		};

		assertThat((Object) functionExecution.executeAndExtract()).isNull();
	}

	@Test
	public void executeAndExtractWithNoResults() {
		AbstractFunctionExecution functionExecution = new AbstractFunctionExecution() {
			@Override protected Execution getExecution() {
				return mockExecution;
			}

			@SuppressWarnings("unchecked")
			@Override <T> Iterable<T> execute() {
				return Collections.emptyList();
			}
		};

		assertThat((Object) functionExecution.executeAndExtract()).isNull();
	}

	@Test
	public void executeAndExtractWithThrowsException() {
		AbstractFunctionExecution functionExecution = new AbstractFunctionExecution() {
			@Override protected Execution getExecution() {
				return mockExecution;
			}

			@SuppressWarnings("unchecked")
			@Override <T> Iterable<T> execute() {
				return Collections.singletonList((T) new IllegalArgumentException("test"));
			}
		};

		expectedException.expect(FunctionException.class);
		expectedException.expectCause(isA(IllegalArgumentException.class));
		expectedException.expectMessage(containsString("Execution of Function with ID 'TestFunction' failed"));

		functionExecution.setFunctionId("TestFunction").executeAndExtract();
	}
}
