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

package org.springframework.data.gemfire.function.execution;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.ResultCollector;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * The AbstractFunctionTemplateTest class is a test suite of test cases testing the contract and functionality
 * of the AbstractFunctionTemplate class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.function.execution.AbstractFunctionExecution
 * @see org.springframework.data.gemfire.function.execution.AbstractFunctionTemplate
 * @see org.apache.geode.cache.execute.Function
 * @see org.apache.geode.cache.execute.ResultCollector
 * @since 1.7.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractFunctionTemplateTest {

	@Mock
	private AbstractFunctionExecution mockFunctionExecution;

	@Mock
	Function mockFunction;

	@Mock
	private ResultCollector mockResultCollector;

	@Test
	public void executeWithFunctionAndArgs() {
		Object[] args = { "test", "testing", "tested" };
		List<Object> results = Arrays.asList(args);

		when(mockFunctionExecution.setArgs(args)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setFunction(mockFunction)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setResultCollector(mockResultCollector)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setTimeout(500)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.execute()).thenReturn(results);

		AbstractFunctionTemplate functionTemplate = new AbstractFunctionTemplate() {
			@Override protected AbstractFunctionExecution getFunctionExecution() {
				return mockFunctionExecution;
			}
		};

		functionTemplate.setResultCollector(mockResultCollector);
		functionTemplate.setTimeout(500);

		Iterable<Object> actualResults = functionTemplate.execute(mockFunction, args);

		assertThat(functionTemplate.getResultCollector(), is(equalTo(mockResultCollector)));
		assertThat(actualResults, is(notNullValue()));
		assertThat(actualResults, is(equalTo((Iterable<Object>) results)));

		verify(mockFunctionExecution, times(1)).setArgs(args);
		verify(mockFunctionExecution, times(1)).setFunction(mockFunction);
		verify(mockFunctionExecution, times(1)).setResultCollector(eq(mockResultCollector));
		verify(mockFunctionExecution, times(1)).setTimeout(500);
		verify(mockFunctionExecution, times(1)).execute();
	}

	@Test
	public void executeAndExtractWithFunctionAndArgs() {
		Object[] args = { "test", "testing", "tested" };

		when(mockFunctionExecution.setArgs(args)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setFunction(mockFunction)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setResultCollector(mockResultCollector)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setTimeout(500)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.executeAndExtract()).thenReturn(args[0]);

		AbstractFunctionTemplate functionTemplate = new AbstractFunctionTemplate() {
			@Override protected AbstractFunctionExecution getFunctionExecution() {
				return mockFunctionExecution;
			}
		};

		functionTemplate.setResultCollector(mockResultCollector);
		functionTemplate.setTimeout(500);

		String result = functionTemplate.executeAndExtract(mockFunction, args);

		assertThat(functionTemplate.getResultCollector(), is(equalTo(mockResultCollector)));
		assertThat(result, is(notNullValue()));
		assertThat(result, is(equalTo("test")));

		verify(mockFunctionExecution, times(1)).setArgs(args);
		verify(mockFunctionExecution, times(1)).setFunction(mockFunction);
		verify(mockFunctionExecution, times(1)).setResultCollector(eq(mockResultCollector));
		verify(mockFunctionExecution, times(1)).setTimeout(500);
		verify(mockFunctionExecution, times(1)).executeAndExtract();
	}

	@Test
	public void executeWithFunctionIdAndArgs() {
		Object[] args = { "test", "testing", "tested" };
		List<Object> results = Arrays.asList(args);

		when(mockFunctionExecution.setArgs(args)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setFunctionId("TestFunction")).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setResultCollector(mockResultCollector)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setTimeout(500)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.execute()).thenReturn(results);

		AbstractFunctionTemplate functionTemplate = new AbstractFunctionTemplate() {
			@Override protected AbstractFunctionExecution getFunctionExecution() {
				return mockFunctionExecution;
			}
		};

		functionTemplate.setResultCollector(mockResultCollector);
		functionTemplate.setTimeout(500);

		Iterable<Object> actualResults = functionTemplate.execute("TestFunction", args);

		assertThat(functionTemplate.getResultCollector(), is(equalTo(mockResultCollector)));
		assertThat(actualResults, is(notNullValue()));
		assertThat(actualResults, is(equalTo((Iterable<Object>) results)));

		verify(mockFunctionExecution, times(1)).setArgs(args);
		verify(mockFunctionExecution, times(1)).setFunctionId("TestFunction");
		verify(mockFunctionExecution, times(1)).setResultCollector(eq(mockResultCollector));
		verify(mockFunctionExecution, times(1)).setTimeout(500);
		verify(mockFunctionExecution, times(1)).execute();
	}

	@Test
	public void executeAndExtractWithFunctionIdAndArgs() {
		Object[] args = { "test", "testing", "tested" };

		when(mockFunctionExecution.setArgs(args)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setFunctionId("TestFunction")).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setResultCollector(mockResultCollector)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setTimeout(500)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.executeAndExtract()).thenReturn(args[0]);

		AbstractFunctionTemplate functionTemplate = new AbstractFunctionTemplate() {
			@Override protected AbstractFunctionExecution getFunctionExecution() {
				return mockFunctionExecution;
			}
		};

		functionTemplate.setResultCollector(mockResultCollector);
		functionTemplate.setTimeout(500);

		String result = functionTemplate.executeAndExtract("TestFunction", args);

		assertThat(functionTemplate.getResultCollector(), is(equalTo(mockResultCollector)));
		assertThat(result, is(notNullValue()));
		assertThat(result, is(equalTo("test")));

		verify(mockFunctionExecution, times(1)).setArgs(args);
		verify(mockFunctionExecution, times(1)).setFunctionId("TestFunction");
		verify(mockFunctionExecution, times(1)).setResultCollector(eq(mockResultCollector));
		verify(mockFunctionExecution, times(1)).setTimeout(500);
		verify(mockFunctionExecution, times(1)).executeAndExtract();
	}

	@Test
	public void executeWithNoResultWithFunctionIdAndArgs() {
		Object[] args = { "test", "testing", "tested" };

		when(mockFunctionExecution.setArgs(args)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setFunctionId("TestFunction")).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setResultCollector(mockResultCollector)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.setTimeout(500)).thenReturn(mockFunctionExecution);
		when(mockFunctionExecution.execute(eq(false))).thenReturn(null);

		AbstractFunctionTemplate functionTemplate = new AbstractFunctionTemplate() {
			@Override protected AbstractFunctionExecution getFunctionExecution() {
				return mockFunctionExecution;
			}
		};

		functionTemplate.setResultCollector(mockResultCollector);
		functionTemplate.setTimeout(500);

		functionTemplate.executeWithNoResult("TestFunction", args);

		assertThat(functionTemplate.getResultCollector(), is(equalTo(mockResultCollector)));

		verify(mockFunctionExecution, times(1)).setArgs(args);
		verify(mockFunctionExecution, times(1)).setFunctionId("TestFunction");
		verify(mockFunctionExecution, times(1)).setResultCollector(eq(mockResultCollector));
		verify(mockFunctionExecution, times(1)).setTimeout(500);
		verify(mockFunctionExecution, times(1)).execute(eq(false));
	}

}
