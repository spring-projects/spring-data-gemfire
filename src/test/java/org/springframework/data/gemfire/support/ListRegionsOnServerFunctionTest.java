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

package org.springframework.data.gemfire.support;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.execute.FunctionContext;
import com.gemstone.gemfire.cache.execute.ResultSender;

/**
 * The ListRegionsOnServerFunctionTest class is a test suite of test cases testing the contract and functionality
 * of the ListRegionsOnServerFunction GemFire Function class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.support.ListRegionsOnServerFunction
 * @since 1.7.0
 */
public class ListRegionsOnServerFunctionTest {

	private ListRegionsOnServerFunction function = new ListRegionsOnServerFunction();

	@Test
	@SuppressWarnings("unchecked")
	public void executeReturnsRootRegionNames() {
		final Cache mockCache = mock(Cache.class, "MockGemFireCache");

		Region mockRegionOne = mock(Region.class, "MockGemFireRegionOne");
		Region mockRegionTwo = mock(Region.class, "MockGemFireRegionTwo");
		Region mockRegionThree = mock(Region.class, "MockGemFireRegionThree");

		FunctionContext mockFunctionContext = mock(FunctionContext.class, "MockGemFireFunctionContext");

		ResultSender<Object> mockResultSender = mock(ResultSender.class, "MockGemFireResultSender");

		when(mockCache.rootRegions()).thenReturn(new HashSet<Region<?, ?>>(
			Arrays.<Region<?, ?>>asList(mockRegionOne, mockRegionTwo, mockRegionThree)));
		when(mockRegionOne.getName()).thenReturn("One");
		when(mockRegionTwo.getName()).thenReturn("Two");
		when(mockRegionThree.getName()).thenReturn("Three");
		when(mockFunctionContext.getResultSender()).thenReturn(mockResultSender);

		final AtomicReference<List<String>> regionNames = new AtomicReference<List<String>>(null);

		doAnswer(new Answer<Void>() {
			@Override
			public Void answer(final InvocationOnMock invocation) throws Throwable {
				regionNames.compareAndSet(null, invocation.getArgumentAt(0, List.class));
				return null;
			}
		}).when(mockResultSender).lastResult(any(List.class));

		ListRegionsOnServerFunction function = new ListRegionsOnServerFunction() {
			@Override Cache getCache() {
				return mockCache;
			}
		};

		function.execute(mockFunctionContext);

		List<String> actualRegionNames = regionNames.get();

		assertThat(actualRegionNames, is(not(nullValue())));
		assertThat(actualRegionNames.isEmpty(), is(false));
		assertThat(actualRegionNames.size(), is(equalTo(3)));
		assertThat(actualRegionNames.containsAll(Arrays.asList("One", "Two", "Three")), is(true));

		verify(mockCache, times(1)).rootRegions();
		verify(mockRegionOne, times(1)).getName();
		verify(mockRegionTwo, times(1)).getName();
		verify(mockRegionThree, times(1)).getName();
		verify(mockFunctionContext, times(1)).getResultSender();
		verify(mockResultSender, times(1)).lastResult(any(List.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void executeWithNoRegions() {
		final Cache mockCache = mock(Cache.class, "MockGemFireCache");

		FunctionContext mockFunctionContext = mock(FunctionContext.class, "MockGemFireFunctionContext");

		ResultSender<Object> mockResultSender = mock(ResultSender.class, "MockGemFireResultSender");

		when(mockCache.rootRegions()).thenReturn(Collections.<Region<?, ?>>emptySet());
		when(mockFunctionContext.getResultSender()).thenReturn(mockResultSender);

		final AtomicReference<List<String>> regionNames = new AtomicReference<List<String>>(null);

		doAnswer(new Answer<Void>() {
			@Override
			public Void answer(final InvocationOnMock invocation) throws Throwable {
				regionNames.compareAndSet(null, invocation.getArgumentAt(0, List.class));
				return null;
			}
		}).when(mockResultSender).lastResult(any(List.class));

		ListRegionsOnServerFunction function = new ListRegionsOnServerFunction() {
			@Override Cache getCache() {
				return mockCache;
			}
		};

		function.execute(mockFunctionContext);

		List<String> actualRegionNames = regionNames.get();

		assertThat(actualRegionNames, is(not(nullValue())));
		assertThat(actualRegionNames.isEmpty(), is(true));

		verify(mockCache, times(1)).rootRegions();
		verify(mockFunctionContext, times(1)).getResultSender();
		verify(mockResultSender, times(1)).lastResult(any(List.class));
	}

	@Test
	public void getIdIsFullyQualifiedClassName() {
		assertThat(function.getId(), is(equalTo(ListRegionsOnServerFunction.class.getName())));
	}

	@Test
	public void hasResultIsTrue() {
		assertThat(function.hasResult(), is(true));
	}

	@Test
	public void isHighAvailabilityAndOptimizeForWriteAreFalse() {
		assertThat(function.isHA(), is(false));
		assertThat(function.optimizeForWrite(), is(false));
	}

}
