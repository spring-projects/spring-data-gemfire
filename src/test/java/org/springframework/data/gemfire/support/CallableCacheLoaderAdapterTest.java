/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.support;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.runners.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;

import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.LoaderHelper;
import com.gemstone.gemfire.cache.Region;

/**
 * Tests the adaption of the {@link java.util.concurrent.Callable} in to GemFire's {@link com.gemstone.gemfire.cache.CacheLoader} interface.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see java.util.concurrent.Callable
 * @see com.gemstone.gemfire.cache.CacheLoader
 * @see com.gemstone.gemfire.cache.LoaderHelper
 * @see com.gemstone.gemfire.cache.Region
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class CallableCacheLoaderAdapterTest {

	@Mock
	private CacheLoader<String, Object> mockCacheLoader;

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Mock
	private LoaderHelper<String, Object> mockLoaderHelper;

	@Mock
	private Region<String, Object> mockRegion;

	@Test
	public void constructCallableCacheLoaderAdapterWithArgumentKeyAndRegion() {
		CallableCacheLoaderAdapter<String, Object> instance = new CallableCacheLoaderAdapter<String, Object>(
			mockCacheLoader, "key", mockRegion, "test");

		assertThat(instance, is(notNullValue()));
		assertThat(instance.getCacheLoader(), is(sameInstance(mockCacheLoader)));
		assertThat(instance.getKey(), is(equalTo("key")));
		assertThat(instance.getRegion(), is(sameInstance(mockRegion)));
		assertThat(String.valueOf(instance.getArgument()), is(equalTo("test")));
	}

	@Test
	public void constructCallableCacheLoaderAdapterWithKeyRegionAndNoArgument() {
		CallableCacheLoaderAdapter<String, Object> instance = new CallableCacheLoaderAdapter<String, Object>(
			mockCacheLoader, "key", mockRegion);

		assertThat(instance, is(notNullValue()));
		assertThat(instance.getCacheLoader(), is(sameInstance(mockCacheLoader)));
		assertThat(instance.getKey(), is(equalTo("key")));
		assertThat(instance.getRegion(), is(sameInstance(mockRegion)));
		assertThat(instance.getArgument(), is(nullValue()));
	}

	@Test
	public void constructCallableCacheLoaderAdapterWithNoArgumentKeyOrRegion() {
		CallableCacheLoaderAdapter<String, Object> instance =
			new CallableCacheLoaderAdapter<String, Object>(mockCacheLoader);

		assertThat(instance, is(notNullValue()));
		assertThat(instance.getCacheLoader(), is(sameInstance(mockCacheLoader)));
		assertThat(instance.getKey(), is(nullValue()));
		assertThat(instance.getRegion(), is(nullValue()));
		assertThat(instance.getArgument(), is(nullValue()));
	}

	@Test
	public void constructCallableCacheLoaderAdapterWithNullCacheLoader() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("CacheLoader must not be null");

		new CallableCacheLoaderAdapter<Object, Object>(null);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void callDelegatesToLoad() throws Exception {
		CallableCacheLoaderAdapter<String, Object> instance = new CallableCacheLoaderAdapter<String, Object>(
			mockCacheLoader, "key", mockRegion, "test");

		when(mockCacheLoader.load(any(LoaderHelper.class))).thenAnswer(new Answer<String>() {
			public String answer(final InvocationOnMock invocation) throws Throwable {
				LoaderHelper<String, Object> loaderHelper = invocation.getArgumentAt(0, LoaderHelper.class);

				assertThat(loaderHelper, is(notNullValue()));
				assertThat((String) loaderHelper.getArgument(), is(equalTo("test")));
				assertThat(loaderHelper.getKey(), is(equalTo("key")));
				assertThat(loaderHelper.getRegion(), is(sameInstance(mockRegion)));

				return "mockValue";
			}
		});

		assertThat((String) instance.call(), is(equalTo("mockValue")));

		verify(mockCacheLoader, times(1)).load(isA(LoaderHelper.class));
	}

	@Test
	public void callThrowsIllegalStateExceptionForNullKey() throws Exception {
		CallableCacheLoaderAdapter<String, Object> instance = new CallableCacheLoaderAdapter<String, Object>(
			mockCacheLoader, null, mockRegion);

		assertThat(instance.getKey(), is(nullValue()));
		assertThat(instance.getRegion(), is(sameInstance(mockRegion)));

		expectedException.expect(IllegalStateException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("The key for which the value is loaded for cannot be null");

		instance.call();
	}

	@Test
	public void callThrowsIllegalStateExceptionForNullRegion() throws Exception {
		CallableCacheLoaderAdapter<String, Object> instance = new CallableCacheLoaderAdapter<String, Object>(
			mockCacheLoader, "key", null);

		assertThat(instance.getKey(), is(equalTo("key")));
		assertThat(instance.getRegion(), is(nullValue()));

		expectedException.expect(IllegalStateException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("The Region to load cannot be null");

		instance.call();
	}

	@Test
	public void closeDelegatesToCacheLoaderClose() {
		new CallableCacheLoaderAdapter<String, Object>(mockCacheLoader).close();
		verify(mockCacheLoader, times(1)).close();
	}

	@Test
	public void loadDelegatesToCacheLoaderLoad() {
		CallableCacheLoaderAdapter<String, Object> instance =
			new CallableCacheLoaderAdapter<String, Object>(mockCacheLoader);

		when(mockCacheLoader.load(eq(mockLoaderHelper))).thenReturn("test");

		assertThat((String) instance.load(mockLoaderHelper), is(equalTo("test")));

		verify(mockCacheLoader, times(1)).load(eq(mockLoaderHelper));
	}

}
