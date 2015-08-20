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

package org.springframework.data.gemfire;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.snapshot.CacheSnapshotService;
import com.gemstone.gemfire.cache.snapshot.RegionSnapshotService;
import com.gemstone.gemfire.cache.snapshot.SnapshotFilter;
import com.gemstone.gemfire.cache.snapshot.SnapshotOptions;

/**
 * The SnapshotServiceFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the SnapshotServiceFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean
 * @since 1.7.0
 */
@SuppressWarnings({ "rawtypes", "unchecked" })
public class SnapshotServiceFactoryBeanTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private SnapshotServiceFactoryBean<Object, Object> factoryBean = new SnapshotServiceFactoryBean<Object, Object>();

	protected <K, V> SnapshotServiceFactoryBean.SnapshotMetadata<K, V> newSnapshotMetadata() {
		return newSnapshotMetadata(new File(System.getProperty("user.dir")));
	}

	protected <K, V> SnapshotServiceFactoryBean.SnapshotMetadata<K, V> newSnapshotMetadata(File location) {
		return newSnapshotMetadata(location, null);
	}

	protected <K, V> SnapshotServiceFactoryBean.SnapshotMetadata<K, V> newSnapshotMetadata(File location,
			SnapshotFilter<K, V> filter) {
		return newSnapshotMetadata(location, filter, SnapshotOptions.SnapshotFormat.GEMFIRE);
	}

	protected <K, V> SnapshotServiceFactoryBean.SnapshotMetadata<K, V> newSnapshotMetadata(File location,
			SnapshotFilter<K, V> filter, SnapshotOptions.SnapshotFormat format) {
		return new SnapshotServiceFactoryBean.SnapshotMetadata<K, V>(location, filter, format);
	}

	protected <K, V> SnapshotServiceFactoryBean.SnapshotMetadata<K, V>[] toArray(
			SnapshotServiceFactoryBean.SnapshotMetadata<K, V>... metadata) {
		return metadata;
	}

	@After
	public void tearDown() {
		factoryBean.setExports(null);
		factoryBean.setImports(null);
		factoryBean.setRegion(null);
	}

	@Test
	public void setCacheToNull() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("The GemFire Cache must not be null");
		factoryBean.setCache(null);
	}

	@Test
	public void getCacheWhenUninitialized() {
		expectedException.expect(IllegalStateException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("The GemFire Cache was not properly initialized");
		factoryBean.getCache();
	}

	@Test
	public void setAndGetCacheSuccessfully() {
		Cache mockCache = mock(Cache.class, "MockCache");
		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean();

		factoryBean.setCache(mockCache);

		assertThat(factoryBean.getCache(), is(sameInstance(mockCache)));
	}

	@Test
	public void setAndGetExports() {
		SnapshotServiceFactoryBean.SnapshotMetadata[] actualExports = factoryBean.getExports();

		assertThat(actualExports, is(notNullValue()));
		assertThat(actualExports.length, is(equalTo(0)));

		SnapshotServiceFactoryBean.SnapshotMetadata[] expectedExports = toArray(newSnapshotMetadata());

		factoryBean.setExports(expectedExports);
		actualExports = factoryBean.getExports();

		assertThat(actualExports, is(sameInstance(expectedExports)));

		factoryBean.setExports(null);
		actualExports = factoryBean.getExports();

		assertThat(actualExports, is(not(sameInstance(expectedExports))));
		assertThat(actualExports, is(notNullValue()));
		assertThat(actualExports.length, is(equalTo(0)));
	}

	@Test
	public void setAndGetImports() {
		SnapshotServiceFactoryBean.SnapshotMetadata[] actualImports = factoryBean.getImports();

		assertThat(actualImports, is(notNullValue()));
		assertThat(actualImports.length, is(equalTo(0)));

		SnapshotServiceFactoryBean.SnapshotMetadata[] expectedImports = toArray(newSnapshotMetadata());

		factoryBean.setImports(expectedImports);
		actualImports = factoryBean.getImports();

		assertThat(actualImports, is(sameInstance(expectedImports)));

		factoryBean.setImports(null);
		actualImports = factoryBean.getImports();

		assertThat(actualImports, is(not(sameInstance(expectedImports))));
		assertThat(actualImports, is(notNullValue()));
		assertThat(actualImports.length, is(equalTo(0)));
	}

	@Test
	public void setAndGetRegionSuccessfully() {
		assertThat(factoryBean.getRegion(), is(nullValue()));

		Region<Object, Object> mockRegion = mock(Region.class, "MockRegion");

		factoryBean.setRegion(mockRegion);

		assertThat(factoryBean.getRegion(), is(sameInstance(mockRegion)));

		factoryBean.setRegion(null);

		assertThat(factoryBean.getRegion(), is(nullValue()));
	}

	@Test
	public void nullSafeArrayWithNonNullArray() {
		SnapshotServiceFactoryBean.SnapshotMetadata[] expectedConfigurations =
			new SnapshotServiceFactoryBean.SnapshotMetadata[0];

		assertThat(factoryBean.nullSafeArray(expectedConfigurations), is(sameInstance(expectedConfigurations)));
	}

	@Test
	public void nullSafeArrayWithNullArray() {
		assertThat(factoryBean.nullSafeArray(null), is(equalTo(SnapshotServiceFactoryBean.EMPTY_ARRAY)));
	}

	@Test
	public void wrapNullCacheSnapshotService() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("The backing CacheSnapshotService must not be null");
		factoryBean.wrap((CacheSnapshotService) null);
	}

	@Test
	public void wrapNullRegionSnapshotService() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("The backing RegionSnapshotService must not be null");
		factoryBean.wrap((RegionSnapshotService) null);
	}

	@Test
	public void isSingletonIsTrue() {
		assertThat(factoryBean.isSingleton(), is(true));
	}

	@Test
	public void createCacheSnapshotServiceAndImportOnInitialization() throws Exception {
		Cache mockCache = mock(Cache.class, "MockCache");
		CacheSnapshotService mockCacheSnapshotService = mock(CacheSnapshotService.class, "MockCacheSnapshotService");

		when(mockCache.getSnapshotService()).thenReturn(mockCacheSnapshotService);

		File userHomeDirectory = new File(System.getProperty("user.home"));

		SnapshotServiceFactoryBean.SnapshotMetadata<Object, Object>[] expectedImports = toArray(
			newSnapshotMetadata(userHomeDirectory));

		SnapshotServiceFactoryBean<Object, Object> factoryBean = new SnapshotServiceFactoryBean<Object, Object>();

		factoryBean.setCache(mockCache);
		factoryBean.setImports(expectedImports);
		factoryBean.setRegion(null);

		assertThat(factoryBean.getObject(), is(nullValue()));
		assertThat((Class<SnapshotServiceFactoryBean.SnapshotServiceAdapter>) factoryBean.getObjectType(),
			is(equalTo(SnapshotServiceFactoryBean.SnapshotServiceAdapter.class)));

		factoryBean.afterPropertiesSet();

		assertThat(factoryBean.getObject(), is(instanceOf(SnapshotServiceFactoryBean.CacheSnapshotServiceAdapter.class)));
		assertThat((Class<SnapshotServiceFactoryBean.CacheSnapshotServiceAdapter>) factoryBean.getObjectType(),
			is(equalTo(SnapshotServiceFactoryBean.CacheSnapshotServiceAdapter.class)));

		verify(mockCache, times(1)).getSnapshotService();
		verify(mockCacheSnapshotService, times(1)).load(eq(userHomeDirectory), eq(SnapshotOptions.SnapshotFormat.GEMFIRE));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createRegionSnapshotServiceAndImportOnInitialization() throws Exception {
		Cache mockCache = mock(Cache.class, "MockCache");
		Region mockRegion = mock(Region.class, "MockRegion");
		RegionSnapshotService mockRegionSnapshotService = mock(RegionSnapshotService.class, "MockRegionSnapshotService");
		SnapshotFilter mockFilter = mock(SnapshotFilter.class, "MockSnapshotFilter");
		SnapshotOptions mockSnapshotOptions = mock(SnapshotOptions.class, "MockSnapshotOptions");

		when(mockCache.getSnapshotService()).thenThrow(new UnsupportedOperationException("operation not supported"));
		when(mockRegion.getSnapshotService()).thenReturn(mockRegionSnapshotService);
		when(mockRegionSnapshotService.createOptions()).thenReturn(mockSnapshotOptions);
		when(mockSnapshotOptions.setFilter(any(SnapshotFilter.class))).thenReturn(mockSnapshotOptions);

		File snapshot = File.createTempFile("snapshot", "dat");

		snapshot.deleteOnExit();

		SnapshotServiceFactoryBean.SnapshotMetadata<Object, Object>[] expectedImports = toArray(
			newSnapshotMetadata(snapshot, mockFilter));

		SnapshotServiceFactoryBean<Object, Object> factoryBean = new SnapshotServiceFactoryBean<Object, Object>();

		factoryBean.setCache(mockCache);
		factoryBean.setImports(expectedImports);
		factoryBean.setRegion(mockRegion);

		assertThat(factoryBean.getObject(), is(nullValue()));
		assertThat((Class<SnapshotServiceFactoryBean.SnapshotServiceAdapter>) factoryBean.getObjectType(),
			is(equalTo(SnapshotServiceFactoryBean.SnapshotServiceAdapter.class)));

		factoryBean.afterPropertiesSet();

		assertThat(factoryBean.getObject(),
			is(instanceOf(SnapshotServiceFactoryBean.RegionSnapshotServiceAdapter.class)));
		assertThat((Class<SnapshotServiceFactoryBean.RegionSnapshotServiceAdapter>) factoryBean.getObjectType(),
			is(equalTo(SnapshotServiceFactoryBean.RegionSnapshotServiceAdapter.class)));

		verify(mockCache, never()).getSnapshotService();
		verify(mockRegion, times(1)).getSnapshotService();
		verify(mockRegionSnapshotService, times(1)).createOptions();
		verify(mockRegionSnapshotService, times(1)).load(eq(snapshot),
			eq(SnapshotOptions.SnapshotFormat.GEMFIRE), eq(mockSnapshotOptions));
		verify(mockSnapshotOptions, times(1)).setFilter(eq(mockFilter));
	}

}
