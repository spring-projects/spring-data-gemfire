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

import static com.gemstone.gemfire.cache.snapshot.SnapshotOptions.SnapshotFormat;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.junit.Assume.assumeThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isNull;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.SnapshotServiceFactoryBean.ArchiveFileFilter;
import static org.springframework.data.gemfire.SnapshotServiceFactoryBean.CacheSnapshotServiceAdapter;
import static org.springframework.data.gemfire.SnapshotServiceFactoryBean.RegionSnapshotServiceAdapter;
import static org.springframework.data.gemfire.SnapshotServiceFactoryBean.SnapshotMetadata;
import static org.springframework.data.gemfire.SnapshotServiceFactoryBean.SnapshotServiceAdapter;
import static org.springframework.data.gemfire.SnapshotServiceFactoryBean.SnapshotServiceAdapterSupport;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import org.apache.commons.logging.Log;
import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Matchers;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.gemfire.test.support.FileSystemUtils;

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
 * @see com.gemstone.gemfire.cache.snapshot.CacheSnapshotService
 * @see com.gemstone.gemfire.cache.snapshot.RegionSnapshotService
 * @since 1.7.0
 */
@SuppressWarnings({ "rawtypes", "unchecked" })
public class SnapshotServiceFactoryBeanTest {

	private static File snapshotDat;

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean();

	protected <K, V> SnapshotMetadata<K, V> newSnapshotMetadata() {
		return newSnapshotMetadata(FileSystemUtils.WORKING_DIRECTORY);
	}

	protected <K, V> SnapshotMetadata<K, V> newSnapshotMetadata(File location) {
		return newSnapshotMetadata(location, null);
	}

	protected <K, V> SnapshotMetadata<K, V> newSnapshotMetadata(SnapshotFilter<K, V> filter) {
		return newSnapshotMetadata(FileSystemUtils.WORKING_DIRECTORY, filter);
	}

	protected <K, V> SnapshotMetadata<K, V> newSnapshotMetadata(File location, SnapshotFilter<K, V> filter) {
		return newSnapshotMetadata(location, filter, SnapshotFormat.GEMFIRE);
	}

	protected <K, V> SnapshotMetadata<K, V> newSnapshotMetadata(File location, SnapshotFilter<K, V> filter,
			SnapshotFormat format) {
		return new SnapshotMetadata<K, V>(location, filter, format);
	}

	protected <K, V> SnapshotMetadata<K, V>[] toArray(SnapshotMetadata<K, V>... metadata) {
		return metadata;
	}

	@BeforeClass
	public static void setupBeforeClass() throws Exception {
		snapshotDat = File.createTempFile("snapshot", "dat");
		snapshotDat.deleteOnExit();
		assertThat(snapshotDat.isFile(), is(true));
	}

	@After
	public void tearDown() {
		factoryBean.setExports(null);
		factoryBean.setImports(null);
		factoryBean.setRegion(null);
	}

	@Test
	public void nullSafeArrayWithNonNullArray() {
		SnapshotMetadata[] expectedConfigurations = new SnapshotMetadata[0];

		assertThat(SnapshotServiceFactoryBean.nullSafeArray(expectedConfigurations),
			is(sameInstance(expectedConfigurations)));
	}

	@Test
	public void nullSafeArrayWithNullArray() {
		assertThat(SnapshotServiceFactoryBean.nullSafeArray(null), is(equalTo(SnapshotServiceFactoryBean.EMPTY_ARRAY)));
	}

	@Test
	public void nullSafeIsDirectoryWithDirectory() {
		assertThat(SnapshotServiceFactoryBean.nullSafeIsDirectory(new File(System.getProperty("user.dir"))), is(true));
	}

	@Test
	public void nullSafeIsDirectoryWithNonDirectories() {
		assertThat(SnapshotServiceFactoryBean.nullSafeIsDirectory(new File("path/to/non-existing/directory")),
			is(false));
		assertThat(SnapshotServiceFactoryBean.nullSafeIsDirectory(FileSystemUtils.JAVA_EXE), is(false));
	}

	@Test
	public void nullSafeIsFileWithFile() {
		assertThat(SnapshotServiceFactoryBean.nullSafeIsFile(FileSystemUtils.JAVA_EXE),
			is(FileSystemUtils.JAVA_EXE.isFile()));
	}

	@Test
	public void nullSafeIsFileWithNonFiles() {
		assertThat(SnapshotServiceFactoryBean.nullSafeIsFile(new File("/path/to/non-existing/file.ext")), is(false));
		assertThat(SnapshotServiceFactoryBean.nullSafeIsFile(new File(System.getProperty("user.dir"))), is(false));
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
		SnapshotMetadata[] actualExports = factoryBean.getExports();

		assertThat(actualExports, is(notNullValue()));
		assertThat(actualExports.length, is(equalTo(0)));

		SnapshotMetadata[] expectedExports = toArray(newSnapshotMetadata());

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
		SnapshotMetadata[] actualImports = factoryBean.getImports();

		assertThat(actualImports, is(notNullValue()));
		assertThat(actualImports.length, is(equalTo(0)));

		SnapshotMetadata[] expectedImports = toArray(newSnapshotMetadata());

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

		Region mockRegion = mock(Region.class, "MockRegion");

		factoryBean.setRegion(mockRegion);

		assertThat(factoryBean.getRegion(), is(sameInstance(mockRegion)));

		factoryBean.setRegion(null);

		assertThat(factoryBean.getRegion(), is(nullValue()));
	}

	@Test
	public void isSingletonIsTrue() {
		assertThat(factoryBean.isSingleton(), is(true));
	}

	@Test
	public void afterPropertiesSetCreatesSnapshotServiceAdapterAndDoesImportWithConfiguredImports() throws Exception {
		final SnapshotServiceAdapter mockSnapshotService = mock(SnapshotServiceAdapter.class,
			"MockSnapshotServiceAdapter");

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean() {
			@Override protected SnapshotServiceAdapter create() {
				return mockSnapshotService;
			}
		};

		SnapshotMetadata expectedSnapshotMetadata = newSnapshotMetadata();

		factoryBean.setImports(toArray(expectedSnapshotMetadata));
		factoryBean.afterPropertiesSet();

		assertThat(factoryBean.getImports()[0], is(equalTo(expectedSnapshotMetadata)));

		verify(mockSnapshotService, times(1)).doImport(eq(toArray(expectedSnapshotMetadata)));
	}

	@Test
	public void createCacheSnapshotService() {
		Cache mockCache = mock(Cache.class, "MockCache");
		CacheSnapshotService mockCacheSnapshotService = mock(CacheSnapshotService.class, "MockCacheSnapshotService");

		when(mockCache.getSnapshotService()).thenReturn(mockCacheSnapshotService);

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean();

		factoryBean.setCache(mockCache);

		SnapshotServiceAdapter adapter = factoryBean.create();

		assertThat(adapter, is(instanceOf(CacheSnapshotServiceAdapter.class)));

		verify(mockCache, times(1)).getSnapshotService();
	}

	@Test
	public void createRegionSnapshotService() {
		Region mockRegion = mock(Region.class, "MockRegion");
		RegionSnapshotService mockRegionSnapshotService = mock(RegionSnapshotService.class, "MockRegionSnapshotService");

		when(mockRegion.getSnapshotService()).thenReturn(mockRegionSnapshotService);

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean();

		factoryBean.setRegion(mockRegion);

		SnapshotServiceAdapter adapter = factoryBean.create();

		assertThat(adapter, is(instanceOf(RegionSnapshotServiceAdapter.class)));

		verify(mockRegion, times(1)).getSnapshotService();
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
	public void destroyPerformsExportWithConfiguredExports() throws Exception {
		final SnapshotServiceAdapter mockSnapshotService = mock(SnapshotServiceAdapter.class,
			"MockSnapshotServiceAdapter");

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean() {
			@Override public SnapshotServiceAdapter getObject() throws Exception {
				return mockSnapshotService;
			}
		};

		SnapshotMetadata expectedSnapshotMetadata = newSnapshotMetadata();

		factoryBean.setExports(toArray(expectedSnapshotMetadata));
		factoryBean.destroy();

		assertThat(factoryBean.getExports()[0], is(equalTo(expectedSnapshotMetadata)));

		verify(mockSnapshotService, times(1)).doExport(eq(toArray(expectedSnapshotMetadata)));
	}

	@Test
	public void onApplicationEventWhenMatchUsingEventSnapshotMetadataPerformsExport() throws Exception {
		Region mockRegion = mock(Region.class, "MockRegion");

		SnapshotOptions mockSnapshotOptions = mock(SnapshotOptions.class, "MockSnapshotOptions");

		final RegionSnapshotService mockSnapshotService = mock(RegionSnapshotService.class, "MockRegionSnapshotService");

		when(mockRegion.getFullPath()).thenReturn("/Example");
		when(mockSnapshotService.createOptions()).thenReturn(mockSnapshotOptions);
		when(mockSnapshotOptions.setFilter(any(SnapshotFilter.class))).thenReturn(mockSnapshotOptions);

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean() {
			@Override public SnapshotServiceAdapter getObject() throws Exception {
				return new RegionSnapshotServiceAdapter(mockSnapshotService);
			}
		};

		factoryBean.setExports(toArray(newSnapshotMetadata()));
		factoryBean.setRegion(mockRegion);

		SnapshotMetadata eventSnapshotMetadata = newSnapshotMetadata(snapshotDat);

		SnapshotApplicationEvent event = new SnapshotApplicationEvent(this, "/Example", eventSnapshotMetadata);

		assertThat(event.isRegionSnapshotEvent(), is(true));
		assertThat(event.getSnapshotMetadata()[0], is(sameInstance(eventSnapshotMetadata)));
		assertThat(factoryBean.getExports()[0], is(not(sameInstance(eventSnapshotMetadata))));
		assertThat(factoryBean.getRegion(), is(sameInstance(mockRegion)));

		factoryBean.onApplicationEvent(event);

		verify(mockRegion, times(1)).getFullPath();
		verify(mockSnapshotOptions, times(1)).setFilter(isNull(SnapshotFilter.class));
		verify(mockSnapshotService, times(1)).createOptions();
		verify(mockSnapshotService, times(1)).save(eq(eventSnapshotMetadata.getLocation()),
			eq(eventSnapshotMetadata.getFormat()), eq(mockSnapshotOptions));
	}

	@Test
	public void onApplicationEventWhenNoMatchDoesNotPerformExport() throws Exception {
		final CacheSnapshotService mockSnapshotService = mock(CacheSnapshotService.class, "MockCacheSnapshotService");

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean() {
			@Override public SnapshotServiceAdapter getObject() throws Exception {
				return new CacheSnapshotServiceAdapter(mockSnapshotService);
			}
		};

		SnapshotApplicationEvent event = new SnapshotApplicationEvent(this, "/Example");

		assertThat(event.isRegionSnapshotEvent(), is(true));
		assertThat(factoryBean.getRegion(), is(nullValue()));

		factoryBean.onApplicationEvent(event);

		verify(mockSnapshotService, never()).createOptions();
		verify(mockSnapshotService, never()).save(any(File.class), any(SnapshotFormat.class));
		verify(mockSnapshotService, never()).save(any(File.class), any(SnapshotFormat.class),
			any(SnapshotOptions.class));
	}

	@Test
	public void resolveSnapshotMetadataFromEvent() {
		SnapshotMetadata eventSnapshotMetadata = newSnapshotMetadata(snapshotDat);
		SnapshotMetadata exportSnapshotMetadata = newSnapshotMetadata();

		factoryBean.setExports(toArray(exportSnapshotMetadata));

		assertThat(factoryBean.getExports()[0], is(equalTo(exportSnapshotMetadata)));
		assertThat(factoryBean.resolveSnapshotMetadata(new SnapshotApplicationEvent(
			this, eventSnapshotMetadata))[0], is(equalTo(eventSnapshotMetadata)));
	}

	@Test
	public void resolveSnapshotMetadataFromFactory() {
		SnapshotMetadata exportSnapshotMetadata = newSnapshotMetadata();

		factoryBean.setExports(toArray(exportSnapshotMetadata));

		assertThat(factoryBean.getExports()[0], is(equalTo(exportSnapshotMetadata)));
		assertThat(factoryBean.resolveSnapshotMetadata(new SnapshotApplicationEvent(this))[0],
			is(equalTo(exportSnapshotMetadata)));
	}

	@Test
	public void withCacheBasedSnapshotServiceOnCacheSnapshotEventIsMatch() {
		SnapshotApplicationEvent event = new SnapshotApplicationEvent(this);

		assertThat(event.isCacheSnapshotEvent(), is(true));
		assertThat(event.isRegionSnapshotEvent(), is(false));
		assertThat(factoryBean.isMatch(event), is(true));
	}

	@Test
	public void withCacheBasedSnapshotServiceOnRegionSnapshotEventIsNotAMatch() {
		SnapshotApplicationEvent event = new SnapshotApplicationEvent(this, "/Example");

		assertThat(event.isCacheSnapshotEvent(), is(false));
		assertThat(event.isRegionSnapshotEvent(), is(true));
		assertThat(factoryBean.isMatch(event), is(false));
	}

	@Test
	public void withRegionBasedSnapshotServiceOnRegionSnapshotEventIsMatch() {
		SnapshotApplicationEvent event = new SnapshotApplicationEvent(this, "/Example");

		assertThat(event.isCacheSnapshotEvent(), is(false));
		assertThat(event.isRegionSnapshotEvent(), is(true));

		Region mockRegion = mock(Region.class, "MockRegion");

		when(mockRegion.getFullPath()).thenReturn(event.getRegionPath());

		factoryBean.setRegion(mockRegion);

		assertThat(factoryBean.getRegion(), is(sameInstance(mockRegion)));
		assertThat(factoryBean.isMatch(event), is(true));

		verify(mockRegion, times(1)).getFullPath();
	}

	@Test
	public void withRegionBasedSnapshotServiceOnCacheSnapshotEventIsNotAMatch() {
		SnapshotApplicationEvent event = new SnapshotApplicationEvent(this);

		assertThat(event.isCacheSnapshotEvent(), is(true));
		assertThat(event.isRegionSnapshotEvent(), is(false));

		factoryBean.setRegion(mock(Region.class, "MockRegion"));

		assertThat(factoryBean.getRegion(), is(notNullValue()));
		assertThat(factoryBean.isMatch(event), is(false));
	}

	@Test
	public void importCacheSnapshotOnInitialization() throws Exception {
		Cache mockCache = mock(Cache.class, "MockCache");

		CacheSnapshotService mockCacheSnapshotService = mock(CacheSnapshotService.class, "MockCacheSnapshotService");

		SnapshotFilter mockSnapshotFilterOne = mock(SnapshotFilter.class, "MockSnapshotFilterOne");
		SnapshotFilter mockSnapshotFilterTwo = mock(SnapshotFilter.class, "MockSnapshotFilterTwo");

		SnapshotOptions mockSnapshotOptionsOne = mock(SnapshotOptions.class, "MockSnapshotOptionsOne");
		SnapshotOptions mockSnapshotOptionsTwo = mock(SnapshotOptions.class, "MockSnapshotOptionsTwo");

		when(mockCache.getSnapshotService()).thenReturn(mockCacheSnapshotService);
		when(mockCacheSnapshotService.createOptions()).thenReturn(mockSnapshotOptionsOne)
			.thenReturn(mockSnapshotOptionsTwo);
		when(mockSnapshotOptionsOne.setFilter(eq(mockSnapshotFilterOne))).thenReturn(mockSnapshotOptionsOne);
		when(mockSnapshotOptionsTwo.setFilter(eq(mockSnapshotFilterTwo))).thenReturn(mockSnapshotOptionsTwo);

		SnapshotMetadata[] expectedImports = toArray(
			newSnapshotMetadata(FileSystemUtils.USER_HOME, mockSnapshotFilterOne),
				newSnapshotMetadata(mockSnapshotFilterTwo));

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean();

		factoryBean.setCache(mockCache);
		factoryBean.setImports(expectedImports);
		factoryBean.setRegion(null);

		assertThat(factoryBean.getObject(), is(nullValue()));
		assertThat((Class<SnapshotServiceAdapter>) factoryBean.getObjectType(),
			is(equalTo(SnapshotServiceAdapter.class)));

		factoryBean.afterPropertiesSet();

		assertThat(factoryBean.getObject(), is(instanceOf(CacheSnapshotServiceAdapter.class)));
		assertThat((Class<CacheSnapshotServiceAdapter>) factoryBean.getObjectType(),
			is(equalTo(CacheSnapshotServiceAdapter.class)));

		verify(mockCache, times(1)).getSnapshotService();
		verify(mockCacheSnapshotService, times(2)).createOptions();
		verify(mockCacheSnapshotService, times(1)).load(eq(FileSystemUtils.safeListFiles(FileSystemUtils.USER_HOME, FileSystemUtils.FileOnlyFilter.INSTANCE)),
			eq(SnapshotFormat.GEMFIRE), eq(mockSnapshotOptionsOne));
		verify(mockCacheSnapshotService, times(1)).load(eq(FileSystemUtils.safeListFiles(
			FileSystemUtils.WORKING_DIRECTORY, FileSystemUtils.FileOnlyFilter.INSTANCE)),
			eq(SnapshotFormat.GEMFIRE), eq(mockSnapshotOptionsTwo));
		verify(mockSnapshotOptionsOne, times(1)).setFilter(eq(mockSnapshotFilterOne));
		verify(mockSnapshotOptionsTwo, times(1)).setFilter(eq(mockSnapshotFilterTwo));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void importRegionSnapshotOnInitialization() throws Exception {
		Cache mockCache = mock(Cache.class, "MockCache");

		Region mockRegion = mock(Region.class, "MockRegion");

		RegionSnapshotService mockRegionSnapshotService = mock(RegionSnapshotService.class, "MockRegionSnapshotService");

		SnapshotFilter mockSnapshotFilterOne = mock(SnapshotFilter.class, "MockSnapshotFilterOne");
		SnapshotFilter mockSnapshotFilterTwo = mock(SnapshotFilter.class, "MockSnapshotFilterTwo");

		SnapshotOptions mockSnapshotOptionsOne = mock(SnapshotOptions.class, "MockSnapshotOptionsOne");
		SnapshotOptions mockSnapshotOptionsTwo = mock(SnapshotOptions.class, "MockSnapshotOptionsTwo");

		when(mockCache.getSnapshotService()).thenThrow(new UnsupportedOperationException("operation not supported"));
		when(mockRegion.getSnapshotService()).thenReturn(mockRegionSnapshotService);
		when(mockRegionSnapshotService.createOptions()).thenReturn(mockSnapshotOptionsOne)
			.thenReturn(mockSnapshotOptionsTwo);
		when(mockSnapshotOptionsOne.setFilter(eq(mockSnapshotFilterOne))).thenReturn(mockSnapshotOptionsOne);
		when(mockSnapshotOptionsTwo.setFilter(eq(mockSnapshotFilterTwo))).thenReturn(mockSnapshotOptionsTwo);

		File snapshotDatTwo = File.createTempFile("snapshot-2", "dat");

		snapshotDatTwo.deleteOnExit();

		SnapshotMetadata[] expectedImports = toArray(newSnapshotMetadata(snapshotDat, mockSnapshotFilterOne),
			newSnapshotMetadata(snapshotDatTwo, mockSnapshotFilterTwo));

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean();

		factoryBean.setCache(mockCache);
		factoryBean.setImports(expectedImports);
		factoryBean.setRegion(mockRegion);

		assertThat(factoryBean.getObject(), is(nullValue()));
		assertThat((Class<SnapshotServiceAdapter>) factoryBean.getObjectType(),
			is(equalTo(SnapshotServiceAdapter.class)));

		factoryBean.afterPropertiesSet();

		assertThat(factoryBean.getObject(), is(instanceOf(RegionSnapshotServiceAdapter.class)));
		assertThat((Class<RegionSnapshotServiceAdapter>) factoryBean.getObjectType(),
			is(equalTo(RegionSnapshotServiceAdapter.class)));

		verify(mockCache, never()).getSnapshotService();
		verify(mockRegion, times(1)).getSnapshotService();
		verify(mockRegionSnapshotService, times(2)).createOptions();
		verify(mockRegionSnapshotService, times(1)).load(eq(snapshotDat), eq(SnapshotFormat.GEMFIRE),
			eq(mockSnapshotOptionsOne));
		verify(mockRegionSnapshotService, times(1)).load(eq(snapshotDatTwo), eq(SnapshotFormat.GEMFIRE),
			eq(mockSnapshotOptionsTwo));
		verify(mockSnapshotOptionsOne, times(1)).setFilter(eq(mockSnapshotFilterOne));
		verify(mockSnapshotOptionsTwo, times(1)).setFilter(eq(mockSnapshotFilterTwo));
	}

	@Test
	public void exportCacheSnapshotOnDestroy() throws Exception {
		Cache mockCache = mock(Cache.class, "MockCache");

		CacheSnapshotService mockCacheSnapshotService = mock(CacheSnapshotService.class, "MockCacheSnapshotService");

		SnapshotFilter mockSnapshotFilterOne = mock(SnapshotFilter.class, "MockSnapshotFilterOne");
		SnapshotFilter mockSnapshotFilterTwo = mock(SnapshotFilter.class, "MockSnapshotFilterTwo");

		SnapshotOptions mockSnapshotOptionsOne = mock(SnapshotOptions.class, "MockSnapshotOptionsOne");
		SnapshotOptions mockSnapshotOptionsTwo = mock(SnapshotOptions.class, "MockSnapshotOptionsTwo");

		when(mockCache.getSnapshotService()).thenReturn(mockCacheSnapshotService);
		when(mockCacheSnapshotService.createOptions()).thenReturn(mockSnapshotOptionsOne)
			.thenReturn(mockSnapshotOptionsTwo);
		when(mockSnapshotOptionsOne.setFilter(eq(mockSnapshotFilterOne))).thenReturn(mockSnapshotOptionsOne);
		when(mockSnapshotOptionsTwo.setFilter(eq(mockSnapshotFilterTwo))).thenReturn(mockSnapshotOptionsTwo);

		SnapshotMetadata[] expectedExports = toArray(newSnapshotMetadata(mockSnapshotFilterOne),
			newSnapshotMetadata(mockSnapshotFilterTwo));

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean();

		factoryBean.setCache(mockCache);
		factoryBean.setExports(expectedExports);
		factoryBean.setImports(null);
		factoryBean.setRegion(null);
		factoryBean.afterPropertiesSet();
		factoryBean.destroy();

		assertThat(factoryBean.getObject(), is(instanceOf(CacheSnapshotServiceAdapter.class)));

		verify(mockCache, times(1)).getSnapshotService();
		verify(mockCacheSnapshotService, times(2)).createOptions();
		verify(mockCacheSnapshotService, times(1)).save(eq(expectedExports[0].getLocation()),
			eq(expectedExports[0].getFormat()), eq(mockSnapshotOptionsOne));
		verify(mockCacheSnapshotService, times(1)).save(eq(expectedExports[1].getLocation()),
			eq(expectedExports[1].getFormat()), eq(mockSnapshotOptionsTwo));
		verify(mockSnapshotOptionsOne, times(1)).setFilter(eq(mockSnapshotFilterOne));
		verify(mockSnapshotOptionsTwo, times(1)).setFilter(eq(mockSnapshotFilterTwo));
	}

	@Test
	public void exportRegionSnapshotOnDestroy() throws Exception {
		Cache mockCache = mock(Cache.class, "MockCache");

		Region mockRegion = mock(Region.class, "MockRegion");

		RegionSnapshotService mockRegionSnapshotService = mock(RegionSnapshotService.class, "MockRegionSnapshotService");

		SnapshotFilter mockSnapshotFilterOne = mock(SnapshotFilter.class, "MockSnapshotFilterOne");
		SnapshotFilter mockSnapshotFilterTwo = mock(SnapshotFilter.class, "MockSnapshotFilterTwo");

		SnapshotOptions mockSnapshotOptionsOne = mock(SnapshotOptions.class, "MockSnapshotOptionsOne");
		SnapshotOptions mockSnapshotOptionsTwo = mock(SnapshotOptions.class, "MockSnapshotOptionsTwo");

		when(mockCache.getSnapshotService()).thenThrow(new UnsupportedOperationException("operation not supported"));
		when(mockRegion.getSnapshotService()).thenReturn(mockRegionSnapshotService);
		when(mockRegionSnapshotService.createOptions()).thenReturn(mockSnapshotOptionsOne)
			.thenReturn(mockSnapshotOptionsTwo);
		when(mockSnapshotOptionsOne.setFilter(eq(mockSnapshotFilterOne))).thenReturn(mockSnapshotOptionsOne);
		when(mockSnapshotOptionsTwo.setFilter(eq(mockSnapshotFilterTwo))).thenReturn(mockSnapshotOptionsTwo);

		SnapshotMetadata[] expectedExports = toArray(newSnapshotMetadata(mockSnapshotFilterOne),
			newSnapshotMetadata(mockSnapshotFilterTwo));

		SnapshotServiceFactoryBean factoryBean = new SnapshotServiceFactoryBean();

		factoryBean.setCache(mockCache);
		factoryBean.setExports(expectedExports);
		factoryBean.setImports(null);
		factoryBean.setRegion(mockRegion);
		factoryBean.afterPropertiesSet();
		factoryBean.destroy();

		assertThat(factoryBean.getObject(), is(instanceOf(RegionSnapshotServiceAdapter.class)));

		verify(mockCache, never()).getSnapshotService();
		verify(mockRegion, times(1)).getSnapshotService();
		verify(mockRegionSnapshotService, times(2)).createOptions();
		verify(mockRegionSnapshotService, times(1)).save(eq(expectedExports[0].getLocation()),
			eq(expectedExports[0].getFormat()), eq(mockSnapshotOptionsOne));
		verify(mockRegionSnapshotService, times(1)).save(eq(expectedExports[1].getLocation()),
			eq(expectedExports[1].getFormat()), eq(mockSnapshotOptionsTwo));
		verify(mockSnapshotOptionsOne, times(1)).setFilter(eq(mockSnapshotFilterOne));
		verify(mockSnapshotOptionsTwo, times(1)).setFilter(eq(mockSnapshotFilterTwo));
	}

	@Test
	public void createOptionsWithFilterOnSnapshotServiceAdapterSupport() {
		SnapshotFilter mockSnapshotFilter = mock(SnapshotFilter.class, "MockSnapshotFilter");

		final SnapshotOptions mockSnapshotOptions = mock(SnapshotOptions.class, "MockSnapshotOptions");

		when(mockSnapshotOptions.setFilter(any(SnapshotFilter.class))).thenReturn(mockSnapshotOptions);

		TestSnapshotServiceAdapter snapshotService = new TestSnapshotServiceAdapter() {
			@Override public SnapshotOptions<Object, Object> createOptions() {
				return mockSnapshotOptions;
			}
		};

		assertThat(snapshotService.createOptions(mockSnapshotFilter), is(equalTo(mockSnapshotOptions)));

		verify(mockSnapshotOptions, times(1)).setFilter(eq(mockSnapshotFilter));
	}

	@Test
	public void invokeExceptionSuppressingCloseOnSnapshotServiceAdapterSupportIsSuccessful() throws Exception {
		Closeable mockCloseable = mock(Closeable.class, "MockCloseable");

		assertThat(new TestSnapshotServiceAdapter().exceptionSuppressingClose(mockCloseable), is(true));

		verify(mockCloseable, times(1)).close();
	}

	@Test
	public void invokeExceptionSuppressingCloseOnSnapshotServiceAdapterSupportIsUnsuccessful() throws Exception {
		Closeable mockCloseable = mock(Closeable.class, "MockCloseable");

		doThrow(new IOException("TEST")).when(mockCloseable).close();

		assertThat(new TestSnapshotServiceAdapter().exceptionSuppressingClose(mockCloseable), is(false));

		verify(mockCloseable, times(1)).close();
	}

	@Test
	public void logDebugWhenDebugging() {
		final Log mockLog = mock(Log.class, "MockLog");

		when(mockLog.isDebugEnabled()).thenReturn(true);

		TestSnapshotServiceAdapter snapshotService = new TestSnapshotServiceAdapter() {
			@Override Log createLog() {
				return mockLog;
			}
		};

		Exception expectedException = new Exception("test");

		snapshotService.logDebug(expectedException, "Log message with argument (%1$s)", "test");

		verify(mockLog, times(1)).isDebugEnabled();
		verify(mockLog, times(1)).debug(eq("Log message with argument (test)"), eq(expectedException));
	}

	@Test
	public void logDebugWhenNotDebugging() {
		final Log mockLog = mock(Log.class, "MockLog");

		when(mockLog.isDebugEnabled()).thenReturn(false);

		TestSnapshotServiceAdapter snapshotService = new TestSnapshotServiceAdapter() {
			@Override Log createLog() {
				return mockLog;
			}
		};

		snapshotService.logDebug(null, "Log message with argument (%1$s)", "test");

		verify(mockLog, times(1)).isDebugEnabled();
		verify(mockLog, never()).debug(any(String.class), any(Throwable.class));
	}

	@Test
	public void toSimpleFilenameUsingVariousPathnames() {
		TestSnapshotServiceAdapter snapshotService = new TestSnapshotServiceAdapter();

		assertThat(snapshotService.toSimpleFilename("/path/to/file.ext"), is(equalTo("file.ext")));
		assertThat(snapshotService.toSimpleFilename("/path/to/file   "), is(equalTo("file")));
		assertThat(snapshotService.toSimpleFilename("/ file.ext"), is(equalTo("file.ext")));
		assertThat(snapshotService.toSimpleFilename("  file.ext "), is(equalTo("file.ext")));
		assertThat(snapshotService.toSimpleFilename("/ "), is(equalTo("")));
		assertThat(snapshotService.toSimpleFilename("  "), is(equalTo("")));
		assertThat(snapshotService.toSimpleFilename(""), is(equalTo("")));
		assertThat(snapshotService.toSimpleFilename(null), is(nullValue()));
	}

	@Test(expected = ImportSnapshotException.class)
	public void loadCacheSnapshotWithDirectoryAndFormatHandlesExceptionAppropriately() throws Exception {
		CacheSnapshotService mockCacheSnapshotService = mock(CacheSnapshotService.class, "MockCacheSnapshotService");

		doThrow(new IOException("TEST")).when(mockCacheSnapshotService).load(any(File.class), any(SnapshotFormat.class));

		CacheSnapshotServiceAdapter adapter = new CacheSnapshotServiceAdapter(mockCacheSnapshotService);

		assertThat(adapter.getSnapshotService(), is(equalTo(mockCacheSnapshotService)));

		try {
			adapter.load(FileSystemUtils.WORKING_DIRECTORY, SnapshotFormat.GEMFIRE);
		}
		catch (ImportSnapshotException expected) {
			assertThat(expected.getMessage(), is(equalTo(String.format(
				"Failed to load snapshots from directory (%1$s) in format (GEMFIRE)",
					FileSystemUtils.WORKING_DIRECTORY))));
			assertThat(expected.getCause(), is(instanceOf(IOException.class)));
			assertThat(expected.getCause().getMessage(), is(equalTo("TEST")));
			throw expected;
		}
		finally {
			verify(mockCacheSnapshotService, times(1)).load(eq(FileSystemUtils.WORKING_DIRECTORY),
				eq(SnapshotFormat.GEMFIRE));
		}
	}

	@Test(expected = ImportSnapshotException.class)
	public void loadCacheSnapshotWithFormatOptionsAndSnapshotFilesHandlesExceptionAppropriately() throws Exception {
		SnapshotOptions mockSnapshotOptions = mock(SnapshotOptions.class, "MockSnapshotOptions");

		CacheSnapshotService mockCacheSnapshotService = mock(CacheSnapshotService.class, "MockCacheSnapshotService");

		doThrow(new ClassCastException("TEST")).when(mockCacheSnapshotService).load(any(File[].class),
			any(SnapshotFormat.class), any(SnapshotOptions.class));

		CacheSnapshotServiceAdapter adapter = new CacheSnapshotServiceAdapter(mockCacheSnapshotService);

		assertThat(adapter.getSnapshotService(), is(equalTo(mockCacheSnapshotService)));

		try {
			adapter.load(SnapshotFormat.GEMFIRE, mockSnapshotOptions, snapshotDat);
		}
		catch (ImportSnapshotException expected) {
			assertThat(expected.getMessage(), is(equalTo(String.format(
				"Failed to load snapshots (%1$s) in format (GEMFIRE) using options (%2$s)",
					Arrays.toString(new File[] { snapshotDat }), mockSnapshotOptions))));
			assertThat(expected.getCause(), is(instanceOf(ClassCastException.class)));
			assertThat(expected.getCause().getMessage(), is(equalTo("TEST")));
			throw expected;
		}
		finally {
			verify(mockCacheSnapshotService, times(1)).load(eq(new File[] { snapshotDat }),
				eq(SnapshotFormat.GEMFIRE), Matchers.isA(SnapshotOptions.class));
		}
	}

	@Test(expected = ExportSnapshotException.class)
	public void saveCacheSnapshotWithDirectoryAndFormatHandlesExceptionAppropriately() throws Exception {
		CacheSnapshotService mockCacheSnapshotService = mock(CacheSnapshotService.class, "MockCacheSnapshotService");

		doThrow(new IOException("TEST")).when(mockCacheSnapshotService).save(any(File.class), any(SnapshotFormat.class));

		CacheSnapshotServiceAdapter adapter = new CacheSnapshotServiceAdapter(mockCacheSnapshotService);

		assertThat(adapter.getSnapshotService(), is(equalTo(mockCacheSnapshotService)));

		try {
			adapter.save(FileSystemUtils.WORKING_DIRECTORY, SnapshotFormat.GEMFIRE);
		}
		catch (ExportSnapshotException expected) {
			assertThat(expected.getMessage(), is(equalTo(String.format(
				"Failed to save snapshots to directory (%1$s) in format (GEMFIRE)",
					FileSystemUtils.WORKING_DIRECTORY))));
			assertThat(expected.getCause(), is(instanceOf(IOException.class)));
			assertThat(expected.getCause().getMessage(), is(equalTo("TEST")));
			throw expected;
		}
		finally {
			verify(mockCacheSnapshotService, times(1)).save(eq(FileSystemUtils.WORKING_DIRECTORY),
				eq(SnapshotFormat.GEMFIRE));
		}
	}

	@Test(expected = ExportSnapshotException.class)
	public void saveCacheSnapshotWithDirectoryFormatAndOptionsHandlesExceptionAppropriately() throws Exception {
		SnapshotOptions mockSnapshotOptions = mock(SnapshotOptions.class, "MockSnapshotOptions");

		CacheSnapshotService mockCacheSnapshotService = mock(CacheSnapshotService.class, "MockCacheSnapshotService");

		doThrow(new ClassCastException("TEST")).when(mockCacheSnapshotService).save(any(File.class),
			any(SnapshotFormat.class), any(SnapshotOptions.class));

		CacheSnapshotServiceAdapter adapter = new CacheSnapshotServiceAdapter(mockCacheSnapshotService);

		assertThat(adapter.getSnapshotService(), is(equalTo(mockCacheSnapshotService)));

		try {
			adapter.save(FileSystemUtils.USER_HOME, SnapshotFormat.GEMFIRE, mockSnapshotOptions);
		}
		catch (ExportSnapshotException expected) {
			assertThat(expected.getMessage(), is(equalTo(String.format(
				"Failed to save snapshots to directory (%1$s) in format (GEMFIRE) using options (%2$s)",
					FileSystemUtils.USER_HOME, mockSnapshotOptions))));
			assertThat(expected.getCause(), is(instanceOf(ClassCastException.class)));
			assertThat(expected.getCause().getMessage(), is(equalTo("TEST")));
			throw expected;
		}
		finally {
			verify(mockCacheSnapshotService, times(1)).save(eq(FileSystemUtils.USER_HOME), eq(SnapshotFormat.GEMFIRE),
				Matchers.isA(SnapshotOptions.class));
		}
	}

	@Test(expected = ImportSnapshotException.class)
	public void loadRegionSnapshotWithSnapshotFileAndFormatHandlesExceptionAppropriately() throws Exception {
		RegionSnapshotService mockRegionSnapshotService = mock(RegionSnapshotService.class, "MockRegionSnapshotService");

		doThrow(new IOException("TEST")).when(mockRegionSnapshotService).load(any(File.class),
			any(SnapshotFormat.class));

		RegionSnapshotServiceAdapter adapter = new RegionSnapshotServiceAdapter(mockRegionSnapshotService);

		assertThat(adapter.getSnapshotService(), is(equalTo(mockRegionSnapshotService)));

		try {
			adapter.load(snapshotDat, SnapshotFormat.GEMFIRE);
		}
		catch (ImportSnapshotException expected) {
			assertThat(expected.getMessage(), is(equalTo(String.format(
				"Failed to load snapshot from file (%1$s) in format (GEMFIRE)", snapshotDat))));
			assertThat(expected.getCause(), is(instanceOf(IOException.class)));
			assertThat(expected.getCause().getMessage(), is(equalTo("TEST")));
			throw expected;
		}
		finally {
			verify(mockRegionSnapshotService, times(1)).load(eq(snapshotDat), eq(SnapshotFormat.GEMFIRE));
		}
	}

	@Test(expected = ImportSnapshotException.class)
	public void loadRegionSnapshotWithFormatOptionsAndSnapshotFilesHandlesExceptionAppropriately() throws Exception {
		SnapshotOptions mockSnapshotOptions = mock(SnapshotOptions.class, "MockSnapshotOptions");

		RegionSnapshotService mockRegionSnapshotService = mock(RegionSnapshotService.class, "MockRegionSnapshotService");

		doThrow(new ClassCastException("TEST")).when(mockRegionSnapshotService).load(
			any(File.class), any(SnapshotFormat.class), any(SnapshotOptions.class));

		RegionSnapshotServiceAdapter adapter = new RegionSnapshotServiceAdapter(mockRegionSnapshotService);

		assertThat(adapter.getSnapshotService(), is(equalTo(mockRegionSnapshotService)));

		try {
			adapter.load(SnapshotFormat.GEMFIRE, mockSnapshotOptions, snapshotDat);
		}
		catch (ImportSnapshotException expected) {
			assertThat(expected.getMessage(), is(equalTo(String.format(
				"Failed to load snapshots (%1$s) in format (GEMFIRE) using options (%2$s)",
					Arrays.toString(new File[] { snapshotDat }), mockSnapshotOptions))));
			assertThat(expected.getCause(), is(instanceOf(ClassCastException.class)));
			assertThat(expected.getCause().getMessage(), is(equalTo("TEST")));
			throw expected;
		}
		finally {
			verify(mockRegionSnapshotService, times(1)).load(eq(snapshotDat), eq(SnapshotFormat.GEMFIRE),
				eq(mockSnapshotOptions));
		}
	}

	@Test(expected = ExportSnapshotException.class)
	public void saveRegionSnapshotWithSnapshotFileAndFormatHandlesExceptionAppropriately() throws Exception {
		RegionSnapshotService mockRegionSnapshotService = mock(RegionSnapshotService.class, "MockRegionSnapshotService");

		doThrow(new IOException("TEST")).when(mockRegionSnapshotService).save(any(File.class),
			any(SnapshotFormat.class));

		RegionSnapshotServiceAdapter adapter = new RegionSnapshotServiceAdapter(mockRegionSnapshotService);

		assertThat(adapter.getSnapshotService(), is(equalTo(mockRegionSnapshotService)));

		try {
			adapter.save(snapshotDat, SnapshotFormat.GEMFIRE);
		}
		catch (ExportSnapshotException expected) {
			assertThat(expected.getMessage(), is(equalTo(String.format(
				"Failed to save snapshot to file (%1$s) in format (GEMFIRE)", snapshotDat))));
			assertThat(expected.getCause(), is(instanceOf(IOException.class)));
			assertThat(expected.getCause().getMessage(), is(equalTo("TEST")));
			throw expected;
		}
		finally {
			verify(mockRegionSnapshotService, times(1)).save(eq(snapshotDat), eq(SnapshotFormat.GEMFIRE));
		}
	}

	@Test(expected = ExportSnapshotException.class)
	public void saveRegionSnapshotWithSnapshotFileFormatAndOptionsHandlesExceptionAppropriately() throws Exception {
		SnapshotOptions mockSnapshotOptions = mock(SnapshotOptions.class, "MockSnapahotOptions");

		RegionSnapshotService mockRegionSnapshotService = mock(RegionSnapshotService.class, "MockRegionSnapshotService");

		doThrow(new ClassCastException("TEST")).when(mockRegionSnapshotService).save(any(File.class),
			any(SnapshotFormat.class), any(SnapshotOptions.class));

		RegionSnapshotServiceAdapter adapter = new RegionSnapshotServiceAdapter(mockRegionSnapshotService);

		assertThat(adapter.getSnapshotService(), is(equalTo(mockRegionSnapshotService)));

		try {
			adapter.save(snapshotDat, SnapshotFormat.GEMFIRE, mockSnapshotOptions);
		}
		catch (ExportSnapshotException expected) {
			assertThat(expected.getMessage(), is(equalTo(String.format(
				"Failed to save snapshot to file (%1$s) in format (GEMFIRE) using options (%2$s)",
					snapshotDat, mockSnapshotOptions))));
			assertThat(expected.getCause(), is(instanceOf(ClassCastException.class)));
			assertThat(expected.getCause().getMessage(), is(equalTo("TEST")));
			throw expected;
		}
		finally {
			verify(mockRegionSnapshotService, times(1)).save(eq(snapshotDat), eq(SnapshotFormat.GEMFIRE),
				eq(mockSnapshotOptions));
		}
	}

	@Test
	public void createSnapshotMetadataWithNullLocation() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("Location must not be null");
		new SnapshotMetadata(null, mock(SnapshotFilter.class), SnapshotFormat.GEMFIRE);
	}

	@Test
	public void createSnapshotMetadataWithDirectoryFilterAndUnspecifiedFormat() {
		SnapshotFilter mockSnapshotFilter = mock(SnapshotFilter.class, "MockSnapshotFilter");

		SnapshotMetadata metadata = new SnapshotMetadata(FileSystemUtils.WORKING_DIRECTORY, mockSnapshotFilter, null);

		assertThat(metadata.getLocation(), is(equalTo(FileSystemUtils.WORKING_DIRECTORY)));
		assertThat(metadata.isDirectory(), is(true));
		assertThat(metadata.isFile(), is(false));
		assertThat(metadata.getFilter(), is(equalTo(mockSnapshotFilter)));
		assertThat(metadata.isFilterPresent(), is(true));
		assertThat(metadata.getFormat(), is(equalTo(SnapshotFormat.GEMFIRE)));
	}

	@Test
	public void createSnapshotMetadataWithFileNullFilterAndGemFireFormat() throws Exception {
		SnapshotMetadata metadata = new SnapshotMetadata(snapshotDat, null, SnapshotFormat.GEMFIRE);

		assertThat(metadata.getLocation(), is(equalTo(snapshotDat)));
		assertThat(metadata.isDirectory(), is(false));
		assertThat(metadata.isFile(), is(true));
		assertThat(metadata.getFilter(), is(nullValue()));
		assertThat(metadata.isFilterPresent(), is(false));
		assertThat(metadata.getFormat(), is(equalTo(SnapshotFormat.GEMFIRE)));
	}

	@Test
	public void isJarFileIsTrue() {
		// JRE
		File runtimeDotJar = new File(new File(FileSystemUtils.JAVA_HOME, "lib"), "rt.jar");

		// JDK
		if (!runtimeDotJar.isFile()) {
			runtimeDotJar = new File(new File(new File(FileSystemUtils.JAVA_HOME, "jre"), "lib"), "rt.jar");
			assumeThat(runtimeDotJar.isFile(), is(true));
		}

		assertThat(ArchiveFileFilter.INSTANCE.isJarFile(runtimeDotJar), is(true));
	}

	@Test
	public void isJarFileIsFalse() throws Exception {
		assertThat(ArchiveFileFilter.INSTANCE.isJarFile(new File("/path/to/non-existing/file.jar")), is(false));
		assertThat(ArchiveFileFilter.INSTANCE.isJarFile(new ClassPathResource("/cluster_config.zip").getFile()), is(false));
		assertThat(ArchiveFileFilter.INSTANCE.isJarFile(new File("to/file.tar")), is(false));
		assertThat(ArchiveFileFilter.INSTANCE.isJarFile(new File("jar.file")), is(false));
		assertThat(ArchiveFileFilter.INSTANCE.isJarFile(new File("  ")), is(false));
		assertThat(ArchiveFileFilter.INSTANCE.isJarFile(new File("")), is(false));
		assertThat(ArchiveFileFilter.INSTANCE.isJarFile(null), is(false));
	}

	@Test
	public void getFileExtensionOfVariousFiles() throws Exception {
		assertThat(ArchiveFileFilter.INSTANCE.getFileExtension(new ClassPathResource("/cluster_config.zip").getFile()), is(equalTo("zip")));
		assertThat(ArchiveFileFilter.INSTANCE.getFileExtension(new File("/path/to/non-existing/file.jar")), is(equalTo("")));
		assertThat(ArchiveFileFilter.INSTANCE.getFileExtension(new File("to/non-existing/file.tar")), is(equalTo("")));
		assertThat(ArchiveFileFilter.INSTANCE.getFileExtension(FileSystemUtils.WORKING_DIRECTORY), is(equalTo("")));
		assertThat(ArchiveFileFilter.INSTANCE.getFileExtension(new File("  ")), is(equalTo("")));
		assertThat(ArchiveFileFilter.INSTANCE.getFileExtension(new File("")), is(equalTo("")));
		assertThat(ArchiveFileFilter.INSTANCE.getFileExtension(null), is(equalTo("")));
	}

	@Test
	public void archiveFileFilterAcceptsJarOrZipFile() throws Exception {
		assertThat(ArchiveFileFilter.INSTANCE.accept(new ClassPathResource("/cluster_config.zip").getFile()), is(true));
	}

	@Test
	public void archiveFileFilterRejectsTarFile() {
		assertThat(ArchiveFileFilter.INSTANCE.accept(new File("/path/to/file.tar")), is(false));
	}

	protected static class TestSnapshotServiceAdapter extends SnapshotServiceAdapterSupport<Object, Object> {

		@Override
		public SnapshotOptions<Object, Object> createOptions() {
			throw new UnsupportedOperationException("not implemented");
		}

		@Override
		protected File[] handleLocation(final SnapshotMetadata<Object, Object> configuration) {
			throw new UnsupportedOperationException("not implemented");
		}

		@Override
		public void load(final File directory, final SnapshotFormat format) {
			throw new UnsupportedOperationException("not implemented");
		}

		@Override
		public void load(SnapshotFormat format, SnapshotOptions<Object, Object> options, File... snapshots) {
			throw new UnsupportedOperationException("not implemented");
		}

		@Override
		public void save(final File location, final SnapshotFormat format) {
			throw new UnsupportedOperationException("not implemented");
		}

		@Override
		public void save(File location, SnapshotFormat format, SnapshotOptions<Object, Object> options) {
			throw new UnsupportedOperationException("not implemented");
		}
	}

}
