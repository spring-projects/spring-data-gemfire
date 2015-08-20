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

import java.io.File;
import java.util.Arrays;

import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.snapshot.CacheSnapshotService;
import com.gemstone.gemfire.cache.snapshot.RegionSnapshotService;
import com.gemstone.gemfire.cache.snapshot.SnapshotFilter;
import com.gemstone.gemfire.cache.snapshot.SnapshotOptions;

/**
 * The SnapshotServiceFactoryBean class is a Spring FactoryBean used to configure and create an instance
 * of the appropriate GemFire Snapshot Service.  A CacheSnapshotService is created if the Region is not specified,
 * otherwise a RegionSnapshotService is used based on the configured Region.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see com.gemstone.gemfire.cache.snapshot.CacheSnapshotService
 * @see com.gemstone.gemfire.cache.snapshot.RegionSnapshotService
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class SnapshotServiceFactoryBean<K, V> implements FactoryBean<SnapshotServiceFactoryBean.SnapshotServiceAdapter<K, V>>,
		InitializingBean, DisposableBean {

	protected static final SnapshotMetadata[] EMPTY_ARRAY = new SnapshotMetadata[0];

	private Cache cache;

	private Region<K, V> region;

	private SnapshotServiceAdapter<K, V> snapshotServiceAdapter;

	private SnapshotMetadata<K, V>[] exports;
	private SnapshotMetadata<K, V>[] imports;

	/**
	 * Sets a reference to the GemFire Cache for which the snapshot will be taken.
	 *
	 * @param cache the GemFire Cache used to create an instance of CacheSnapshotService.
	 * @throws IllegalArgumentException if the Cache reference is null.
	 * @see com.gemstone.gemfire.cache.Cache
	 * @see #getCache()
	 */
	public void setCache(Cache cache) {
		Assert.notNull(cache, "The GemFire Cache must not be null");
		this.cache = cache;
	}

	/**
	 * Gets a reference to the GemFire Cache for which the snapshot will be taken.
	 *
	 * @return the GemFire Cache used to create an instance of CacheSnapshotService.
	 * @throws IllegalStateException if the Cache argument is null.
	 * @see com.gemstone.gemfire.cache.Cache
	 * @see #setCache(Cache)
	 */
	protected Cache getCache() {
		Assert.state(cache != null, "The GemFire Cache was not properly initialized");
		return cache;
	}

	public void setExports(SnapshotMetadata<K, V>[] exports) {
		this.exports = exports;
	}

	protected SnapshotMetadata<K, V>[] getExports() {
		return nullSafeArray(exports);
	}

	public void setImports(SnapshotMetadata<K, V>[] imports) {
		this.imports = imports;
	}

	protected SnapshotMetadata<K, V>[] getImports() {
		return nullSafeArray(imports);
	}

	/**
	 * Sets a reference to the GemFire Region for which the snapshot will be taken.
	 *
	 * @param region the GemFire Region used to create an instance of the RegionSnapshotService.
	 * @see com.gemstone.gemfire.cache.Region
	 * @see #getRegion()
	 */
	public void setRegion(Region<K, V> region) {
		this.region = region;
	}

	/**
	 * Gets a reference to the GemFire Region for which the snapshot will be taken.
	 *
	 * @return the GemFire Region used to create an instance of the RegionSnapshotService.
	 * @see com.gemstone.gemfire.cache.Region
	 * @see #getRegion()
	 */
	protected Region<K, V> getRegion() {
		return region;
	}

	/**
	 * Gets the reference to the GemFire Snapshot Service created by this FactoryBean.
	 *
	 * @return the GemFire Snapshot Service created by this FactoryBean.
	 * @throws Exception if the GemFire Snapshot Service failed to be created.
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 */
	@Override
	@SuppressWarnings("unchecked")
	public SnapshotServiceAdapter<K, V> getObject() throws Exception {
		return snapshotServiceAdapter;
	}

	/**
	 * Gets the type of Snapshot Service created by this FactoryBean.
	 *
	 * @return a Class object representing the type of Snapshot Service created by this FactoryBean.
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.CacheSnapshotServiceAdapter
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.RegionSnapshotServiceAdapter
	 */
	@Override
	public Class<?> getObjectType() {
		return (snapshotServiceAdapter != null ? snapshotServiceAdapter.getClass() : SnapshotServiceAdapter.class);
	}

	/**
	 * Determines this this FactoryBean creates single GemFire Snapshot Service instances.
	 *
	 * @return true.
	 */
	@Override
	public boolean isSingleton() {
		return true;
	}

	/**
	 * Constructs and initializes the GemFire Snapshot Service used to take a snapshot of the configured Cache
	 * or Region if initialized.  In addition, this initialization method will perform the actual import.
	 *
	 * @throws Exception if the construction and initialization of the GemFire Snapshot Service fails.
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.SnapshotServiceAdapter#doImport(SnapshotMetadata[])
	 * @see #getImports()
	 * @see #create()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void afterPropertiesSet() throws Exception {
		snapshotServiceAdapter = create();
		snapshotServiceAdapter.doImport(getImports());
	}

	/**
	 * Constructs an appropriate instance of the SnapshotServiceAdapter based on the FactoryBean configuration. If
	 * a Region has not been specified, then a GemFire Snapshot Service for the Cache is constructed, otherwise
	 * the GemFire Snapshot Service for the configured Region is used.
	 *
	 * @return a SnapshotServiceAdapter wrapping the appropriate GemFire Snapshot Service (either Cache or Region)
	 * depending on the FactoryBean configuration.
	 * @see #wrap(CacheSnapshotService)
	 * @see #wrap(RegionSnapshotService)
	 * @see #getRegion()
	 */
	protected SnapshotServiceAdapter create() {
		Region<K, V> region = getRegion();
		return (region != null ? wrap(region.getSnapshotService()) : wrap(getCache().getSnapshotService()));
	}

	/**
	 * Wraps the GemFire CacheSnapshotService into an appropriate Adapter to ...
	 *
	 * @param cacheSnapshotService the GemFire CacheSnapshotService to wrap.
	 * @return a SnapshotServiceAdapter wrapping the GemFire CacheSnapshotService.
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.CacheSnapshotServiceAdapter
	 * @see com.gemstone.gemfire.cache.snapshot.CacheSnapshotService
	 */
	protected SnapshotServiceAdapter<Object, Object> wrap(CacheSnapshotService cacheSnapshotService) {
		return new CacheSnapshotServiceAdapter(cacheSnapshotService);
	}

	/**
	 * Wraps the GemFire RegionSnapshotService into an appropriate Adapter to ...
	 *
	 * @param regionSnapshotService the GemFire RegionSnapshotService to wrap.
	 * @return a SnapshotServiceAdapter wrapping the GemFire RegionSnapshotService.
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.RegionSnapshotServiceAdapter
	 * @see com.gemstone.gemfire.cache.snapshot.RegionSnapshotService
	 */
	protected SnapshotServiceAdapter<K, V> wrap(RegionSnapshotService<K, V> regionSnapshotService) {
		return new RegionSnapshotServiceAdapter(regionSnapshotService);
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	protected SnapshotMetadata<K, V>[] nullSafeArray(SnapshotMetadata<K, V>[] configurations) {
		return (configurations != null ? configurations : EMPTY_ARRAY);
	}

	/**
	 * Performs an export of the GemFire Cache or Region if configured.
	 *
	 * @throws Exception if the Cache/Region data export operation fails.
	 * @see org.springframework.data.gemfire.SnapshotServiceFactoryBean.SnapshotServiceAdapter#doExport(SnapshotMetadata[])
	 * @see #getExports()
	 * @see #getObject()
	 */
	@Override
	public void destroy() throws Exception {
		getObject().doExport(getExports());
	}

	public interface SnapshotServiceAdapter<K, V> {

		SnapshotOptions<K, V> createOptions();

		void doExport(SnapshotMetadata<K, V>[] configurations);

		void doImport(SnapshotMetadata<K, V>[] configurations);

		void load(File directory, SnapshotFormat format);

		void load(SnapshotFormat format, SnapshotOptions<K, V> options, File... snapshots);

		void save(File location, SnapshotFormat format);

		void save(File location, SnapshotFormat format, SnapshotOptions<K, V> options);

	}

	protected class CacheSnapshotServiceAdapter implements SnapshotServiceAdapter<Object, Object> {

		private final CacheSnapshotService snapshotService;

		public CacheSnapshotServiceAdapter(CacheSnapshotService snapshotService) {
			Assert.notNull(snapshotService, "The backing CacheSnapshotService must not be null");
			this.snapshotService = snapshotService;
		}

		protected CacheSnapshotService getSnapshotService() {
			return snapshotService;
		}

		@Override
		public SnapshotOptions<Object, Object> createOptions() {
			return getSnapshotService().createOptions();
		}

		protected SnapshotOptions<Object, Object> createOptions(SnapshotFilter<Object, Object> filter) {
			return createOptions().setFilter(filter);
		}

		@Override
		public void doExport(SnapshotMetadata<Object, Object>[] configurations) {
			if (!ObjectUtils.isEmpty(configurations)) {
				for (SnapshotMetadata<Object, Object> configuration : configurations) {
					save(configuration.getLocation(), configuration.getFormat(),
						createOptions(configuration.getFilter()));
				}
			}
		}

		@Override
		public void doImport(SnapshotMetadata<Object, Object>[] configurations) {
			if (!ObjectUtils.isEmpty(configurations)) {
				for (SnapshotMetadata<Object, Object> configuration : configurations) {
					if (configuration.isDirectory()) {
						load(configuration.getLocation(), configuration.getFormat());
					}
					else {
						load(configuration.getFormat(), createOptions(configuration.getFilter()),
							configuration.getLocation());
					}
				}
			}
		}

		@Override
		public void load(File directory, SnapshotFormat format) {
			try {
				getSnapshotService().load(directory, format);
			}
			catch (Throwable t) {
				throw new RuntimeException(String.format(
					"Failed to load snapshots from directory (%1$s) in format (%2$s)", directory, format), t);
			}
		}

		@Override
		public void load(SnapshotFormat format, SnapshotOptions<Object, Object> options,
				File... snapshots) {
			try {
				getSnapshotService().load(snapshots, format, options);
			}
			catch (Throwable t) {
				throw new RuntimeException(String.format(
					"Failed to load snapshots (%1$s) in format (%2$s) with options (%3$s)",
						Arrays.toString(snapshots), format, options), t);
			}
		}

		@Override
		public void save(File directory, SnapshotFormat format) {
			try {
				getSnapshotService().save(directory, format);
			}
			catch (Throwable t) {
				throw new RuntimeException(String.format(
					"Failed to save snapshots to directory (%1$s) using format (%2$s)", directory, format), t);
			}
		}

		@Override
		public void save(File directory, SnapshotFormat format,
				SnapshotOptions<Object, Object> options) {
			try {
				getSnapshotService().save(directory, format, options);
			}
			catch (Throwable t) {
				throw new RuntimeException(String.format(
					"Failed to save snapshots to directory (%1$s) using format (%2$s) with options (%3$s)",
						directory, format, options), t);
			}
		}
	}

	protected class RegionSnapshotServiceAdapter implements SnapshotServiceAdapter<K, V> {

		private final RegionSnapshotService<K, V> snapshotService;

		public RegionSnapshotServiceAdapter(RegionSnapshotService<K, V> snapshotService) {
			Assert.notNull(snapshotService, "The backing RegionSnapshotService must not be null");
			this.snapshotService = snapshotService;
		}

		protected RegionSnapshotService<K, V> getSnapshotService() {
			return snapshotService;
		}

		@Override
		public SnapshotOptions<K, V> createOptions() {
			return getSnapshotService().createOptions();
		}

		protected SnapshotOptions<K, V> createOptions(SnapshotFilter<K, V> filter) {
			return createOptions().setFilter(filter);
		}

		@Override
		public void doExport(final SnapshotMetadata<K, V>[] configurations) {
			if (!ObjectUtils.isEmpty(configurations)) {
				for (SnapshotMetadata<K, V> configuration : configurations) {
					Assert.isTrue(configuration.isFile(), String.format("The export location (%1$s) must be a file",
						configuration.getLocation()));
					save(configuration.getLocation(), configuration.getFormat(), createOptions(configuration.getFilter()));
				}
			}
		}

		@Override
		public void doImport(final SnapshotMetadata<K, V>[] configurations) {
			if (!ObjectUtils.isEmpty(configurations)) {
				for (SnapshotMetadata<K, V> configuration : configurations) {
					Assert.isTrue(configuration.isFile(), String.format("The import location (%1$s) must be a file",
						configuration.getLocation()));
					load(configuration.getFormat(), createOptions(configuration.getFilter()), configuration.getLocation());
				}
			}
		}

		@Override
		public void load(File snapshot, SnapshotFormat format) {
			try {
				getSnapshotService().load(snapshot, format);
			}
			catch (Throwable t) {
				throw new RuntimeException(String.format(
					"Failed to load snapshot from file (%1$s) in format (%2$s)", snapshot, format), t);
			}
		}

		@Override
		public void load(SnapshotFormat format, SnapshotOptions<K, V> options, File... snapshots) {
			try {
				for (File snapshot : snapshots) {
					getSnapshotService().load(snapshot, format, options);
				}
			}
			catch (Throwable t) {
				throw new RuntimeException(String.format(
					"Failed to load snapshots (%1$s) in format (%2$s) with options (%3$s)",
						Arrays.toString(snapshots), format, options), t);
			}
		}

		@Override
		public void save(File snapshot, SnapshotFormat format) {
			try {
				getSnapshotService().save(snapshot, format);
			}
			catch (Throwable t) {
				throw new RuntimeException(String.format(
					"Failed to save snapshots to directory (%1$s) using format (%2$s)", snapshot, format), t);
			}
		}

		@Override
		public void save(File snapshot, SnapshotFormat format, SnapshotOptions<K, V> options) {
			try {
				getSnapshotService().save(snapshot, format, options);
			}
			catch (Throwable t) {
				throw new RuntimeException(String.format(
					"Failed to save snapshots to directory (%1$s) using format (%2$s) with options (%3$s)",
						snapshot, format, options), t);
			}
		}
	}

	public static class SnapshotMetadata<K, V> {

		private final File location;

		private final SnapshotFilter<K, V> filter;

		private final SnapshotFormat format;

		public SnapshotMetadata(File location, SnapshotFilter<K, V> filter, SnapshotFormat format) {
			Assert.isTrue(location != null && location.exists(), String.format(
				"The File location (%1$s) must exist", location));

			this.location = location;
			this.filter = filter;
			this.format = format;
		}

		public boolean isDirectory() {
			return getLocation().isDirectory();
		}

		public boolean isFile() {
			return getLocation().isFile();
		}

		public File getLocation() {
			return location;
		}

		public boolean isFilterPresent() {
			return (getFilter() != null);
		}

		public SnapshotFilter<K, V> getFilter() {
			return filter;
		}

		public SnapshotFormat getFormat() {
			return (format != null ? format : SnapshotFormat.GEMFIRE);
		}

		@Override
		public String toString() {
			return String.format("{ @type = %1$s, location = %2$s, filter = %2$s, format = %4$s }",
				getClass().getName(), getLocation().getAbsolutePath(), getFilter(), getFormat());
		}
	}

}
