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

package org.springframework.data.gemfire.snapshot;

import static java.util.Arrays.stream;
import static org.apache.geode.cache.snapshot.SnapshotOptions.SnapshotFormat;
import static org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotServiceAdapter;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.io.Closeable;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.snapshot.CacheSnapshotService;
import org.apache.geode.cache.snapshot.RegionSnapshotService;
import org.apache.geode.cache.snapshot.SnapshotFilter;
import org.apache.geode.cache.snapshot.SnapshotOptions;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationListener;
import org.springframework.data.gemfire.snapshot.event.ExportSnapshotApplicationEvent;
import org.springframework.data.gemfire.snapshot.event.SnapshotApplicationEvent;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.FileCopyUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

/**
 * The SnapshotServiceFactoryBean class is a Spring FactoryBean used to configure and create an instance
 * of an appropriate GemFire Snapshot Service to perform data import and exports.  A CacheSnapshotService is created
 * if the Region is not specified, otherwise a RegionSnapshotService is used based on the configured Region.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotServiceAdapter
 * @see org.apache.geode.cache.snapshot.CacheSnapshotService
 * @see org.apache.geode.cache.snapshot.RegionSnapshotService
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class SnapshotServiceFactoryBean<K, V> extends AbstractFactoryBeanSupport<SnapshotServiceAdapter<K, V>>
		implements InitializingBean, DisposableBean, ApplicationListener<SnapshotApplicationEvent<K, V>> {

	protected static final SnapshotMetadata[] EMPTY_ARRAY = new SnapshotMetadata[0];

	private Boolean suppressImportOnInit;

	private Cache cache;

	private Region<K, V> region;

	private SnapshotMetadata<K, V>[] exports;
	private SnapshotMetadata<K, V>[] imports;

	private SnapshotServiceAdapter<K, V> snapshotServiceAdapter;

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	static <K, V> SnapshotMetadata<K, V>[] nullSafeArray(SnapshotMetadata<K, V>[] configurations) {
		return (configurations != null ? configurations : EMPTY_ARRAY);
	}

	/* (non-Javadoc) */
	static boolean nullSafeIsDirectory(File file) {
		return (file != null && file.isDirectory());
	}

	/* (non-Javadoc) */
	static boolean nullSafeIsFile(File file) {
		return (file != null && file.isFile());
	}

	/**
	 * Constructs and initializes the GemFire Snapshot Service used to take a snapshot of the configured Cache
	 * or Region if initialized.  In addition, this initialization method will perform the actual import.
	 *
	 * @throws Exception if the construction and initialization of the GemFire Snapshot Service fails.
	 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 * @see #getSuppressImportOnInit()
	 * @see #getImports()
	 * @see #create()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void afterPropertiesSet() throws Exception {

		this.snapshotServiceAdapter = create();

		if (!getSuppressImportOnInit()) {
			this.snapshotServiceAdapter.doImport(getImports());
		}
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

		return Optional.ofNullable(getRegion())
			.<SnapshotServiceAdapter>map(region -> wrap(region.getSnapshotService()))
			.orElseGet(() -> wrap(getCache().getSnapshotService()));
	}

	/**
	 * Wraps the GemFire CacheSnapshotService into an appropriate Adapter to uniformly access snapshot operations
	 * on the Cache and Regions alike.
	 *
	 * @param cacheSnapshotService the GemFire CacheSnapshotService to wrap.
	 * @return a SnapshotServiceAdapter wrapping the GemFire CacheSnapshotService.
	 * @see SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 * @see SnapshotServiceFactoryBean.CacheSnapshotServiceAdapter
	 * @see org.apache.geode.cache.snapshot.CacheSnapshotService
	 */
	protected SnapshotServiceAdapter<Object, Object> wrap(CacheSnapshotService cacheSnapshotService) {
		return new CacheSnapshotServiceAdapter(cacheSnapshotService);
	}

	/**
	 * Wraps GemFire's RegionSnapshotService into an appropriate Adapter to uniformly access snapshot operations
	 * on the Cache and Regions alike.
	 *
	 * @param regionSnapshotService the GemFire RegionSnapshotService to wrap.
	 * @return a SnapshotServiceAdapter wrapping the GemFire RegionSnapshotService.
	 * @see SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 * @see SnapshotServiceFactoryBean.RegionSnapshotServiceAdapter
	 * @see org.apache.geode.cache.snapshot.RegionSnapshotService
	 */
	protected SnapshotServiceAdapter<K, V> wrap(RegionSnapshotService<K, V> regionSnapshotService) {
		return new RegionSnapshotServiceAdapter<>(regionSnapshotService);
	}

	/**
	 * Sets a reference to the GemFire Cache for which the snapshot will be taken.
	 *
	 * @param cache the GemFire Cache used to create an instance of CacheSnapshotService.
	 * @throws IllegalArgumentException if the Cache reference is null.
	 * @see org.apache.geode.cache.Cache
	 * @see #getCache()
	 */
	public void setCache(Cache cache) {
		this.cache = Optional.ofNullable(cache)
			.orElseThrow(() -> newIllegalArgumentException("The GemFire Cache must not be null"));
	}

	/**
	 * Gets a reference to the GemFire Cache for which the snapshot will be taken.
	 *
	 * @return the GemFire Cache used to create an instance of CacheSnapshotService.
	 * @throws IllegalStateException if the Cache argument is null.
	 * @see org.apache.geode.cache.Cache
	 * @see #setCache(Cache)
	 */
	protected Cache getCache() {
		return Optional.ofNullable(this.cache)
			.orElseThrow(() -> newIllegalStateException("The GemFire Cache was not properly initialized"));
	}

	/**
	 * Sets the meta-data (location, filter and format) used to create a snapshot from the Cache or Region data.
	 *
	 * @param exports an array of snapshot meta-data used for each export.
	 * @see SnapshotServiceFactoryBean.SnapshotMetadata
	 */
	public void setExports(SnapshotMetadata<K, V>[] exports) {
		this.exports = exports;
	}

	/**
	 * Sets the meta-data (location, filter and format) used to create a snapshot from the Cache or Region data.
	 *
	 * @return an array of snapshot meta-data used for each export.
	 * @see SnapshotServiceFactoryBean.SnapshotMetadata
	 */
	protected SnapshotMetadata<K, V>[] getExports() {
		return nullSafeArray(exports);
	}

	/**
	 * Sets the meta-data (location, filter and format) used to read a data snapshot into an entire Cache
	 * or individual Region.
	 *
	 * @param imports an array of snapshot meta-data used for each import.
	 * @see SnapshotServiceFactoryBean.SnapshotMetadata
	 */
	public void setImports(SnapshotMetadata<K, V>[] imports) {
		this.imports = imports;
	}

	/**
	 * Gets the meta-data (location, filter and format) used to read a data snapshot into an entire Cache
	 * or individual Region.
	 *
	 * @return an array of snapshot meta-data used for each import.
	 * @see SnapshotServiceFactoryBean.SnapshotMetadata
	 */
	protected SnapshotMetadata<K, V>[] getImports() {
		return nullSafeArray(imports);
	}

	/**
	 * Sets a reference to the GemFire Region for which the snapshot will be taken.
	 *
	 * @param region the GemFire Region used to create an instance of the RegionSnapshotService.
	 * @see org.apache.geode.cache.Region
	 * @see #getRegion()
	 */
	public void setRegion(Region<K, V> region) {
		this.region = region;
	}

	/**
	 * Gets a reference to the GemFire Region for which the snapshot will be taken.
	 *
	 * @return the GemFire Region used to create an instance of the RegionSnapshotService.
	 * @see org.apache.geode.cache.Region
	 * @see #getRegion()
	 */
	protected Region<K, V> getRegion() {
		return region;
	}

	/**
	 * Sets a boolean condition to indicate whether importing on initialization should be suppressed.
	 *
	 * @param suppressImportOnInit a Boolean value to indicate whether importing on initialization should be suppressed.
	 * @see #getSuppressImportOnInit()
	 */
	public void setSuppressImportOnInit(Boolean suppressImportOnInit) {
		this.suppressImportOnInit = suppressImportOnInit;
	}

	/**
	 * Determines whether importing on initialization should be suppressed.
	 *
	 * @return a boolean value indicating whether import on initialization should be suppressed.
	 * @see #setSuppressImportOnInit(Boolean)
	 * @see #afterPropertiesSet()
	 */
	protected boolean getSuppressImportOnInit() {
		return Boolean.TRUE.equals(suppressImportOnInit);
	}

	/**
	 * Gets the reference to the GemFire Snapshot Service created by this FactoryBean.
	 *
	 * @return the GemFire Snapshot Service created by this FactoryBean.
	 * @throws Exception if the GemFire Snapshot Service failed to be created.
	 * @see SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 */
	@Override
	public SnapshotServiceAdapter<K, V> getObject() throws Exception {
		return this.snapshotServiceAdapter;
	}

	/**
	 * Gets the type of Snapshot Service created by this FactoryBean.
	 *
	 * @return a Class object representing the type of Snapshot Service created by this FactoryBean.
	 * @see SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 * @see SnapshotServiceFactoryBean.CacheSnapshotServiceAdapter
	 * @see SnapshotServiceFactoryBean.RegionSnapshotServiceAdapter
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Class<?> getObjectType() {
		return Optional.ofNullable(this.snapshotServiceAdapter).map(Object::getClass)
			.orElse((Class) SnapshotServiceAdapter.class);
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
	 * Performs an export of the GemFire Cache or Region if configured.
	 *
	 * @throws Exception if the Cache/Region data export operation fails.
	 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 * @see #getExports()
	 * @see #getObject()
	 */
	@Override
	@SuppressWarnings("all")
	public void destroy() throws Exception {
		getObject().doExport(getExports());
	}

	/**
	 * Listens for SnapshotApplicationEvents triggering a GemFire Cache-wide or Region data snapshot import/export
	 * when details of the event match the criteria of this factory's constructed GemFire SnapshotService.
	 *
	 * @param event the SnapshotApplicationEvent triggering a GemFire Cache or Region data import/export.
	 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 * @see org.springframework.data.gemfire.snapshot.event.ExportSnapshotApplicationEvent
	 * @see org.springframework.data.gemfire.snapshot.event.ImportSnapshotApplicationEvent
	 * @see org.springframework.data.gemfire.snapshot.event.SnapshotApplicationEvent
	 * @see #isMatch(SnapshotApplicationEvent)
	 * @see #resolveSnapshotMetadata(SnapshotApplicationEvent)
	 * @see #getObject()
	 */
	@Override
	@SuppressWarnings("all")
	public void onApplicationEvent(SnapshotApplicationEvent<K, V> event) {

		try {
			if (isMatch(event)) {
				if (event instanceof ExportSnapshotApplicationEvent) {
					getObject().doExport(resolveSnapshotMetadata(event));
				}
				else {
					getObject().doImport(resolveSnapshotMetadata(event));
				}
			}
		}
		catch (Exception ignore) {
		}
	}

	/**
	 * Determines whether the details of the given SnapshotApplicationEvent match the criteria of this factory
	 * to trigger a GemFire Cache or Region data export.
	 *
	 * @param event the SnapshotApplicationEvent containing details of the application requested data export.
	 * @return a boolean value indicating whether the application requested snapshot event details match
	 * the criteria required by this factory to trigger a GemFire Cache or Region data export.
	 * @see SnapshotApplicationEvent
	 */
	protected boolean isMatch(SnapshotApplicationEvent event) {
		return (event.isCacheSnapshotEvent() || event.matches(getRegion()));
	}

	/**
	 * Resolves the SnapshotMetadata used to perform the GemFire Cache or Region data snapshot import/export.
	 * If the event contains specific SnapshotMetadata, then this is preferred over the factory's own
	 * "import" or "export" SnapshotMetadata.
	 *
	 * @param event the SnapshotApplicationEvent from which to resolve the SnapshotMetadata.
	 * @return the resolved SnapshotMetadata, either from the event or this factory's configured imports/exports.
	 * @see SnapshotApplicationEvent#getSnapshotMetadata()
	 * @see #getExports()
	 * @see #getImports()
	 */
	protected SnapshotMetadata<K, V>[] resolveSnapshotMetadata(SnapshotApplicationEvent<K, V> event) {

		SnapshotMetadata<K, V>[] eventSnapshotMetadata = event.getSnapshotMetadata();

		return (!ObjectUtils.isEmpty(eventSnapshotMetadata) ? eventSnapshotMetadata
			: (event instanceof ExportSnapshotApplicationEvent ? getExports() : getImports()));
	}

	/**
	 * The SnapshotServiceAdapter interface is an Adapter adapting both GemFire CacheSnapshotService
	 * and RegionSnapshotService to treat them uniformly.
	 *
	 * @param <K> the class type of the Region key.
	 * @param <V> the class type of the Region value.
	 */
	public interface SnapshotServiceAdapter<K, V> {

		SnapshotOptions<K, V> createOptions();

		@SuppressWarnings("unchecked")
		void doExport(SnapshotMetadata<K, V>... configurations);

		@SuppressWarnings("unchecked")
		void doImport(SnapshotMetadata<K, V>... configurations);

		void load(File directory, SnapshotFormat format);

		void load(SnapshotFormat format, SnapshotOptions<K, V> options, File... snapshots);

		void save(File location, SnapshotFormat format);

		void save(File location, SnapshotFormat format, SnapshotOptions<K, V> options);

	}

	/**
	 * SnapshotServiceAdapterSupport is an abstract base class for all SnapshotServiceAdapter implementations
	 * encapsulating common reusable functionality.
	 *
	 * @param <K> the class type of the Cache Region key.
	 * @param <V> the class type of the Cache Region value.
	 * @see SnapshotServiceFactoryBean.SnapshotServiceAdapter
	 */
	protected static abstract class SnapshotServiceAdapterSupport<K, V> implements SnapshotServiceAdapter<K, V> {

		protected static final File TEMPORARY_DIRECTORY = new File(System.getProperty("java.io.tmpdir"));

		protected final Log log = createLog();

		Log createLog() {
			return LogFactory.getLog(getClass());
		}

		@Override
		public SnapshotOptions<K, V> createOptions() {
			throw new UnsupportedOperationException("not implemented");
		}

		protected SnapshotOptions<K, V> createOptions(SnapshotMetadata<K, V> metadata) {
			return createOptions()
				.setFilter(metadata.getFilter())
				.setParallelMode(metadata.isParallel());
		}

		@Override
		@SuppressWarnings("unchecked")
		public void doExport(SnapshotMetadata<K, V>... configurations) {

			stream(nullSafeArray(configurations)).forEach(configuration ->
				save(configuration.getLocation(), configuration.getFormat(), createOptions(configuration)));
		}

		@Override
		@SuppressWarnings("unchecked")
		public void doImport(SnapshotMetadata<K, V>... configurations) {

			stream(nullSafeArray(configurations)).forEach(configuration ->
				load(configuration.getFormat(), createOptions(configuration), handleLocation(configuration)));
		}

		protected abstract File[] handleLocation(SnapshotMetadata<K, V> configuration);

		protected File[] handleDirectoryLocation(File directory) {
			return directory.listFiles(pathname -> nullSafeIsFile(pathname));
		}

		protected File[] handleFileLocation(File file) {

			if (ArchiveFileFilter.INSTANCE.accept(file)) {
				try {

					File extractedArchiveDirectory =
						new File(TEMPORARY_DIRECTORY, file.getName().replaceAll("\\.", "-"));

					Assert.state(extractedArchiveDirectory.isDirectory() || extractedArchiveDirectory.mkdirs(),
						String.format("Failed create directory (%1$s) in which to extract archive (%2$s)",
							extractedArchiveDirectory, file));

					ZipFile zipFile = (ArchiveFileFilter.INSTANCE.isJarFile(file)
						? new JarFile(file, false, JarFile.OPEN_READ)
							: new ZipFile(file, ZipFile.OPEN_READ));

					for (ZipEntry entry : CollectionUtils.iterable(zipFile.entries())) {
						if (!entry.isDirectory()) {

							DataInputStream entryInputStream = new DataInputStream(zipFile.getInputStream(entry));

							DataOutputStream entryOutputStream = new DataOutputStream(new FileOutputStream(
								new File(extractedArchiveDirectory, toSimpleFilename(entry.getName()))));

							try {
								FileCopyUtils.copy(entryInputStream, entryOutputStream);
							}
							finally {
								exceptionSuppressingClose(entryInputStream);
								exceptionSuppressingClose(entryOutputStream);
							}
						}
					}

					return handleDirectoryLocation(extractedArchiveDirectory);
				}
				catch (Throwable cause) {
					throw new ImportSnapshotException(
						String.format("Failed to extract archive [%1$s] to import", file), cause);
				}
			}

			return new File[] { file };
		}

		protected boolean exceptionSuppressingClose(Closeable closeable) {

			try {
				closeable.close();
				return true;
			}
			catch (IOException ignore) {
				logDebug(ignore, "Failed to close [%s]", closeable);
				return false;
			}
		}

		protected void logDebug(Throwable t, String message, Object... arguments) {
			if (log.isDebugEnabled()) {
				log.debug(String.format(message, arguments), t);
			}
		}

		@Override
		public void load(File directory, SnapshotFormat format) {
			throw new UnsupportedOperationException("not implemented");
		}

		@Override
		public void load(SnapshotFormat format, SnapshotOptions<K, V> options, File... snapshots) {
			throw new UnsupportedOperationException("not implemented");
		}

		@Override
		public void save(File location, SnapshotFormat format) {
			throw new UnsupportedOperationException("not implemented");
		}

		@Override
		public void save(File location, SnapshotFormat format, SnapshotOptions<K, V> options) {
			throw new UnsupportedOperationException("not implemented");
		}

		protected String toSimpleFilename(String pathname) {
			int pathSeparatorIndex = String.valueOf(pathname).lastIndexOf(File.separator);
			pathname = (pathSeparatorIndex > -1 ? pathname.substring(pathSeparatorIndex + 1) : pathname);
			return StringUtils.trimWhitespace(pathname);
		}
	}

	/**
	 * The CacheSnapshotServiceAdapter is a SnapshotServiceAdapter adapting GemFire's CacheSnapshotService.
	 *
	 * @see SnapshotServiceFactoryBean.SnapshotServiceAdapterSupport
	 */
	protected static class CacheSnapshotServiceAdapter extends SnapshotServiceAdapterSupport<Object, Object> {

		private final CacheSnapshotService snapshotService;

		public CacheSnapshotServiceAdapter(CacheSnapshotService snapshotService) {
			Assert.notNull(snapshotService, "The backing CacheSnapshotService must not be null");
			this.snapshotService = snapshotService;
		}

		protected CacheSnapshotService getSnapshotService() {
			return this.snapshotService;
		}

		@Override
		public SnapshotOptions<Object, Object> createOptions() {
			return getSnapshotService().createOptions();
		}

		@Override
		protected File[] handleLocation(SnapshotMetadata<Object, Object> configuration) {

			return (configuration.isFile()
				? handleFileLocation(configuration.getLocation())
				: handleDirectoryLocation(configuration.getLocation()));
		}

		@Override
		public void load(File directory, SnapshotFormat format) {

			try {
				getSnapshotService().load(directory, format);
			}
			catch (Throwable cause) {
				throw new ImportSnapshotException(String.format(
					"Failed to load snapshots from directory [%1$s] in format [%2$s]",
						directory, format), cause);
			}
		}

		@Override
		public void load(SnapshotFormat format, SnapshotOptions<Object, Object> options, File... snapshots) {

			try {
				getSnapshotService().load(snapshots, format, options);
			}
			catch (Throwable cause) {
				throw new ImportSnapshotException(String.format(
					"Failed to load snapshots [%1$s] in format [%2$s] using options [%3$s]",
						Arrays.toString(snapshots), format, options), cause);
			}
		}

		@Override
		public void save(File directory, SnapshotFormat format) {

			try {
				getSnapshotService().save(directory, format);
			}
			catch (Throwable cause) {
				throw new ExportSnapshotException(String.format(
					"Failed to save snapshots to directory [%1$s] in format [%2$s]",
						directory, format), cause);
			}
		}

		@Override
		public void save(File directory, SnapshotFormat format, SnapshotOptions<Object, Object> options) {

			try {
				getSnapshotService().save(directory, format, options);
			}
			catch (Throwable cause) {
				throw new ExportSnapshotException(String.format(
					"Failed to save snapshots to directory [%1$s] in format [%2$s] using options [%3$s]",
						directory, format, options), cause);
			}
		}
	}

	/**
	 * The RegionSnapshotServiceAdapter is a SnapshotServiceAdapter adapting GemFire's RegionSnapshotService.
	 *
	 * @see SnapshotServiceFactoryBean.SnapshotServiceAdapterSupport
	 */
	protected static class RegionSnapshotServiceAdapter<K, V> extends SnapshotServiceAdapterSupport<K, V> {

		private final RegionSnapshotService<K, V> snapshotService;

		public RegionSnapshotServiceAdapter(RegionSnapshotService<K, V> snapshotService) {
			Assert.notNull(snapshotService, "The backing RegionSnapshotService must not be null");
			this.snapshotService = snapshotService;
		}

		protected RegionSnapshotService<K, V> getSnapshotService() {
			return this.snapshotService;
		}

		@Override
		public SnapshotOptions<K, V> createOptions() {
			return getSnapshotService().createOptions();
		}

		@Override
		protected File[] handleLocation(SnapshotMetadata<K, V> configuration) {
			return new File[] { configuration.getLocation() };
		}

		@Override
		public void load(File snapshot, SnapshotFormat format) {

			try {
				getSnapshotService().load(snapshot, format);
			}
			catch (Throwable cause) {
				throw new ImportSnapshotException(String.format(
					"Failed to load snapshot from file [%1$s] in format [%2$s]",
						snapshot, format), cause);
			}
		}

		@Override
		public void load(SnapshotFormat format, SnapshotOptions<K, V> options, File... snapshots) {

			try {
				for (File snapshot : snapshots) {
					getSnapshotService().load(snapshot, format, options);
				}
			}
			catch (Throwable cause) {
				throw new ImportSnapshotException(String.format(
					"Failed to load snapshots [%1$s] in format [%2$s] using options [%3$s]",
						Arrays.toString(snapshots), format, options), cause);
			}
		}

		@Override
		public void save(File snapshot, SnapshotFormat format) {

			try {
				getSnapshotService().save(snapshot, format);
			}
			catch (Throwable cause) {
				throw new ExportSnapshotException(String.format(
					"Failed to save snapshot to file [%1$s] in format [%2$s]",
						snapshot, format), cause);
			}
		}

		@Override
		public void save(File snapshot, SnapshotFormat format, SnapshotOptions<K, V> options) {

			try {
				getSnapshotService().save(snapshot, format, options);
			}
			catch (Throwable cause) {
				throw new ExportSnapshotException(String.format(
					"Failed to save snapshot to file [%1$s] in format [%2$s] using options [%3$s]",
						snapshot, format, options), cause);
			}
		}
	}

	/**
	 * The SnapshotMetadata class encapsulates details of the GemFire Cache or Region data snapshot
	 * on either import or export.
	 *
	 * @param <K> the class type of the Region key.
	 * @param <V> the class type of the Region value.
	 */
	public static class SnapshotMetadata<K, V> {

		protected static final boolean DEFAULT_PARALLEL = false;

		protected static final SnapshotFormat DEFAULT_SNAPSHOT_FORMAT = SnapshotFormat.GEMFIRE;

		private boolean parallel;

		private final File location;

		private final SnapshotFilter<K, V> filter;

		private final SnapshotFormat format;

		public SnapshotMetadata(File location) {
			this(location, DEFAULT_SNAPSHOT_FORMAT, null);
		}

		public SnapshotMetadata(File location, SnapshotFormat format) {
			this(location, format, null);
		}

		public SnapshotMetadata(File location, SnapshotFormat format, SnapshotFilter<K, V> filter) {

			Assert.notNull(location, "Location is required");

			this.location = location;
			this.format = format;
			this.filter = filter;
		}

		public boolean isDirectory() {
			return nullSafeIsDirectory(getLocation());
		}

		public boolean isFile() {
			return nullSafeIsFile(getLocation());
		}

		public File getLocation() {
			return this.location;
		}

		public SnapshotFormat getFormat() {
			return Optional.ofNullable(this.format).orElse(DEFAULT_SNAPSHOT_FORMAT);
		}

		public boolean isFilterPresent() {
			return (getFilter() != null);
		}

		public SnapshotFilter<K, V> getFilter() {
			return this.filter;
		}

		public void setParallel(boolean parallel) {
			this.parallel = parallel;
		}

		public boolean isParallel() {
			return this.parallel;
		}

		@Override
		public String toString() {
			return String.format("{ @type = %1$s, location = %2$s, format = %3$s, filter = %4$s, parallel = %5$s }",
				getClass().getName(), getLocation().getAbsolutePath(), getFormat(), getFilter(), isParallel());
		}
	}

	/**
	 * The ArchiveFileFilter class is a Java FileFilter implementation accepting any File that is either
	 * a JAR file or ZIP file.
	 *
	 * @see java.io.File
	 * @see java.io.FileFilter
	 */
	protected static final class ArchiveFileFilter implements FileFilter {

		protected static final ArchiveFileFilter INSTANCE = new ArchiveFileFilter();

		protected static final List<String> ACCEPTED_FILE_EXTENSIONS = Arrays.asList("jar", "zip");

		protected static final String FILE_EXTENSION_DOT_SEPARATOR = ".";

		protected boolean isJarFile(File file) {
			return "jar".equalsIgnoreCase(getFileExtension(file));
		}

		protected String getFileExtension(File file) {
			String fileExtension = "";

			if (nullSafeIsFile(file)) {
				String pathname = file.getAbsolutePath();
				int fileExtensionIndex = pathname.lastIndexOf(FILE_EXTENSION_DOT_SEPARATOR);
				fileExtension = (fileExtensionIndex > -1 ? pathname.substring(fileExtensionIndex + 1) : "");
			}

			return fileExtension.toLowerCase();
		}

		@Override
		public boolean accept(final File pathname) {
			return ACCEPTED_FILE_EXTENSIONS.contains(getFileExtension(pathname));
		}
	}
}
