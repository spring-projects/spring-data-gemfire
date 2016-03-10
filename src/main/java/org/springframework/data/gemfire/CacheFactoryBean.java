/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.core.io.Resource;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.support.PersistenceExceptionTranslator;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;

import com.gemstone.gemfire.GemFireCheckedException;
import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.DynamicRegionFactory;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.TransactionListener;
import com.gemstone.gemfire.cache.TransactionWriter;
import com.gemstone.gemfire.cache.util.GatewayConflictResolver;
import com.gemstone.gemfire.distributed.DistributedMember;
import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.internal.datasource.ConfigProperty;
import com.gemstone.gemfire.internal.jndi.JNDIInvoker;
import com.gemstone.gemfire.pdx.PdxSerializable;
import com.gemstone.gemfire.pdx.PdxSerializer;

/**
 * FactoryBean used to configure a GemFire peer Cache node. Allows either retrieval of an existing, opened Cache
 * or the creation of a new Cache instance.
 * <p>
 * This class implements the {@link org.springframework.dao.support.PersistenceExceptionTranslator}
 * interface, as auto-detected by Spring's
 * {@link org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor}, for AOP-based translation
 * of native Exceptions to Spring DataAccessExceptions. Hence, the presence of this class automatically enables
 * a PersistenceExceptionTranslationPostProcessor to translate GemFire Exceptions appropriately.
 * 
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.BeanClassLoaderAware
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.dao.support.PersistenceExceptionTranslator
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.CacheFactory
 * @see com.gemstone.gemfire.cache.DynamicRegionFactory
 * @see com.gemstone.gemfire.cache.GemFireCache
 * @see com.gemstone.gemfire.distributed.DistributedMember
 * @see com.gemstone.gemfire.distributed.DistributedSystem
 */
@SuppressWarnings("unused")
public class CacheFactoryBean implements BeanClassLoaderAware, BeanFactoryAware, BeanNameAware, FactoryBean<Cache>,
		InitializingBean, DisposableBean, PersistenceExceptionTranslator {

	protected boolean close = true;
	protected boolean lazyInitialize = true;
	protected boolean useBeanFactoryLocator = false;

	protected final Log log = LogFactory.getLog(getClass());

	protected BeanFactory beanFactory;

	protected Boolean copyOnRead;
	protected Boolean enableAutoReconnect;
	protected Boolean pdxIgnoreUnreadFields;
	protected Boolean pdxPersistent;
	protected Boolean pdxReadSerialized;
	protected Boolean useClusterConfiguration;

	protected Cache cache;

	protected ClassLoader beanClassLoader;

	protected DynamicRegionSupport dynamicRegionSupport;

	protected Float criticalHeapPercentage;
	protected Float evictionHeapPercentage;

	protected GemfireBeanFactoryLocator beanFactoryLocator;

	protected Integer lockLease;
	protected Integer lockTimeout;
	protected Integer messageSyncInterval;
	protected Integer searchTimeout;

	protected List<JndiDataSource> jndiDataSources;

	protected List<TransactionListener> transactionListeners;

	// Declared with type 'Object' for backward compatibility
	protected Object gatewayConflictResolver;
	protected Object pdxSerializer;

	protected Properties properties;

	protected Resource cacheXml;

	protected String beanName;
	private String cacheResolutionMessagePrefix;
	protected String pdxDiskStoreName;

	protected TransactionWriter transactionWriter;

	public static class DynamicRegionSupport {

		private Boolean persistent = Boolean.TRUE;
		private Boolean registerInterest = Boolean.TRUE;

		private String diskDirectory;
		private String poolName;

		public String getDiskDir() {
			return diskDirectory;
		}

		public void setDiskDir(String diskDirectory) {
			this.diskDirectory = diskDirectory;
		}

		public Boolean getPersistent() {
			return persistent;
		}

		public void setPersistent(Boolean persistent) {
			this.persistent = persistent;
		}

		public String getPoolName() {
			return poolName;
		}

		public void setPoolName(String poolName) {
			this.poolName = poolName;
		}

		public Boolean getRegisterInterest() {
			return registerInterest;
		}

		public void setRegisterInterest(Boolean registerInterest) {
			this.registerInterest = registerInterest;
		}

		public void initializeDynamicRegionFactory() {
			File localDiskDirectory = (this.diskDirectory == null ? null : new File(this.diskDirectory));

			DynamicRegionFactory.Config config = new DynamicRegionFactory.Config(localDiskDirectory, poolName,
				persistent, registerInterest);

			DynamicRegionFactory.get().open(config);
		}
	}

	public static class JndiDataSource {

		private List<ConfigProperty> props;
		private Map<String, String> attributes;

		public Map<String, String> getAttributes() {
			return attributes;
		}

		public void setAttributes(Map<String, String> attributes) {
			this.attributes = attributes;
		}

		public List<ConfigProperty> getProps() {
			return props;
		}

		public void setProps(List<ConfigProperty> props) {
			this.props = props;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.InitializingBean#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		postProcessPropertiesBeforeInitialization(getProperties());

		if (!isLazyInitialize()) {
			init();
		}
	}

	/* (non-Javadoc) */
	protected void postProcessPropertiesBeforeInitialization(Properties gemfireProperties) {
		if (GemfireUtils.isGemfireVersion8OrAbove()) {
			gemfireProperties.setProperty("disable-auto-reconnect", String.valueOf(
				!Boolean.TRUE.equals(getEnableAutoReconnect())));
			gemfireProperties.setProperty("use-cluster-configuration", String.valueOf(
				Boolean.TRUE.equals(getUseClusterConfiguration())));
		}
	}

	/* (non-Javadoc) */
	private Cache init() throws Exception {
		initBeanFactoryLocator();

		final ClassLoader currentThreadContextClassLoader = Thread.currentThread().getContextClassLoader();

		try {
			// use bean ClassLoader to load Spring configured, GemFire Declarable classes
			Thread.currentThread().setContextClassLoader(beanClassLoader);

			this.cache = postProcess(resolveCache());

			DistributedSystem system = cache.getDistributedSystem();

			DistributedMember member = system.getDistributedMember();

			log.info(String.format("Connected to Distributed System [%1$s] as Member [%2$s]"
				.concat("in Group(s) [%3$s] with Role(s) [%4$s] on Host [%5$s] having PID [%6$d]."),
					system.getName(), member.getId(), member.getGroups(), member.getRoles(), member.getHost(),
						member.getProcessId()));

			log.info(String.format("%1$s GemFire v.%2$s Cache [%3$s].", cacheResolutionMessagePrefix,
				CacheFactory.getVersion(), cache.getName()));

			return cache;
		}
		finally {
			Thread.currentThread().setContextClassLoader(currentThreadContextClassLoader);
		}
	}

	/* (non-Javadoc) */
	private void initBeanFactoryLocator() {
		if (useBeanFactoryLocator && beanFactoryLocator == null) {
			beanFactoryLocator = new GemfireBeanFactoryLocator();
			beanFactoryLocator.setBeanFactory(beanFactory);
			beanFactoryLocator.setBeanName(beanName);
			beanFactoryLocator.afterPropertiesSet();
		}
	}

	/**
	 * If Dynamic Regions are enabled, create and initialize a DynamicRegionFactory before creating the Cache.
	 */
	private void initDynamicRegionFactory() {
		if (dynamicRegionSupport != null) {
			dynamicRegionSupport.initializeDynamicRegionFactory();
		}
	}

	/**
	 * Resolves the GemFire Cache by first attempting to lookup and find an existing Cache instance in the VM;
	 * if an existing Cache could not be found, then this method proceeds in attempting to create a new Cache instance.
	 *
	 * @return the resolved GemFire Cache instance.
	 * @see com.gemstone.gemfire.cache.Cache
	 * @see #fetchCache()
	 * @see #createFactory(java.util.Properties)
	 * @see #prepareFactory(Object)
	 * @see #createCache(Object)
	 */
	protected Cache resolveCache() {
		try {
			cacheResolutionMessagePrefix = "Found existing";
			return (Cache) fetchCache();
		}
		catch (CacheClosedException ex) {
			cacheResolutionMessagePrefix = "Created new";
			initDynamicRegionFactory();
			return (Cache) createCache(prepareFactory(createFactory(resolveProperties())));
		}
	}

	/**
	 * Fetches an existing GemFire Cache instance from the CacheFactory.
	 *
	 * @param <T> parameterized Class type extension of GemFireCache.
	 * @return the existing GemFire Cache instance if available.
	 * @throws com.gemstone.gemfire.cache.CacheClosedException if an existing GemFire Cache instance does not exist.
	 * @see com.gemstone.gemfire.cache.GemFireCache
	 * @see com.gemstone.gemfire.cache.CacheFactory#getAnyInstance()
	 */
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T fetchCache() {
		return (T) (cache != null ? cache : CacheFactory.getAnyInstance());
	}

	/**
	 * Resolves the GemFire System properties used to configure the GemFire Cache instance.
	 *
	 * @return a Properties object containing GemFire System properties used to configure the GemFire Cache instance.
	 * @see #getProperties()
	 */
	protected Properties resolveProperties() {
		return getProperties();
	}

	/**
	 * Creates an instance of GemFire factory initialized with the given GemFire System Properties
	 * to create an instance of a GemFire cache.
	 *
	 * @param gemfireProperties a Properties object containing GemFire System properties.
	 * @return an instance of a GemFire factory used to create a GemFire cache instance.
	 * @see java.util.Properties
	 * @see com.gemstone.gemfire.cache.CacheFactory
	 */
	protected Object createFactory(Properties gemfireProperties) {
		return new CacheFactory(gemfireProperties);
	}

	/**
	 * Initializes the GemFire factory used to create the GemFire cache instance.  Sets PDX options
	 * specified by the user.
	 *
	 * @param factory the GemFire factory used to create an instance of the GemFire cache.
	 * @return the initialized GemFire cache factory.
	 * @see #isPdxOptionsSpecified()
	 */
	protected Object prepareFactory(Object factory) {
		if (isPdxOptionsSpecified()) {
			CacheFactory cacheFactory = (CacheFactory) factory;

			if (pdxSerializer != null) {
				Assert.isAssignable(PdxSerializer.class, pdxSerializer.getClass(), "Invalid pdx serializer used");
				cacheFactory.setPdxSerializer((PdxSerializer) pdxSerializer);
			}
			if (pdxDiskStoreName != null) {
				cacheFactory.setPdxDiskStore(pdxDiskStoreName);
			}
			if (pdxIgnoreUnreadFields != null) {
				cacheFactory.setPdxIgnoreUnreadFields(pdxIgnoreUnreadFields);
			}
			if (pdxPersistent != null) {
				cacheFactory.setPdxPersistent(pdxPersistent);
			}
			if (pdxReadSerialized != null) {
				cacheFactory.setPdxReadSerialized(pdxReadSerialized);
			}
		}

		return factory;
	}

	/**
	 * Determines whether the user specified PDX options.
	 *
	 * @return a boolean value indicating whether the user specified PDX options or not.
	 */
	protected boolean isPdxOptionsSpecified() {
		return (pdxSerializer != null || pdxReadSerialized != null || pdxPersistent != null
			|| pdxIgnoreUnreadFields != null || pdxDiskStoreName != null);
	}

	/**
	 * Creates a new GemFire cache instance using the provided factory.
	 *
	 * @param <T> parameterized Class type extension of GemFireCache.
	 * @param factory the appropriate GemFire factory used to create a cache instance.
	 * @return an instance of the GemFire cache.
	 * @see com.gemstone.gemfire.cache.GemFireCache
	 * @see com.gemstone.gemfire.cache.CacheFactory#create()
	 */
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T createCache(Object factory) {
		return (T) (cache != null ? cache : ((CacheFactory) factory).create());
	}

	/**
	 * Post processes the GemFire Cache instance by loading any cache.xml, applying settings specified in SDG XML
	 * configuration meta-data, and registering the appropriate Transaction Listeners, Writer and JNDI settings.
	 *
	 * @param <T> parameterized Class type extension of GemFireCache.
	 * @param cache the GemFire Cache instance to process.
	 * @return the GemFire Cache instance after processing.
	 * @throws IOException if the cache.xml Resource could not be loaded and applied to the Cache instance.
	 * @see com.gemstone.gemfire.cache.Cache#loadCacheXml(java.io.InputStream)
	 * @see #getCacheXml()
	 * @see #setHeapPercentages(com.gemstone.gemfire.cache.GemFireCache)
	 * @see #registerTransactionListeners(com.gemstone.gemfire.cache.GemFireCache)
	 * @see #registerTransactionWriter(com.gemstone.gemfire.cache.GemFireCache)
	 * @see #registerJndiDataSources()
	 */
	protected <T extends GemFireCache> T postProcess(T cache) throws IOException {
		Resource localCacheXml = getCacheXml();

		// load cache.xml Resource and initialize the Cache
		if (localCacheXml != null) {
			if (log.isDebugEnabled()) {
				log.debug(String.format("initializing Cache with '%1$s'", cacheXml));
			}

			cache.loadCacheXml(localCacheXml.getInputStream());
		}

		if (this.copyOnRead != null) {
			cache.setCopyOnRead(this.copyOnRead);
		}
		if (gatewayConflictResolver != null) {
			((Cache) cache).setGatewayConflictResolver((GatewayConflictResolver) gatewayConflictResolver);
		}
		if (lockLease != null) {
			((Cache) cache).setLockLease(lockLease);
		}
		if (lockTimeout != null) {
			((Cache) cache).setLockTimeout(lockTimeout);
		}
		if (messageSyncInterval != null) {
			((Cache) cache).setMessageSyncInterval(messageSyncInterval);
		}
		if (searchTimeout != null) {
			((Cache) cache).setSearchTimeout(searchTimeout);
		}

		setHeapPercentages(cache);
		registerTransactionListeners(cache);
		registerTransactionWriter(cache);
		registerJndiDataSources();

		return cache;
	}

	/* (non-Javadoc) */
	private void setHeapPercentages(GemFireCache cache) {
		if (criticalHeapPercentage != null) {
			Assert.isTrue(criticalHeapPercentage > 0.0 && criticalHeapPercentage <= 100.0,
				String.format("'criticalHeapPercentage' (%1$s) is invalid; must be > 0.0 and <= 100.0",
					criticalHeapPercentage));
			cache.getResourceManager().setCriticalHeapPercentage(criticalHeapPercentage);
		}

		if (evictionHeapPercentage != null) {
			Assert.isTrue(evictionHeapPercentage > 0.0 && evictionHeapPercentage <= 100.0,
				String.format("'evictionHeapPercentage' (%1$s) is invalid; must be > 0.0 and <= 100.0",
					evictionHeapPercentage));
			cache.getResourceManager().setEvictionHeapPercentage(evictionHeapPercentage);
		}
	}

	/* (non-Javadoc) */
	private void registerTransactionListeners(GemFireCache cache) {
		for (TransactionListener transactionListener : CollectionUtils.nullSafeCollection(transactionListeners)) {
			cache.getCacheTransactionManager().addListener(transactionListener);
		}
	}

	/* (non-Javadoc) */
	private void registerTransactionWriter(GemFireCache cache) {
		if (transactionWriter != null) {
			cache.getCacheTransactionManager().setWriter(transactionWriter);
		}
	}

	/* (non-Javadoc) */
	private void registerJndiDataSources() {
		for (JndiDataSource jndiDataSource : CollectionUtils.nullSafeCollection(jndiDataSources)) {
			String typeAttributeValue = jndiDataSource.getAttributes().get("type");
			JndiDataSourceType jndiDataSourceType = JndiDataSourceType.valueOfIgnoreCase(typeAttributeValue);

			Assert.notNull(jndiDataSourceType, String.format(
				"'jndi-binding' 'type' [%1$s] is invalid; 'type' must be one of %2$s", typeAttributeValue,
					Arrays.toString(JndiDataSourceType.values())));

			jndiDataSource.getAttributes().put("type", jndiDataSourceType.getName());
			JNDIInvoker.mapDatasource(jndiDataSource.getAttributes(), jndiDataSource.getProps());
		}
	}

	@Override
	public void destroy() throws Exception {
		if (close) {
			Cache localCache = fetchCache();

			if (localCache != null && !localCache.isClosed()) {
				close(localCache);
			}

			this.cache = null;

			if (beanFactoryLocator != null) {
				beanFactoryLocator.destroy();
				beanFactoryLocator = null;
			}
		}
	}

	protected void close(GemFireCache cache) {
		cache.close();
	}

	@Override
	public DataAccessException translateExceptionIfPossible(RuntimeException e) {
		if (e instanceof GemFireException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireException) e);
		}

		if (e instanceof IllegalArgumentException) {
			DataAccessException wrapped = GemfireCacheUtils.convertQueryExceptions(e);
			// ignore conversion if the generic exception is returned
			if (!(wrapped instanceof GemfireSystemException)) {
				return wrapped;
			}
		}

		if (e.getCause() instanceof GemFireException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireException) e.getCause());
		}

		if (e.getCause() instanceof GemFireCheckedException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireCheckedException) e.getCause());
		}

		return null;
	}

	@Override
	public void setBeanClassLoader(ClassLoader classLoader) {
		this.beanClassLoader = classLoader;
	}

	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	@Override
	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * Sets the Cache configuration.
	 *
	 * @param cacheXml the cache.xml Resource used to initialize the GemFire Cache.
	 * @see org.springframework.core.io.Resource
	 */
	public void setCacheXml(Resource cacheXml) {
		this.cacheXml = cacheXml;
	}

	/**
	 * Sets the cache properties.
	 * 
	 * @param properties the properties to set
	 */
	public void setProperties(Properties properties) {
		this.properties = properties;
	}

	/**
	 * @param lazyInitialize set to false to force cache initialization if no other bean references it
	 */
	public void setLazyInitialize(boolean lazyInitialize) {
		this.lazyInitialize = lazyInitialize;
	}

	/**
	 * Indicates whether a bean factory locator is enabled (default) for this
	 * cache definition or not. The locator stores the enclosing bean factory
	 * reference to allow auto-wiring of Spring beans into GemFire managed
	 * classes. Usually disabled when the same cache is used in multiple
	 * application context/bean factories inside the same VM.
	 * 
	 * @param usage true if the bean factory locator is used underneath or not
	 */
	public void setUseBeanFactoryLocator(boolean usage) {
		this.useBeanFactoryLocator = usage;
	}

	/**
	 * Set whether the Cache should be closed.
	 *
	 * @param close set to false if destroy() should not close the cache
	 */
	public void setClose(boolean close) {
		this.close = close;
	}

	/**
	 * Set the copyOnRead attribute of the Cache.
	 *
	 * @param copyOnRead a boolean value indicating whether the object stored in the Cache is copied on gets.
	 */
	public void setCopyOnRead(Boolean copyOnRead) {
		this.copyOnRead = copyOnRead;
	}

	/**
	 * Set the Cache's critical heap percentage attribute.
	 *
	 * @param criticalHeapPercentage floating point value indicating the critical heap percentage.
	 */
	public void setCriticalHeapPercentage(Float criticalHeapPercentage) {
		this.criticalHeapPercentage = criticalHeapPercentage;
	}

	/**
	 * Sets an instance of the DynamicRegionSupport to support Dynamic Regions in this GemFire Cache.
	 *
	 * @param dynamicRegionSupport the DynamicRegionSupport class to setup Dynamic Regions in this Cache.
	 */
	public void setDynamicRegionSupport(DynamicRegionSupport dynamicRegionSupport) {
		this.dynamicRegionSupport = dynamicRegionSupport;
	}

	/**
	 * Controls whether auto-reconnect functionality introduced in GemFire 8 is enabled or not.
	 *
	 * @param enableAutoReconnect a boolean value to enable/disable auto-reconnect functionality.
	 * @since GemFire 8.0
	 */
	public void setEnableAutoReconnect(Boolean enableAutoReconnect) {
		this.enableAutoReconnect = enableAutoReconnect;
	}

	/**
	 * Set the Cache's eviction heap percentage attribute.
	 *
	 * @param evictionHeapPercentage float-point value indicating the Cache's heap use percentage to trigger eviction.
	 */
	public void setEvictionHeapPercentage(Float evictionHeapPercentage) {
		this.evictionHeapPercentage = evictionHeapPercentage;
	}

	/**
	 * Requires GemFire 7.0 or higher
	 * @param gatewayConflictResolver defined as Object in the signature for backward
	 * compatibility with Gemfire 6 compatibility. This must be an instance of
	 * {@link com.gemstone.gemfire.cache.util.GatewayConflictResolver}
	 */
	public void setGatewayConflictResolver(Object gatewayConflictResolver) {
		this.gatewayConflictResolver = gatewayConflictResolver;
	}

	/**
	 * @param jndiDataSources the list of configured JndiDataSources to use with this Cache.
	 */
	public void setJndiDataSources(List<JndiDataSource> jndiDataSources) {
		this.jndiDataSources = jndiDataSources;
	}

	/**
	 * Sets the number of seconds for implicit and explicit object lock leases to timeout.
	 *
	 * @param lockLease an integer value indicating the object lock lease timeout.
	 */
	public void setLockLease(Integer lockLease) {
		this.lockLease = lockLease;
	}

	/**
	 * Sets the number of seconds in which the implicit object lock request will timeout.
	 *
	 * @param lockTimeout an integer value specifying the object lock request timeout.
	 */
	public void setLockTimeout(Integer lockTimeout) {
		this.lockTimeout = lockTimeout;
	}

	/**
	 * Set for client subscription queue synchronization when this member acts as a server to clients
	 * and server redundancy is used. Sets the frequency (in seconds) at which the primary server sends messages
	 * to its secondary servers to remove queued events that have already been processed by the clients.
	 *
	 * @param messageSyncInterval an integer value specifying the number of seconds in which the primary server
	 * sends messages to secondary servers.
	 */
	public void setMessageSyncInterval(Integer messageSyncInterval) {
		this.messageSyncInterval = messageSyncInterval;
	}

	/**
	 * Sets the {@link PdxSerializable} for this cache. Applicable on GemFire
	 * 6.6 or higher. The argument is of type object for compatibility with
	 * GemFire 6.5.
	 *
	 * @param serializer pdx serializer configured for this cache.
	 */
	public void setPdxSerializer(Object serializer) {
		this.pdxSerializer = serializer;
	}

	/**
	 * Sets the object preference to PdxInstance. Applicable on GemFire 6.6 or higher.
	 *
	 * @param pdxReadSerialized a boolean value indicating the PDX instance should be returned from Region.get(key)
	 * when available.
	 */
	public void setPdxReadSerialized(Boolean pdxReadSerialized) {
		this.pdxReadSerialized = pdxReadSerialized;
	}

	/**
	 * Controls whether type metadata for PDX objects is persisted to disk. Applicable on GemFire 6.6 or higher.
	 *
	 * @param pdxPersistent a boolean value indicating that PDX type meta-data should be persisted to disk.
	 */
	public void setPdxPersistent(Boolean pdxPersistent) {
		this.pdxPersistent = pdxPersistent;
	}

	/**
	 * Controls whether pdx ignores fields that were unread during
	 * deserialization. Applicable on GemFire 6.6 or higher.
	 *
	 * @param pdxIgnoreUnreadFields the pdxIgnoreUnreadFields to set
	 */
	public void setPdxIgnoreUnreadFields(Boolean pdxIgnoreUnreadFields) {
		this.pdxIgnoreUnreadFields = pdxIgnoreUnreadFields;
	}

	/**
	 * Set the disk store that is used for PDX meta data. Applicable on GemFire
	 * 6.6 or higher.
	 *
	 * @param pdxDiskStoreName the pdxDiskStoreName to set
	 */
	public void setPdxDiskStoreName(String pdxDiskStoreName) {
		this.pdxDiskStoreName = pdxDiskStoreName;
	}

	/**
	 * Set the number of seconds a netSearch operation can wait for data before timing out.
	 *
	 * @param searchTimeout an integer value indicating the netSearch timeout value.
	 */
	public void setSearchTimeout(Integer searchTimeout) {
		this.searchTimeout = searchTimeout;
	}

	/**
	 * Sets the list of TransactionListeners used to configure the Cache to receive transaction events after
	 * the transaction is processed (committed, rolled back).
	 *
	 * @param transactionListeners the list of GemFire TransactionListeners listening for transaction events.
	 * @see com.gemstone.gemfire.cache.TransactionListener
	 */
	public void setTransactionListeners(List<TransactionListener> transactionListeners) {
		this.transactionListeners = transactionListeners;
	}

	/**
	 * Sets the TransactionWriter used to configure the Cache for handling transaction events, such as to veto
	 * the transaction or update an external DB before the commit.
	 *
	 * @param transactionWriter the GemFire TransactionWriter callback receiving transaction events.
	 * @see com.gemstone.gemfire.cache.TransactionWriter
	 */
	public void setTransactionWriter(TransactionWriter transactionWriter) {
		this.transactionWriter = transactionWriter;
	}

	/**
	 * Sets the state of the use-shared-configuration GemFire distribution config setting.
	 *
	 * @param useSharedConfiguration a boolean value to set the use-shared-configuration GemFire distribution property.
	 */
	public void setUseClusterConfiguration(Boolean useSharedConfiguration) {
		this.useClusterConfiguration = useSharedConfiguration;
	}

	/**
	 * Gets a reference to the JRE ClassLoader used to load and create bean classes in the Spring container.
	 *
	 * @return the JRE ClassLoader used to load and created beans in the Spring container.
	 * @see java.lang.ClassLoader
	 */
	public ClassLoader getBeanClassLoader() {
		return beanClassLoader;
	}

	/**
	 * Gets a reference to the Spring BeanFactory that created this GemFire Cache FactoryBean.
	 *
	 * @return a reference to the Spring BeanFactory.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	public BeanFactory getBeanFactory() {
		return beanFactory;
	}

	/* (non-Javadoc) */
	public GemfireBeanFactoryLocator getBeanFactoryLocator() {
		return beanFactoryLocator;
	}

	/**
	 * Gets the Spring bean name for the GemFire Cache.
	 *
	 * @return a String value indicating the Spring container bean name for the GemFire Cache object/component.
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * Gets a reference to the GemFire native cache.xml file as a Spring Resource.
	 *
	 * @return the a reference to the GemFire native cache.xml as a Spring Resource.
	 * @see org.springframework.core.io.Resource
	 */
	public Resource getCacheXml() {
		return cacheXml;
	}

	/* (non-Javadoc) */
	private File getCacheXmlFile() {
		try {
			return getCacheXml().getFile();
		}
		catch (IOException e) {
			throw new IllegalStateException(String.format("Resource (%1$s) is not resolvable as a file", e));
		}
	}

	/* (non-Javadoc) */
	private boolean isCacheXmlAvailable() {
		try {
			Resource localCacheXml = getCacheXml();
			return (localCacheXml != null && localCacheXml.getFile().isFile());
		}
		catch (IOException ignore) {
			return false;
		}
	}

	/**
	 * Gets a reference to the GemFire System Properties.
	 *
	 * @return a reference to the GemFire System Properties.
	 * @see java.util.Properties
	 */
	public Properties getProperties() {
		return (properties != null ? properties : (properties = new Properties()));
	}

	@Override
	public Cache getObject() throws Exception {
		return init();
	}

	@Override
	public Class<? extends GemFireCache> getObjectType() {
		return (cache != null ? cache.getClass() : Cache.class);
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	/**
	 * @return the copyOnRead
	 */
	public Boolean getCopyOnRead() {
		return copyOnRead;
	}

	/**
	 * @return the criticalHeapPercentage
	 */
	public Float getCriticalHeapPercentage() {
		return criticalHeapPercentage;
	}

	/**
	 * @return the dynamicRegionSupport
	 */
	public DynamicRegionSupport getDynamicRegionSupport() {
		return dynamicRegionSupport;
	}

	/**
	 * Gets the value for the auto-reconnect setting.
	 *
	 * @return a boolean value indicating whether auto-reconnect was specified (non-null) and whether it was enabled
	 * or not.
	 */
	public Boolean getEnableAutoReconnect() {
		return enableAutoReconnect;
	}

	/**
	 * @return the evictionHeapPercentage
	 */
	public Float getEvictionHeapPercentage() {
		return evictionHeapPercentage;
	}

	/**
	 * @return the gatewayConflictResolver
	 */
	public Object getGatewayConflictResolver() {
		return gatewayConflictResolver;
	}

	/**
	 * @return the list of configured JndiDataSources.
	 */
	public List<JndiDataSource> getJndiDataSources() {
		return jndiDataSources;
	}

	/**
	 * @return the pdxSerializer
	 */
	public Object getPdxSerializer() {
		return pdxSerializer;
	}

	/**
	 * @return the pdxReadSerialized
	 */
	public Boolean getPdxReadSerialized() {
		return pdxReadSerialized;
	}

	/**
	 * @return the pdxPersistent
	 */
	public Boolean getPdxPersistent() {
		return pdxPersistent;
	}

	/**
	 * @return the pdxIgnoreUnreadFields
	 */
	public Boolean getPdxIgnoreUnreadFields() {
		return pdxIgnoreUnreadFields;
	}

	/**
	 * @return the pdxDiskStoreName
	 */
	public String getPdxDiskStoreName() {
		return pdxDiskStoreName;
	}

	/**
	 * @return the lockLease
	 */
	public Integer getLockLease() {
		return lockLease;
	}

	/**
	 * @return the lockTimeout
	 */
	public Integer getLockTimeout() {
		return lockTimeout;
	}

	/**
	 * @return the messageSyncInterval
	 */
	public Integer getMessageSyncInterval() {
		return messageSyncInterval;
	}

	/**
	 * @return the searchTimeout
	 */
	public Integer getSearchTimeout() {
		return searchTimeout;
	}

	/**
	 * @return the transactionListeners
	 */
	public List<TransactionListener> getTransactionListeners() {
		return transactionListeners;
	}

	/**
	 * @return the transactionWriter
	 */
	public TransactionWriter getTransactionWriter() {
		return transactionWriter;
	}

	/**
	 * Gets the value fo the use-shared-configuration GemFire setting.
	 *
	 * @return a boolean value indicating whether shared configuration use has been enabled or not.
	 */
	public Boolean getUseClusterConfiguration() {
		return this.useClusterConfiguration;
	}

	/**
	 * Determines whether this Cache instance will be lazily initialized.
	 *
	 * @return a boolean value indicating whether this Cache instance will be lazily initialized.
	 */
	public boolean isLazyInitialize() {
		return lazyInitialize;
	}

}
