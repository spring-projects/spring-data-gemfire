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
import java.util.Arrays;
import java.util.Collections;
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
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.CollectionUtils;

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
 */
public class CacheFactoryBean implements BeanClassLoaderAware, BeanFactoryAware, BeanNameAware, FactoryBean<Cache>,
		InitializingBean, DisposableBean, PersistenceExceptionTranslator {

	protected static final List<String> VALID_JNDI_DATASOURCE_TYPE_NAMES = Collections.unmodifiableList(
		Arrays.asList("ManagedDataSource", "PooledDataSource", "SimpleDataSource", "XAPooledDataSource"));

	protected boolean close = true;
	protected boolean lazyInitialize = true;
	protected boolean useBeanFactoryLocator = true;

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

	protected GemfireBeanFactoryLocator factoryLocator;

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

	/**
	 * Inner class to avoid a hard dependency on the GemFire 6.6 API.
	 *
	 * @author Costin Leau
	 */
	private class PdxOptions implements Runnable {

		private final CacheFactory factory;

		PdxOptions(CacheFactory factory) {
			this.factory = factory;
		}

		@Override
		public void run() {
			if (pdxSerializer != null) {
				Assert.isAssignable(PdxSerializer.class, pdxSerializer.getClass(), "Invalid pdx serializer used");
				factory.setPdxSerializer((PdxSerializer) pdxSerializer);
			}
			if (pdxDiskStoreName != null) {
				factory.setPdxDiskStore(pdxDiskStoreName);
			}
			if (pdxIgnoreUnreadFields != null) {
				factory.setPdxIgnoreUnreadFields(pdxIgnoreUnreadFields);
			}
			if (pdxPersistent != null) {
				factory.setPdxPersistent(pdxPersistent);
			}
			if (pdxReadSerialized != null) {
				factory.setPdxReadSerialized(pdxReadSerialized);
			}
		}
	}

	private void init() throws Exception {
		if (useBeanFactoryLocator && factoryLocator == null) {
			factoryLocator = new GemfireBeanFactoryLocator();
			factoryLocator.setBeanFactory(beanFactory);
			factoryLocator.setBeanName(beanName);
			factoryLocator.afterPropertiesSet();
		}

		final ClassLoader originalThreadContextClassLoader = Thread.currentThread().getContextClassLoader();

		try {
			String messagePrefix;

			// use bean ClassLoader to load GemFire Declarable classes
			Thread.currentThread().setContextClassLoader(beanClassLoader);

			try {
				cache = (Cache) fetchCache();
				messagePrefix = "Retrieved existing";
			}
			catch (CacheClosedException ex) {
				initializeDynamicRegionFactory();

				Object factory = createFactory(getProperties());

				if (isPdxSettingsSpecified()) {
					Assert.isTrue(ClassUtils.isPresent("com.gemstone.gemfire.pdx.PdxSerializer", beanClassLoader),
						"Cannot set PDX options since GemFire 6.6 not detected.");
					applyPdxOptions(factory);
				}

				cache = (Cache) createCache(factory);
				messagePrefix = "Created new";
			}

			if (this.copyOnRead != null) {
				cache.setCopyOnRead(this.copyOnRead);
			}
			if (gatewayConflictResolver != null) {
				cache.setGatewayConflictResolver((GatewayConflictResolver) gatewayConflictResolver);
			}
			if (lockLease != null) {
				cache.setLockLease(lockLease);
			}
			if (lockTimeout != null) {
				cache.setLockTimeout(lockTimeout);
			}
			if (messageSyncInterval != null) {
				cache.setMessageSyncInterval(messageSyncInterval);
			}
			if (searchTimeout != null) {
				cache.setSearchTimeout(searchTimeout);
			}

			DistributedSystem system = cache.getDistributedSystem();

			DistributedMember member = system.getDistributedMember();

			log.info(String.format("Connected to Distributed System [%1$s] as Member [%2$s] in Groups [%3$s] with Roles [%4$s] on Host [%5$s] having PID [%6$d].",
				system.getName(), member.getId(), member.getGroups(), member.getRoles(), member.getHost(), member.getProcessId()));

			log.info(String.format("%1$s GemFire v.%2$s Cache [%3$s].", messagePrefix, CacheFactory.getVersion(),
				cache.getName()));

			// load/init cache.xml
			if (cacheXml != null) {
				cache.loadCacheXml(cacheXml.getInputStream());

				if (log.isDebugEnabled()) {
					log.debug(String.format("Initialized Cache from '%1$s'.", cacheXml));
				}
			}

			setHeapPercentages();
			registerTransactionListeners();
			registerTransactionWriter();
			registerJndiDataSources();
		}
		finally {
			Thread.currentThread().setContextClassLoader(originalThreadContextClassLoader);
		}
	}

	private boolean isPdxSettingsSpecified() {
		return (pdxSerializer != null || pdxPersistent != null || pdxReadSerialized != null
			|| pdxIgnoreUnreadFields != null || pdxDiskStoreName != null);
	}

	/**
	 * Sets the PDX properties for the given object. Note this is implementation
	 * specific as it depends on the type of the factory passed in.
	 *
	 * @param factory the GemFire CacheFactory used to apply the PDX configuration settings.
	 */
	protected void applyPdxOptions(Object factory) {
		if (factory instanceof CacheFactory) {
			new PdxOptions((CacheFactory) factory).run();
		}
	}

	/**
	 * If dynamic regions are enabled, create a DynamicRegionFactory before
	 * creating the cache
	 */
	private void initializeDynamicRegionFactory() {
		if (dynamicRegionSupport != null) {
			dynamicRegionSupport.initializeDynamicRegionFactory();
		}
	}

	/**
	 * Register all declared transaction listeners
	 */
	protected void registerTransactionListeners() {
		if (!CollectionUtils.isEmpty(transactionListeners)) {
			for (TransactionListener transactionListener : transactionListeners) {
				cache.getCacheTransactionManager().addListener(transactionListener);
			}
		}
	}

	/**
	 * Register a transaction writer if declared
	 */
	protected void registerTransactionWriter() {
		if (transactionWriter != null) {
			cache.getCacheTransactionManager().setWriter(transactionWriter);
		}
	}

	private void registerJndiDataSources() {
		if (jndiDataSources != null) {
			for (JndiDataSource jndiDataSource : jndiDataSources) {
				validate(jndiDataSource);
				JNDIInvoker.mapDatasource(jndiDataSource.getAttributes(), jndiDataSource.getProps());
			}
		}
	}

	private void validate(final JndiDataSource jndiDataSource) {
		Map<String, String> attributes = jndiDataSource.getAttributes();
		String typeAttributeValue = attributes.get("type");
		Assert.isTrue(VALID_JNDI_DATASOURCE_TYPE_NAMES.contains(typeAttributeValue),
			String.format("The 'jndi-binding', 'type' [%1$s] is invalid; the 'type' must be one of %2$s.",
				typeAttributeValue, VALID_JNDI_DATASOURCE_TYPE_NAMES));
	}

	private void setHeapPercentages() {
		if (criticalHeapPercentage != null) {
			Assert.isTrue(criticalHeapPercentage > 0.0 && criticalHeapPercentage <= 100.0,
				String.format("Invalid value specified for 'criticalHeapPercentage' (%1$s). Must be > 0.0 and <= 100.0.",
					criticalHeapPercentage));
			cache.getResourceManager().setCriticalHeapPercentage(criticalHeapPercentage);
		}

		if (evictionHeapPercentage != null) {
			Assert.isTrue(evictionHeapPercentage > 0.0 && evictionHeapPercentage <= 100.0,
				String.format("Invalid value specified for 'evictionHeapPercentage' (%1$s). Must be > 0.0 and <= 100.0.",
					evictionHeapPercentage));
			cache.getResourceManager().setEvictionHeapPercentage(evictionHeapPercentage);
		}
	}

	protected GemFireCache createCache(Object factory) {
		return (cache != null ? cache : ((CacheFactory) factory).create());
	}

	protected Object createFactory(Properties gemfireProperties) {
		return new CacheFactory(gemfireProperties);
	}

	protected GemFireCache fetchCache() {
		return (cache != null ? cache : CacheFactory.getAnyInstance());
	}

	@Override
	public void destroy() throws Exception {
		if (close) {
			if (cache != null && !cache.isClosed()) {
				cache.close();
			}

			cache = null;

			if (factoryLocator != null) {
				factoryLocator.destroy();
				factoryLocator = null;
			}
		}
	}

	@Override
	public DataAccessException translateExceptionIfPossible(final RuntimeException e) {
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
	 * Sets the cache configuration.
	 *
	 * @param cacheXml the cacheXml to set
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
	 * Controls whether auto-reconnect functionality introduced in GemFire 8 is enabled or not.
	 *
	 * @param enableAutoReconnect a boolean value to enable/disable auto-reconnect functionality.
	 * @since GemFire 8.0
	 */
	public void setEnableAutoReconnect(Boolean enableAutoReconnect) {
		this.enableAutoReconnect = enableAutoReconnect;
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
	 * Set the Cache's eviction heap percentage attribute.
	 *
	 * @param evictionHeapPercentage float-point value indicating the Cache's heap use percentage to trigger eviction.
	 */
	public void setEvictionHeapPercentage(Float evictionHeapPercentage) {
		this.evictionHeapPercentage = evictionHeapPercentage;
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
	 * Sets an instance of the DynamicRegionSupport to support Dynamic Regions in this GemFire Cache.
	 *
	 * @param dynamicRegionSupport the DynamicRegionSupport class to setup Dynamic Regions in this Cache.
	 */
	public void setDynamicRegionSupport(DynamicRegionSupport dynamicRegionSupport) {
		this.dynamicRegionSupport = dynamicRegionSupport;
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
	 * Sets the state of the use-shared-configuration GemFire distribution config setting.
	 *
	 * @param useSharedConfiguration a boolean value to set the use-shared-configuration GemFire distribution property.
	 */
	public void setUseClusterConfiguration(Boolean useSharedConfiguration) {
		this.useClusterConfiguration = useSharedConfiguration;
	}

	/**
	 * @return the beanClassLoader
	 */
	public ClassLoader getBeanClassLoader() {
		return beanClassLoader;
	}

	/**
	 * @return the beanFactory
	 */
	public BeanFactory getBeanFactory() {
		return beanFactory;
	}

	/**
	 * @return the beanFactoryLocator
	 */
	public GemfireBeanFactoryLocator getBeanFactoryLocator() {
		return factoryLocator;
	}

	/**
	 * @return the beanName
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * @return the cacheXml
	 */
	public Resource getCacheXml() {
		return cacheXml;
	}

	/**
	 * @return the properties
	 */
	public Properties getProperties() {
		if (properties == null) {
			properties = new Properties();
		}

		return properties;
	}

	@Override
	public Cache getObject() throws Exception {
		init();
		return cache;
	}

	@Override
	public Class<? extends Cache> getObjectType() {
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
	 * @return the dynamicRegionSupport
	 */
	public DynamicRegionSupport getDynamicRegionSupport() {
		return dynamicRegionSupport;
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

	protected void postProcessPropertiesBeforeInitialization(Properties gemfireProperties) {
		gemfireProperties.setProperty("disable-auto-reconnect", String.valueOf(
			!Boolean.TRUE.equals(getEnableAutoReconnect())));
		gemfireProperties.setProperty("use-cluster-configuration", String.valueOf(
			Boolean.TRUE.equals(getUseClusterConfiguration())));
	}

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.InitializingBean#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		postProcessPropertiesBeforeInitialization(getProperties());

		if (!isLazyInitialize()) {
			init();
		}
	}

}
