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
import com.gemstone.gemfire.cache.CacheTransactionManager;
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
 * Factory used for configuring a Gemfire Cache manager. Allows either retrieval
 * of an existing, opened cache or the creation of a new one.
 * 
 * <p>
 * This class implements the
 * {@link org.springframework.dao.support.PersistenceExceptionTranslator}
 * interface, as auto-detected by Spring's
 * {@link org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor}
 * , for AOP-based translation of native exceptions to Spring
 * DataAccessExceptions. Hence, the presence of this class automatically enables
 * a PersistenceExceptionTranslationPostProcessor to translate GemFire
 * exceptions.
 * 
 * @author Costin Leau
 * @author David Turanski
 */
public class CacheFactoryBean implements BeanNameAware, BeanFactoryAware, BeanClassLoaderAware, DisposableBean,
		FactoryBean<Cache>, PersistenceExceptionTranslator {
	
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

	public static class DynamicRegionSupport {
		private String diskDir;

		private String poolName;

		private Boolean persistent = Boolean.TRUE;

		private Boolean registerInterest = Boolean.TRUE;

		public String getDiskDir() {
			return diskDir;
		}

		public void setDiskDir(String diskDir) {
			this.diskDir = diskDir;
		}

		public Boolean getPersistent() {
			return persistent;
		}

		public void setPersistent(Boolean persistent) {
			this.persistent = persistent;
		}

		public Boolean getRegisterInterest() {
			return registerInterest;
		}

		public void setRegisterInterest(Boolean registerInterest) {
			this.registerInterest = registerInterest;
		}

		public String getPoolName() {
			return poolName;
		}

		public void setPoolName(String poolName) {
			this.poolName = poolName;
		}

		public void initializeDynamicRegionFactory() {
			DynamicRegionFactory.Config config = null;
			if (diskDir == null) {
				config = new DynamicRegionFactory.Config(null, poolName, persistent, registerInterest);
			} else {
				config = new DynamicRegionFactory.Config(new File(diskDir), poolName, persistent, registerInterest);
			}
			DynamicRegionFactory.get().open(config);
		}
	}

	public static class JndiDataSource {
		private Map<String, String> attributes;

		private List<ConfigProperty> props;

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

	protected final Log log = LogFactory.getLog(getClass());

	protected Cache cache;

	protected Resource cacheXml;

	protected Properties properties;

	protected ClassLoader beanClassLoader;

	protected GemfireBeanFactoryLocator factoryLocator;

	protected BeanFactory beanFactory;

	protected String beanName;

	protected boolean useBeanFactoryLocator = true;
	
	protected boolean close = true;

	// PDX options
	protected Object pdxSerializer;

	protected Boolean pdxPersistent;

	protected Boolean pdxReadSerialized;

	protected Boolean pdxIgnoreUnreadFields;

	protected String pdxDiskStoreName;

	protected Boolean copyOnRead;

	protected Integer lockTimeout;

	protected Integer lockLease;

	protected Integer messageSyncInterval;

	protected Integer searchTimeout;

	protected List<TransactionListener> transactionListeners;

	protected TransactionWriter transactionWriter;

	protected Float evictionHeapPercentage;

	protected Float criticalHeapPercentage;

	protected DynamicRegionSupport dynamicRegionSupport;

	protected List<JndiDataSource> jndiDataSources;

	// Defined this way for backward compatibility
	protected Object gatewayConflictResolver;

	private void init() throws Exception {
		// initialize locator
		if (useBeanFactoryLocator) {
			factoryLocator = new GemfireBeanFactoryLocator();
			factoryLocator.setBeanFactory(beanFactory);
			factoryLocator.setBeanName(beanName);
			factoryLocator.afterPropertiesSet();
		}
		// use the bean class loader to load Declarable classes
		Thread th = Thread.currentThread();
		ClassLoader oldTCCL = th.getContextClassLoader();

		try {
			th.setContextClassLoader(beanClassLoader);
			// first look for open caches
			String msg = null;
			try {
				cache =(Cache)fetchCache();
				msg = "Retrieved existing";
			} catch (CacheClosedException ex) {

				initializeDynamicRegionFactory();

				Object factory = createFactory(this.properties);

				// GemFire 6.6 specific options
				if (pdxSerializer != null || pdxPersistent != null || pdxReadSerialized != null
						|| pdxIgnoreUnreadFields != null || pdxDiskStoreName != null) {
					Assert.isTrue(ClassUtils.isPresent("com.gemstone.gemfire.pdx.PdxSerializer", beanClassLoader),
							"Cannot set PDX options since GemFire 6.6 not detected");
					applyPdxOptions(factory);
				}

				// fall back to cache creation
				cache = (Cache)createCache(factory);
				msg = "Created";
			}
			if (this.copyOnRead != null) {
				cache.setCopyOnRead(this.copyOnRead);
			}
			
			if (lockLease != null) {
				cache.setLockLease(lockLease);
			}
			if (lockTimeout != null) {
				cache.setLockTimeout(lockTimeout);
			}
			if (searchTimeout != null) {
				cache.setSearchTimeout(searchTimeout);
			}
			if (messageSyncInterval != null) {
				cache.setMessageSyncInterval(messageSyncInterval);
			}

			if (gatewayConflictResolver != null) {
				cache.setGatewayConflictResolver((GatewayConflictResolver) gatewayConflictResolver);
			}

			DistributedSystem system = cache.getDistributedSystem();
			
			DistributedMember member = system.getDistributedMember();
			log.info("Connected to Distributed System [" + system.getName() + "=" + member.getId() + "@"
					+ member.getHost() + "]");

			log.info(msg + " GemFire v." + CacheFactory.getVersion() + " Cache [" + cache.getName() + "]");

			// load/init cache.xml
			if (cacheXml != null) {
				cache.loadCacheXml(cacheXml.getInputStream());

				if (log.isDebugEnabled()) {
					log.debug("Initialized cache from " + cacheXml);
				}
			}

			setHeapPercentages();
			registerTransactionListeners();
			registerTransactionWriter();
			registerJndiDataSources();
		} finally {
			th.setContextClassLoader(oldTCCL);
		}
	}

	private void registerJndiDataSources() {
		if (jndiDataSources != null) {
			for (JndiDataSource jndiDataSource : jndiDataSources) {
				JNDIInvoker.mapDatasource(jndiDataSource.getAttributes(), jndiDataSource.getProps());
			}
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

	private void setHeapPercentages() {
		if (criticalHeapPercentage != null) {
			Assert.isTrue(criticalHeapPercentage > 0.0 && criticalHeapPercentage <= 100.0,
					"invalid value specified for criticalHeapPercentage :" + criticalHeapPercentage
							+ ". Must be > 0.0 and <= 100.0");
			cache.getResourceManager().setCriticalHeapPercentage(criticalHeapPercentage);

		}

		if (evictionHeapPercentage != null) {
			Assert.isTrue(evictionHeapPercentage > 0.0 && evictionHeapPercentage <= 100.0,
					"invalid value specified for evictionHeapPercentage :" + evictionHeapPercentage
							+ ". Must be > 0.0 and <= 100.0");
			cache.getResourceManager().setEvictionHeapPercentage(evictionHeapPercentage);
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

	/**
	 * Register all declared transaction listeners
	 */
	protected void registerTransactionListeners() {
		if (!CollectionUtils.isEmpty(transactionListeners)) {
			CacheTransactionManager txManager = cache.getCacheTransactionManager();
			for (TransactionListener transactionListener : transactionListeners) {
				txManager.addListener(transactionListener);
			}
		}
	}

	/**
	 * Sets the PDX properties for the given object. Note this is implementation
	 * specific as it depends on the type of the factory passed in.
	 * 
	 * @param factory
	 */
	protected void applyPdxOptions(Object factory) {
		if (factory instanceof CacheFactory) {
			new PdxOptions((CacheFactory) factory).run();
		}
	}
	
	protected Object createFactory(Properties props) {
		return new CacheFactory(props);
	}

	protected GemFireCache fetchCache() {
		return (cache == null)? CacheFactory.getAnyInstance(): cache;
	}

	protected GemFireCache createCache(Object factory) {
		return (cache == null)? ((CacheFactory) factory).create() : cache;
	}

	@Override
	public void destroy() throws Exception {
		if (!close) {
			return;
		}
		if (cache != null && !cache.isClosed()) {
			cache.close();
		}

		cache = null;

		if (factoryLocator != null) {
			factoryLocator.destroy();
			factoryLocator = null;
		}
	}

	@Override
	public DataAccessException translateExceptionIfPossible(RuntimeException ex) {
		if (ex instanceof GemFireException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireException) ex);
		}
		if (ex instanceof IllegalArgumentException) {
			DataAccessException wrapped = GemfireCacheUtils.convertQueryExceptions(ex);
			// ignore conversion if the generic exception is returned
			if (!(wrapped instanceof GemfireSystemException)) {
				return wrapped;
			}
		}
		if (ex.getCause() instanceof GemFireException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireException) ex.getCause());
		}
		if (ex.getCause() instanceof GemFireCheckedException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireCheckedException) ex.getCause());
		}

		return null;
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
	 * Sets the cache properties.
	 * 
	 * @param properties the properties to set
	 */
	public void setProperties(Properties properties) {
		this.properties = properties;
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
	 * Sets the object preference to PdxInstance type. Applicable on GemFire 6.6
	 * or higher.
	 * 
	 * @param pdxPersistent the pdxPersistent to set
	 */
	public void setPdxPersistent(Boolean pdxPersistent) {
		this.pdxPersistent = pdxPersistent;
	}

	/**
	 * Controls whether the type metadata for PDX objects is persisted to disk.
	 * Applicable on GemFire 6.6 or higher.
	 * 
	 * @param pdxReadSerialized the pdxReadSerialized to set
	 */
	public void setPdxReadSerialized(Boolean pdxReadSerialized) {
		this.pdxReadSerialized = pdxReadSerialized;
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
	 * @return the beanFactory
	 */
	public BeanFactory getBeanFactory() {
		return beanFactory;
	}

	/**
	 * 
	 * @param copyOnRead
	 */
	public void setCopyOnRead(Boolean copyOnRead) {
		this.copyOnRead = copyOnRead;
	}

	/**
	 * 
	 * @param lockTimeout
	 */
	public void setLockTimeout(Integer lockTimeout) {
		this.lockTimeout = lockTimeout;
	}

	/**
	 * 
	 * @param lockLease
	 */
	public void setLockLease(Integer lockLease) {
		this.lockLease = lockLease;
	}

	/**
	 * 
	 * @param messageSyncInterval
	 */
	public void setMessageSyncInterval(Integer messageSyncInterval) {
		this.messageSyncInterval = messageSyncInterval;
	}

	/**
	 * 
	 * @param searchTimeout
	 */
	public void setSearchTimeout(Integer searchTimeout) {
		this.searchTimeout = searchTimeout;
	}

	/**
	 * 
	 * @param evictionHeapPercentage
	 */
	public void setEvictionHeapPercentage(Float evictionHeapPercentage) {
		this.evictionHeapPercentage = evictionHeapPercentage;
	}

	/**
	 * 
	 * @param criticalHeapPercentage
	 */
	public void setCriticalHeapPercentage(Float criticalHeapPercentage) {
		this.criticalHeapPercentage = criticalHeapPercentage;
	}
	
	/**
	 * 
	 * @param close set to false if destroy() should not close the cache
	 */
	public void setClose(boolean close) {
		this.close = close;
	}

	/**
	 * 
	 * @param transactionListeners
	 */
	public void setTransactionListeners(List<TransactionListener> transactionListeners) {
		this.transactionListeners = transactionListeners;
	}

	/**
	 * 
	 * @param transactionWriter
	 */
	public void setTransactionWriter(TransactionWriter transactionWriter) {
		this.transactionWriter = transactionWriter;
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
	 * 
	 * @param dynamicRegionSupport
	 */
	public void setDynamicRegionSupport(DynamicRegionSupport dynamicRegionSupport) {
		this.dynamicRegionSupport = dynamicRegionSupport;
	}

	/**
	 * 
	 * @param jndiDataSources
	 */
	public void setJndiDataSources(List<JndiDataSource> jndiDataSources) {
		this.jndiDataSources = jndiDataSources;
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
		return properties;
	}

	/**
	 * @return the beanClassLoader
	 */
	public ClassLoader getBeanClassLoader() {
		return beanClassLoader;
	}

	/**
	 * @return the beanName
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * @return the pdxSerializer
	 */
	public Object getPdxSerializer() {
		return pdxSerializer;
	}

	/**
	 * @return the pdxPersistent
	 */
	public Boolean getPdxPersistent() {
		return pdxPersistent;
	}

	/**
	 * @return the pdxReadSerialized
	 */
	public Boolean getPdxReadSerialized() {
		return pdxReadSerialized;
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
	 * @return the copyOnRead
	 */
	public Boolean getCopyOnRead() {
		return copyOnRead;
	}

	/**
	 * @return the lockTimeout
	 */
	public Integer getLockTimeout() {
		return lockTimeout;
	}

	/**
	 * @return the lockLease
	 */
	public Integer getLockLease() {
		return lockLease;
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
	 * @return the evictionHeapPercentage
	 */
	public Float getEvictionHeapPercentage() {
		return evictionHeapPercentage;
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
	 * @return the jndiDataSources
	 */
	public List<JndiDataSource> getJndiDataSources() {
		return jndiDataSources;
	}

	/**
	 * @return the gatewayConflictResolver
	 */
	public Object getGatewayConflictResolver() {
		return gatewayConflictResolver;
	}
	public GemfireBeanFactoryLocator getBeanFactoryLocator() {
		return factoryLocator;
	}
}
